pub(crate) mod diagnostics;
pub(crate) mod expr;
pub(crate) mod generic;
pub(crate) mod path;
pub(crate) mod pattern;
pub(crate) mod stmt;

pub mod reify;

use error_snippet::Result;
use indexmap::IndexMap;
use lume_span::{Internable, Location, NodeId};
use lume_tir::{TypedIR, VariableId, VariableSource};
pub use lume_type_metadata::StaticMetadata;
use lume_typech::TyCheckCtx;
use lume_typech::query::Callable;

pub struct Lower<'tcx> {
    /// Defines the type context which will be lowering into `TypedIR`.
    tcx: &'tcx TyCheckCtx,

    /// Defines the `TypedIR` being built.
    ir: TypedIR,

    /// Saved variable mappings between function-declaration and -definition.
    ///
    /// See [`Lower::lower_block`] for more information.
    mappings: IndexMap<NodeId, Variables>,
}

impl<'tcx> Lower<'tcx> {
    pub fn new(tcx: &'tcx TyCheckCtx) -> Self {
        Self {
            tcx,
            ir: TypedIR::default(),
            mappings: IndexMap::new(),
        }
    }

    /// Lowers the defined type context into a `TypedIR` map, which can then be
    /// further lowering into MIR.
    ///
    /// # Errors
    ///
    /// Returns `Err` if a definition within the context is invalid or if the
    /// type context returned an error.
    #[libftrace::traced(level = Debug, err)]
    pub fn lower(mut self) -> Result<TypedIR> {
        self.define_callables()?;
        self.lower_callables()?;

        let mut reification_pass = reify::ReificationPass::new(self.tcx);

        for ty in self.tcx.db().types() {
            let type_ref = lume_types::TypeRef::new(ty.id, ty.name.location);
            reification_pass.build_type_metadata_of(&type_ref)?;
        }

        for function in self.ir.functions.values_mut() {
            reification_pass.execute(function)?;
        }

        self.ir.metadata = reification_pass.static_metadata;

        Ok(self.ir)
    }

    /// Lowers the given type context into a `TypedIR` map, which can then be
    /// further lowering into MIR.
    ///
    /// # Errors
    ///
    /// Returns `Err` if a definition within the context is invalid or if the
    /// type context returned an error.
    pub fn build(tcx: &TyCheckCtx) -> Result<TypedIR> {
        let lower = Lower::new(tcx);
        lower.lower()
    }

    #[libftrace::traced(level = Debug, err)]
    fn define_callables(&mut self) -> Result<()> {
        for method in self.tcx.tdb().methods() {
            if method.is_intrinsic() {
                continue;
            }

            libftrace::debug!("defining method {:+}", method.name);

            let signature = self.tcx.signature_of(Callable::Method(method))?;
            let location = self.tcx.hir_span_of_node(method.id);
            let kind = self.determine_method_kind(method, self.tcx.hir_body_of_node(method.id).is_some());

            let mut func_lower = LowerFunction::new(self);
            let func = func_lower.define(method.id, &method.name, signature.as_ref(), kind, location)?;

            self.mappings.insert(method.id, func_lower.variables);
            self.ir.functions.insert(func.id, func);
        }

        for func in self.tcx.tdb().functions() {
            libftrace::debug!("defining function {:+}", func.name);

            let signature = self.tcx.signature_of(Callable::Function(func))?;
            let location = self.tcx.hir_span_of_node(func.id);

            let mut func_lower = LowerFunction::new(self);
            let func = func_lower.define(
                func.id,
                &func.name,
                signature.as_ref(),
                lume_tir::FunctionKind::Static,
                location,
            )?;

            self.mappings.insert(func.id, func_lower.variables);
            self.ir.functions.insert(func.id, func);
        }

        Ok(())
    }

    #[libftrace::traced(level = Debug, err)]
    fn lower_callables(&mut self) -> Result<()> {
        for method in self.tcx.tdb().methods() {
            if !self.tcx.hir_is_local_node(method.id) || !self.should_lower_method(method) {
                continue;
            }

            libftrace::debug!("lowering method {:+}", method.name);

            self.lower_block(method.id)?;
        }

        for func in self.tcx.tdb().functions() {
            if !self.tcx.hir_is_local_node(func.id) {
                continue;
            }

            libftrace::debug!("lowering function {:+}", func.name);

            self.lower_block(func.id)?;
        }

        Ok(())
    }

    #[libftrace::traced(level = Debug, fields(id), err)]
    fn lower_block(&mut self, id: NodeId) -> Result<()> {
        // We need to conserve the variable mappings between function-declaration and
        // -definition, since we likely declared some variables in the function
        // declaration (specifically parameters and type parameters).
        //
        // If we skip this, the definition might have duplicate variables.
        let mappings = self.mappings.swap_remove(&id).unwrap_or_default();

        let block = if let Some(body) = self.tcx.hir_body_of_node(id) {
            let mut func_lower = LowerFunction::new(self);
            func_lower.variables = mappings;

            Some(func_lower.lower_block(body)?)
        } else {
            None
        };

        self.ir.functions.get_mut(&id).unwrap().block = block;

        Ok(())
    }

    /// Determines the [`lume_tir::FunctionKind`] of the given method.
    #[libftrace::traced(level = Debug, fields(method = method.name.to_wide_string(), has_body), ret)]
    pub(crate) fn determine_method_kind(&self, method: &lume_types::Method, has_body: bool) -> lume_tir::FunctionKind {
        // Checks whether the method is an implementation of
        // `std::ops::Dispose::dispose()`.
        if self.tcx.is_method_dropper(method.id) {
            return lume_tir::FunctionKind::Dropper;
        }

        // Intrinsic methods are only defined so they can be type-checked against.
        // They do not need to exist within the binary.
        if method.is_intrinsic() {
            return lume_tir::FunctionKind::Dynamic;
        }

        // Trait method definitions without any default implementation have no reason to
        // be in the binary, since they have no body to codegen from.
        if self.is_dynamic_dispatch(method) {
            return lume_tir::FunctionKind::Dynamic;
        }

        // Static trait method definitions cannot be called, if they do not have a
        // default body to be invoked.
        if self.tcx.is_static_method(method.id) && method.is_trait_definition() && !has_body {
            return lume_tir::FunctionKind::Unreachable;
        }

        lume_tir::FunctionKind::Static
    }

    /// Determines whether the given method should be lowered into TIR
    /// or if it should stay as a declaration without body.
    pub(crate) fn should_lower_method(&self, method: &lume_types::Method) -> bool {
        let has_body = self.tcx.hir_body_of_node(method.id).is_some();

        // Intrinsic methods are only defined so they can be type-checked against.
        // They do not need to exist within the binary.
        if method.is_intrinsic() {
            return false;
        }

        // Trait method definitions without any default implementation have no reason to
        // be in the binary, since they have no body to codegen from.
        if self.is_dynamic_dispatch(method) && !has_body {
            return false;
        }

        // Certain kinds of functions should never be lowered, such as static trait
        // methods with default implementations.
        if let Some(declaration) = self.ir.functions.get(&method.id)
            && !declaration.kind.should_be_lowered()
            && !has_body
        {
            return false;
        }

        true
    }

    /// Determines whether the given method is meant to be invoked dynamically
    /// via dynamic dispatch.
    #[inline]
    #[must_use]
    #[libftrace::traced(level = Debug, fields(method = method.name.to_wide_string()), ret)]
    pub(crate) fn is_dynamic_dispatch(&self, method: &lume_types::Method) -> bool {
        libftrace::trace!(
            "kind: {:?}, instanced: {}",
            method.kind,
            self.tcx.is_instanced_method(method.id)
        );

        method.kind == lume_types::MethodKind::TraitDefinition && self.tcx.is_instanced_method(method.id)
    }
}

/// General mappings between nodes and variables.
#[derive(Default, Clone)]
struct Variables {
    /// Maps each variable declaration ID to its corresponding variable ID.
    ///
    /// Note: the ID of the node does not necessarily correspond to the ID of a
    /// `VariableDeclaration` node, since other expressions or nodes may declare
    /// variables.
    pub(crate) mapping: IndexMap<lume_span::NodeId, VariableId>,

    /// Maps each variable ID to it's corresponding source.
    pub(crate) sources: IndexMap<VariableId, VariableSource>,
}

impl Variables {
    /// Adds a mapping between a node ID and a variable ID.
    pub(crate) fn add_mapping(&mut self, node_id: lume_span::NodeId, variable_id: VariableId) {
        self.mapping.insert(node_id, variable_id);
    }

    /// Adds a mapping between a node ID and a variable ID.
    pub(crate) fn mapping_of(&self, node_id: lume_span::NodeId) -> Option<VariableId> {
        self.mapping.get(&node_id).copied()
    }

    /// Allocates a new variable in the function with the given source and
    /// returns it's ID.
    pub(crate) fn mark_variable(&mut self, source: VariableSource) -> VariableId {
        let id = VariableId(self.sources.len());
        self.sources.insert(id, source);

        id
    }
}

struct LowerFunction<'tcx> {
    /// Defines the parent lowering context.
    lower: &'tcx Lower<'tcx>,
    variables: Variables,
}

impl<'tcx> LowerFunction<'tcx> {
    pub fn new(lower: &'tcx Lower<'tcx>) -> Self {
        Self {
            lower,
            variables: Variables::default(),
        }
    }

    fn define(
        &mut self,
        id: NodeId,
        name: &lume_hir::Path,
        signature: lume_types::FunctionSig,
        kind: lume_tir::FunctionKind,
        location: Location,
    ) -> Result<lume_tir::Function> {
        let name = self.path_hir(name, id)?;
        let hir_type_params = self.lower.tcx.available_type_params_at(id);

        let parameters = self.parameters(signature.params);
        let type_params = self.type_parameters(&hir_type_params);
        let return_type = signature.ret_ty.clone();

        Ok(lume_tir::Function {
            id,
            name,
            kind,
            parameters,
            type_params,
            return_type,
            block: None,
            location,
        })
    }

    fn lower_block(&mut self, block: &lume_hir::Block) -> Result<lume_tir::Block> {
        let statements = block
            .statements
            .iter()
            .map(|stmt| self.statement(*stmt))
            .collect::<Result<Vec<_>>>()?;

        let return_type = self.lower.tcx.type_of_block(block)?;

        Ok(lume_tir::Block {
            statements,
            return_type,
        })
    }

    /// Allocates a new variable in the function with the given source and
    /// returns it's ID.
    #[inline]
    fn mark_variable(&mut self, source: VariableSource) -> VariableId {
        self.variables.mark_variable(source)
    }

    fn parameters(&mut self, params: &[lume_types::Parameter]) -> Vec<lume_tir::Parameter> {
        params
            .iter()
            .map(|param| {
                let var = self.mark_variable(VariableSource::Parameter);

                lume_tir::Parameter {
                    index: param.idx,
                    var,
                    name: param.name.intern(),
                    ty: param.ty.clone(),
                    vararg: param.vararg,
                    location: param.location,
                }
            })
            .collect()
    }
}
