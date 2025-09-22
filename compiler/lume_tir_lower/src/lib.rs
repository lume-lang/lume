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
use lume_typech::TyCheckCtx;

pub use lume_type_metadata::StaticMetadata;

pub struct Lower<'tcx> {
    /// Defines the type context which will be lowering into `TypedIR`.
    tcx: &'tcx TyCheckCtx,

    /// Defines the `TypedIR` being built.
    ir: TypedIR,
}

impl<'tcx> Lower<'tcx> {
    pub fn new(tcx: &'tcx TyCheckCtx) -> Self {
        Self {
            tcx,
            ir: TypedIR::default(),
        }
    }

    /// Lowers the defined type context into a `TypedIR` map, which can then be further
    /// lowering into MIR.
    ///
    /// # Errors
    ///
    /// Returns `Err` if a definition within the context is invalid or if the type context
    /// returned an error.
    pub fn lower(mut self) -> Result<TypedIR> {
        self.define_callables()?;
        self.lower_callables()?;

        let mut reification_pass = reify::ReificationPass::new(self.tcx);

        for ty in self.tcx.db().types() {
            let type_ref = lume_types::TypeRef::new(ty.id, Location::empty());
            reification_pass.build_type_metadata_of(&type_ref)?;
        }

        for function in self.ir.functions.values_mut() {
            reification_pass.execute(function)?;
        }

        self.ir.metadata = reification_pass.static_metadata;

        Ok(self.ir)
    }

    /// Lowers the given type context into a `TypedIR` map, which can then be further
    /// lowering into MIR.
    ///
    /// # Errors
    ///
    /// Returns `Err` if a definition within the context is invalid or if the type context
    /// returned an error.
    pub fn build(tcx: &TyCheckCtx) -> Result<TypedIR> {
        let lower = Lower::new(tcx);
        lower.lower()
    }

    fn define_callables(&mut self) -> Result<()> {
        for method in self.tcx.tdb().methods() {
            if method.is_intrinsic() {
                continue;
            }

            tracing::debug!(target: "tir_lower", "defining method {:+}", method.name);

            let location = self.tcx.hir_span_of_node(method.id);
            let kind = self.determine_method_kind(method, self.tcx.hir_body_of_node(method.id).is_some());

            let mut func_lower = LowerFunction::new(self);
            let func = func_lower.define(method.id, &method.name, method.sig(), kind, location)?;

            self.ir.functions.insert(func.id, func);
        }

        for func in self.tcx.tdb().functions() {
            tracing::debug!(target: "tir_lower", "defining function {:+}", func.name);

            let location = self.tcx.hir_span_of_node(func.id);

            let mut func_lower = LowerFunction::new(self);
            let func = func_lower.define(
                func.id,
                &func.name,
                func.sig(),
                lume_tir::FunctionKind::Static,
                location,
            )?;

            self.ir.functions.insert(func.id, func);
        }

        Ok(())
    }

    fn lower_callables(&mut self) -> Result<()> {
        for method in self.tcx.tdb().methods() {
            if !self.tcx.hir_is_local_node(method.id) || !self.should_lower_method(method) {
                continue;
            }

            tracing::debug!(target: "tir_lower", "lowering method {:+}", method.name);

            self.lower_block(method.id)?;
        }

        for func in self.tcx.tdb().functions() {
            if !self.tcx.hir_is_local_node(func.id) {
                continue;
            }

            tracing::debug!(target: "tir_lower", "lowering function {:+}", func.name);

            self.lower_block(func.id)?;
        }

        Ok(())
    }

    fn lower_block(&mut self, id: NodeId) -> Result<()> {
        let block = if let Some(body) = self.tcx.hir_body_of_node(id) {
            let mut func_lower = LowerFunction::new(self);

            Some(func_lower.lower_block(body)?)
        } else {
            None
        };

        self.ir.functions.get_mut(&id).unwrap().block = block;

        Ok(())
    }

    /// Determines the [`lume_tir::FunctionKind`] of the given method.
    pub(crate) fn determine_method_kind(&self, method: &lume_types::Method, has_body: bool) -> lume_tir::FunctionKind {
        // Checks whether the method is an implementation of `std::ops::Dispose::dispose()`.
        if self.tcx.is_method_dropper(method.id) {
            return lume_tir::FunctionKind::Dropper;
        }

        // Intrinsic methods are only defined so they can be type-checked against.
        // They do not need to exist within the binary.
        if method.is_intrinsic() {
            return lume_tir::FunctionKind::Dynamic;
        }

        // Trait method definitions without any default implementation have no reason to be in the
        // binary, since they have no body to codegen from.
        if is_dynamic_dispatch(method, has_body) {
            return lume_tir::FunctionKind::Dynamic;
        }

        // Static trait method definitions cannot be called, if they do not have a default
        // body to be invoked.
        if method.is_static() && method.is_trait_definition() && !has_body {
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

        // Trait method definitions without any default implementation have no reason to be in the
        // binary, since they have no body to codegen from.
        if is_dynamic_dispatch(method, has_body) {
            return false;
        }

        // Certain kinds of functions should never be lowered, such as static trait methods with
        // default implementations.
        if let Some(declaration) = self.ir.functions.get(&method.id)
            && !declaration.kind.should_be_lowered()
        {
            return false;
        }

        true
    }
}

/// Determines whether the given method is meant to be invoked dynamically via dynamic dispatch.
#[inline]
#[must_use]
pub(crate) fn is_dynamic_dispatch(method: &lume_types::Method, has_body: bool) -> bool {
    method.kind == lume_types::MethodKind::TraitDefinition && method.parameters.is_instanced() && !has_body
}

struct LowerFunction<'tcx> {
    /// Defines the parent lowering context.
    lower: &'tcx Lower<'tcx>,

    variable_mapping: IndexMap<lume_span::NodeId, VariableId>,
    variable_source: IndexMap<VariableId, VariableSource>,
}

impl<'tcx> LowerFunction<'tcx> {
    pub fn new(lower: &'tcx Lower<'tcx>) -> Self {
        Self {
            lower,
            variable_mapping: IndexMap::new(),
            variable_source: IndexMap::new(),
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
        let hir_type_params = self
            .lower
            .tcx
            .hir_avail_type_params(id)
            .iter()
            .map(|param| param.id)
            .collect::<Vec<_>>();

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

    /// Allocates a new variable in the function with the given source and returns it's ID.
    fn mark_variable(&mut self, source: VariableSource) -> VariableId {
        let id = VariableId(self.variable_source.len());
        self.variable_source.insert(id, source);
        id
    }

    fn parameters(&mut self, params: &lume_types::Parameters) -> Vec<lume_tir::Parameter> {
        params
            .params
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
