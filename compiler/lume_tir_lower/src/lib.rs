pub(crate) mod diagnostics;
pub(crate) mod expr;
pub(crate) mod generic;
pub(crate) mod metadata;
pub(crate) mod path;
pub(crate) mod pattern;
pub(crate) mod reification;
pub(crate) mod stmt;

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
    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    pub fn lower(mut self) -> Result<TypedIR> {
        self.define_callables()?;
        self.lower_callables()?;

        let metadata_builder = metadata::MetadataBuilder::new(self.tcx);
        self.ir.metadata = metadata_builder.build_metadata()?;

        let mut reification_pass = reification::ReificationPass::new(self.tcx);

        for function in self.ir.functions.values_mut() {
            reification_pass.add_metadata_parameters(function);
        }

        for function in self.ir.functions.values_mut() {
            reification_pass.add_metadata_arguments(function)?;
        }

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

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn define_callables(&mut self) -> Result<()> {
        for method in self.tcx.tdb().methods() {
            if method.is_intrinsic() {
                continue;
            }

            tracing::debug!("defining method {:+}", method.name);

            let signature = self.tcx.signature_of(Callable::Method(method))?;
            let location = self.tcx.hir_span_of_node(method.id);
            let kind = self.determine_method_kind(method);
            let visibility = if self.tcx.should_export(method.id)? {
                lume_tir::Visibility::Exported
            } else {
                lume_tir::Visibility::Local
            };

            let mut func_lower = LowerFunction::new(self);
            let func = func_lower.define(method.id, &method.name, signature.as_ref(), kind, visibility, location)?;

            self.mappings.insert(method.id, func_lower.variables);
            self.ir.functions.insert(func.id, func);
        }

        for func in self.tcx.tdb().functions() {
            tracing::debug!("defining function {:+}", func.name);

            let signature = self.tcx.signature_of(Callable::Function(func))?;
            let location = self.tcx.hir_span_of_node(func.id);
            let visibility = if self.tcx.should_export(func.id)? {
                lume_tir::Visibility::Exported
            } else {
                lume_tir::Visibility::Local
            };

            let mut func_lower = LowerFunction::new(self);
            let func = func_lower.define(
                func.id,
                &func.name,
                signature.as_ref(),
                lume_tir::FunctionKind::Static,
                visibility,
                location,
            )?;

            self.mappings.insert(func.id, func_lower.variables);
            self.ir.functions.insert(func.id, func);
        }

        Ok(())
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn lower_callables(&mut self) -> Result<()> {
        for method in self.tcx.tdb().methods() {
            if !self.tcx.hir_is_local_node(method.id) || method.kind == lume_types::MethodKind::Intrinsic {
                continue;
            }

            tracing::debug!("lowering method {:+}", method.name);

            self.lower_block(method.id)?;
        }

        for func in self.tcx.tdb().functions() {
            if !self.tcx.hir_is_local_node(func.id) {
                continue;
            }

            tracing::debug!("lowering function {:+}", func.name);

            self.lower_block(func.id)?;
        }

        Ok(())
    }

    #[tracing::instrument(level = "DEBUG", skip_all, fields(%id), err)]
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
    #[tracing::instrument(level = "DEBUG", skip_all, fields(method = method.name.to_wide_string()), ret)]
    pub(crate) fn determine_method_kind(&self, method: &lume_types::Method) -> lume_tir::FunctionKind {
        // Checks whether the method is an implementation of
        // `std::ops::Dispose::dispose()`.
        if self.tcx.is_method_dropper(method.id) {
            return lume_tir::FunctionKind::Dropper;
        }

        // Intrinsic methods are only defined so they can be type-checked against.
        // They do not need to exist within the binary.
        if method.is_intrinsic() {
            return lume_tir::FunctionKind::Intrinsic;
        }

        // Trait method definitions without default implementations will be dynamically
        // dispatched using the passed type metadata.
        if self.tcx.is_callable_dynamic(method.id) {
            return lume_tir::FunctionKind::Dynamic;
        }

        lume_tir::FunctionKind::Static
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
        visibility: lume_tir::Visibility,
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
            visibility,
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
                    kind: lume_tir::ParameterKind::Regular,
                    location: param.location,
                }
            })
            .collect()
    }
}
