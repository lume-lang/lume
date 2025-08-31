mod errors;
pub(crate) mod expr;
pub(crate) mod generic;
pub(crate) mod path;
pub(crate) mod stmt;

pub mod reify;

use error_snippet::Result;
use indexmap::IndexMap;
use lume_span::{DefId, Internable, Location};
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
            // We only build type metadata of concrete types, so we skip
            // generic types and type parameters.
            if ty.is_generic() || ty.is_type_parameter() {
                continue;
            }

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

            let location = self.tcx.hir_span_of_def(method.hir);
            let kind = if is_dynamic_dispatch(method, self.tcx.hir_body_of_def(method.hir).is_some()) {
                lume_tir::FunctionKind::Dynamic
            } else {
                lume_tir::FunctionKind::Static
            };

            let mut func_lower = LowerFunction::new(self);
            let func = func_lower.define(method.hir, &method.name, method.sig(), kind, location)?;

            self.ir.functions.insert(func.id, func);
        }

        for func in self.tcx.tdb().functions() {
            tracing::debug!(target: "tir_lower", "defining function {:+}", func.name);

            let location = self.tcx.hir_span_of_def(lume_span::DefId::Item(func.hir));

            let mut func_lower = LowerFunction::new(self);
            let func = func_lower.define(
                DefId::Item(func.hir),
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
            if should_skip_method(method, self.tcx.hir_body_of_def(method.hir).is_some()) {
                continue;
            }

            tracing::debug!(target: "tir_lower", "lowering method {:+}", method.name);

            self.lower_block(method.hir)?;
        }

        for func in self.tcx.tdb().functions() {
            tracing::debug!(target: "tir_lower", "lowering function {:+}", func.name);

            self.lower_block(DefId::Item(func.hir))?;
        }

        Ok(())
    }

    fn lower_block(&mut self, id: DefId) -> Result<()> {
        let block = if let Some(body) = self.tcx.hir_body_of_def(id) {
            let mut func_lower = LowerFunction::new(self);

            Some(func_lower.lower_block(body)?)
        } else {
            None
        };

        self.ir.functions.get_mut(&id).unwrap().block = block;

        Ok(())
    }
}

pub(crate) fn should_skip_method(method: &lume_types::Method, has_body: bool) -> bool {
    // Intrinsic methods are only defined so they can be type-checked against.
    // They do not need to exist within the binary.
    if method.is_intrinsic() {
        return true;
    }

    // Trait method definitions without any default implementation have no reason to be in the
    // binary, since they have no body to codegen from.
    if is_dynamic_dispatch(method, has_body) {
        return true;
    }

    false
}

/// Determines whether the given method is meant to be invoked dynamically via dynamic dispatch.
#[inline]
#[must_use]
pub(crate) fn is_dynamic_dispatch(method: &lume_types::Method, has_body: bool) -> bool {
    method.kind == lume_types::MethodKind::TraitDefinition && !has_body
}

struct LowerFunction<'tcx> {
    /// Defines the parent lowering context.
    lower: &'tcx Lower<'tcx>,

    variable_mapping: IndexMap<lume_span::StatementId, VariableId>,
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
        id: DefId,
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
            .map(|param| param.type_param_id.unwrap())
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
