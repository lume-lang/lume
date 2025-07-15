pub(crate) mod expr;
pub(crate) mod path;
pub(crate) mod stmt;

use error_snippet::Result;
use indexmap::IndexMap;
use lume_span::Internable;
use lume_tir::{FunctionId, FunctionKind, TypedIR, VariableId, VariableSource};
use lume_typech::TyCheckCtx;

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
            tracing::debug!(target: "tir_lower", "defining method {:+}", method.name);

            let id = FunctionId::new(FunctionKind::Method, method.id.0);
            let mut func_lower = LowerFunction::new(self);
            let func = func_lower.define(id, method.hir, &method.name, method.sig())?;

            self.ir.functions.insert(id, func);
        }

        for func in self.tcx.tdb().functions() {
            tracing::debug!(target: "tir_lower", "defining function {:+}", func.name);

            let id = FunctionId::new(FunctionKind::Function, func.id.index.as_usize());
            let mut func_lower = LowerFunction::new(self);
            let func = func_lower.define(id, lume_span::DefId::Item(func.hir), &func.name, func.sig())?;

            self.ir.functions.insert(id, func);
        }

        Ok(())
    }

    fn lower_callables(&mut self) -> Result<()> {
        for method in self.tcx.tdb().methods() {
            tracing::debug!(target: "tir_lower", "lowering method {:+}", method.name);

            let id = FunctionId::new(FunctionKind::Method, method.id.0);
            self.lower_block(id, method.hir)?;
        }

        for func in self.tcx.tdb().functions() {
            tracing::debug!(target: "tir_lower", "lowering function {:+}", func.name);

            let id = FunctionId::new(FunctionKind::Function, func.id.index.as_usize());
            self.lower_block(id, lume_span::DefId::Item(func.hir))?;
        }

        Ok(())
    }

    fn lower_block(&mut self, id: FunctionId, hir: lume_span::DefId) -> Result<()> {
        let block = if let Some(body) = self.tcx.hir_body_of_def(hir) {
            let mut func_lower = LowerFunction::new(self);

            Some(func_lower.lower_block(body)?)
        } else {
            None
        };

        self.ir.functions.get_mut(&id).unwrap().block = block;

        Ok(())
    }
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
        id: FunctionId,
        hir_id: lume_span::DefId,
        name: &lume_hir::Path,
        signature: lume_types::FunctionSig,
    ) -> Result<lume_tir::Function> {
        let type_params = self.lower.tcx.hir_avail_type_params(hir_id);
        let type_params = type_params.iter().map(AsRef::as_ref).collect::<Vec<_>>();
        let name = self.path_generic(name, &type_params)?;

        let parameters = self.parameters(signature.params);
        let return_type = signature.ret_ty.clone();

        Ok(lume_tir::Function {
            id,
            name,
            parameters,
            return_type,
            block: None,
        })
    }

    fn lower_block(&mut self, block: &lume_hir::Block) -> Result<lume_tir::Block> {
        let statements = block
            .statements
            .iter()
            .map(|stmt| self.statement(stmt))
            .collect::<Result<Vec<_>>>()?;

        Ok(lume_tir::Block { statements })
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
                }
            })
            .collect()
    }
}
