pub(crate) mod expr;
pub(crate) mod stmt;
pub(crate) mod ty;

use lume_mir::{Function, FunctionId, ModuleMap};
use lume_typech::TyCheckCtx;

pub struct ModuleTransformer<'tcx> {
    tcx: &'tcx TyCheckCtx,

    /// Defines the MIR map which is being created.
    mir: ModuleMap,
}

impl<'tcx> ModuleTransformer<'tcx> {
    /// Transforms the supplied context into a MIR map.
    pub fn transform(tcx: &'tcx TyCheckCtx) -> ModuleMap {
        let mut transformer = Self {
            tcx,
            mir: ModuleMap::new(),
        };

        for method in &transformer.tcx.tdb().methods {
            let body = transformer.tcx.hir_body_of(method.hir);

            transformer.transform_method(method, body);
        }

        for func in &transformer.tcx.tdb().functions {
            let body = transformer.tcx.hir_body_of(func.hir);

            transformer.transform_function(func, body);
        }

        transformer.mir
    }

    fn transform_method(&mut self, method: &lume_types::Method, body: Option<&lume_hir::Block>) {
        let id = self.mir.new_function_id();
        let name = method.name.to_string();

        let func = FunctionTransformer::transform(self, &self.mir, id, name, &method.sig(), body);
        self.mir.functions.push(func);
    }

    fn transform_function(&mut self, func: &lume_types::Function, body: Option<&lume_hir::Block>) {
        let id = self.mir.new_function_id();
        let name = func.name.to_string();

        let func = FunctionTransformer::transform(self, &self.mir, id, name, &func.sig(), body);
        self.mir.functions.push(func);
    }
}

#[allow(dead_code)]
pub(crate) struct FunctionTransformer<'mir> {
    transformer: &'mir ModuleTransformer<'mir>,

    mir: &'mir ModuleMap,

    /// Defines the MIR function which is being created.
    func: Function,
}

impl<'mir> FunctionTransformer<'mir> {
    /// Transforms the supplied context into a MIR map.
    pub fn transform(
        transformer: &'mir ModuleTransformer,
        mir: &'mir ModuleMap,
        id: FunctionId,
        name: String,
        signature: &lume_types::FunctionSig<'mir>,
        block: Option<&lume_hir::Block>,
    ) -> Function {
        let mut transformer = Self {
            transformer,
            mir,
            func: Function::new(id, name),
        };

        transformer.lower_signature(signature);

        if let Some(body) = block {
            transformer.lower(body);
        } else {
            transformer.func.external = true;
        }

        transformer.func
    }

    fn lower_signature(&mut self, signature: &lume_types::FunctionSig<'mir>) {
        for param in signature.params.inner() {
            let param_ty = self.lower_type(&param.ty);

            self.func.parameters.push(param_ty);
        }

        self.func.return_type = self.lower_type(signature.ret_ty);
    }

    fn lower(&mut self, block: &lume_hir::Block) {
        let _entry_block = self.func.new_active_block();

        for statement in &block.statements {
            self.statement(statement);
        }
    }
}
