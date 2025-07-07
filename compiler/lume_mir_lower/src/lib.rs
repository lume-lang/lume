pub(crate) mod expr;
pub(crate) mod stmt;
pub(crate) mod ty;

use indexmap::IndexMap;
use lume_infer::query::CallReference;
use lume_mir::{Function, FunctionId, ModuleMap, RegisterId};
use lume_typech::TyCheckCtx;
use lume_types::TypeDatabaseContext;

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

        transformer.define_callables(tcx.tdb());
        transformer.transform_callables(tcx.tdb());

        transformer.mir
    }

    fn define_callables(&mut self, tdb: &TypeDatabaseContext) {
        for method in &tdb.methods {
            self.define_method(method);
        }

        for func in &tdb.functions {
            self.define_function(func);
        }
    }

    fn transform_callables(&mut self, tdb: &TypeDatabaseContext) {
        for method in &tdb.methods {
            let body = self.tcx.hir_body_of(method.hir);

            self.transform_method(method, body);
        }

        for func in &tdb.functions {
            let body = self.tcx.hir_body_of(func.hir);

            self.transform_function(func, body);
        }
    }

    fn define_method(&mut self, method: &lume_types::Method) {
        let id = self.mir.new_function_id(CallReference::Method(method.id));
        let name = method.name.to_string();

        let func = FunctionTransformer::define(self, &self.mir, id, name, &method.sig());
        self.mir.functions.insert(id, func);
    }

    fn define_function(&mut self, func: &lume_types::Function) {
        let id = self.mir.new_function_id(CallReference::Function(func.id));
        let name = func.name.to_string();

        let func = FunctionTransformer::define(self, &self.mir, id, name, &func.sig());
        self.mir.functions.insert(id, func);
    }

    fn transform_method(&mut self, method: &lume_types::Method, body: Option<&lume_hir::Block>) {
        let id = self.mir.new_function_id(CallReference::Method(method.id));
        let func = self.mir.function(id).clone();

        let func = FunctionTransformer::transform(self, &self.mir, func, body);
        self.mir.functions.insert(id, func);
    }

    fn transform_function(&mut self, func: &lume_types::Function, body: Option<&lume_hir::Block>) {
        let id = self.mir.new_function_id(CallReference::Function(func.id));
        let func = self.mir.function(id).clone();

        let func = FunctionTransformer::transform(self, &self.mir, func, body);
        self.mir.functions.insert(id, func);
    }
}

#[allow(dead_code)]
pub(crate) struct FunctionTransformer<'mir> {
    transformer: &'mir ModuleTransformer<'mir>,

    mir: &'mir ModuleMap,

    /// Defines the MIR function which is being created.
    func: Function,

    variables: IndexMap<lume_span::StatementId, RegisterId>,
}

impl<'mir> FunctionTransformer<'mir> {
    /// Defines the MIR function which is being created.
    pub fn define(
        transformer: &'mir ModuleTransformer,
        mir: &'mir ModuleMap,
        id: FunctionId,
        name: String,
        signature: &lume_types::FunctionSig<'mir>,
    ) -> Function {
        let mut transformer = Self {
            transformer,
            mir,
            func: Function::new(id, name),
            variables: IndexMap::new(),
        };

        transformer.lower_signature(signature);

        transformer.func
    }

    /// Transforms the supplied context into a MIR map.
    pub fn transform(
        transformer: &'mir ModuleTransformer,
        mir: &'mir ModuleMap,
        func: Function,
        block: Option<&lume_hir::Block>,
    ) -> Function {
        let mut transformer = Self {
            transformer,
            mir,
            func,
            variables: IndexMap::new(),
        };

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

        // If the current block is not returning, add a return statement so
        // there's always a valid return value.
        //
        // We're assuming it'll be a void return, since the type checker should
        // have detected a missing return statement in a non-void function.
        self.func.current_block_mut().return_void();
    }

    pub(crate) fn tcx(&self) -> &TyCheckCtx {
        self.transformer.tcx
    }
}
