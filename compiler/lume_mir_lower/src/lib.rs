pub(crate) mod expr;
pub(crate) mod stmt;
pub(crate) mod ty;

use indexmap::IndexMap;
use lume_infer::query::CallReference;
use lume_mir::{Function, FunctionId, ModuleMap, RegisterId};
use lume_typech::TyCheckCtx;
use lume_types::{FunctionSig, TypeDatabaseContext};

/// Defines a transformer which will lower a typed HIR map into an MIR map.
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
        for method in tdb.methods() {
            let id = self.mir.new_function_id(CallReference::Method(method.id));

            self.define_callable(id, method.name.to_string(), &method.sig());
        }

        for func in &tdb.functions {
            let id = self.mir.new_function_id(CallReference::Function(func.id));

            self.define_callable(id, func.name.to_string(), &func.sig());
        }
    }

    fn transform_callables(&mut self, tdb: &TypeDatabaseContext) {
        for method in tdb.methods() {
            let id = self.mir.new_function_id(CallReference::Method(method.id));
            let body = self.tcx.hir_body_of_def(method.hir);

            self.transform_callable(id, body);
        }

        for func in &tdb.functions {
            let id = self.mir.new_function_id(CallReference::Function(func.id));
            let body = self.tcx.hir_body_of_item(func.hir);

            self.transform_callable(id, body);
        }
    }

    fn define_callable(&mut self, id: FunctionId, name: String, signature: &FunctionSig) {
        let func = FunctionTransformer::define(self, &self.mir, id, name, signature);
        self.mir.functions.insert(id, func);
    }

    fn transform_callable(&mut self, id: FunctionId, body: Option<&lume_hir::Block>) {
        // Grab the existing function from the MIR, which hasn't yet been transformed.
        let func = self.mir.function(id).clone();

        // Transform the function
        let func = FunctionTransformer::transform(self, &self.mir, func, body);

        // Replace the previous function with the transformed one.
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
            transformer.func.signature.external = true;
        }

        transformer.func
    }

    fn lower_signature(&mut self, signature: &lume_types::FunctionSig<'mir>) {
        for param in signature.params.inner() {
            let param_ty = self.lower_type(&param.ty);

            self.func.signature.parameters.push(param_ty.clone());

            // Offset the register counter by the number of parameters
            self.func.registers.allocate_param(param_ty);
        }

        self.func.signature.return_type = self.lower_type(signature.ret_ty);
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

    pub(crate) fn function(&self, func_id: FunctionId) -> &Function {
        self.transformer.mir.function(func_id)
    }

    /// Defines a new declaration in the current function block.
    fn declare(&mut self, decl: lume_mir::Declaration) -> RegisterId {
        let ty = self.type_of_decl(&decl);

        self.func.declare(ty, decl)
    }

    /// Defines a new operand declaration in the current function block.
    fn declare_value(&mut self, value: lume_mir::Operand) -> RegisterId {
        self.declare(lume_mir::Declaration::Operand(value))
    }

    /// Defines a new call instruction in the current function block.
    fn call(&mut self, func_id: FunctionId, args: Vec<lume_mir::Operand>) -> lume_mir::Operand {
        let call_inst = self.declare(lume_mir::Declaration::Call { func_id, args });

        lume_mir::Operand::Load { id: call_inst }
    }
}
