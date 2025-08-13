pub(crate) mod expr;
pub(crate) mod passes;
pub(crate) mod stmt;
pub(crate) mod ty;

use std::collections::HashMap;

use lume_mir::{Function, FunctionId, ModuleMap, RegisterId};
use lume_typech::TyCheckCtx;

/// Defines a transformer which will lower a typed HIR map into an MIR map.
pub struct ModuleTransformer<'tcx> {
    tcx: &'tcx TyCheckCtx,

    /// Defines the MIR map which is being created.
    mir: ModuleMap,
}

impl<'tcx> ModuleTransformer<'tcx> {
    /// Transforms the supplied context into a MIR map.
    pub fn transform(tcx: &'tcx TyCheckCtx, tir: lume_tir::TypedIR) -> ModuleMap {
        let mut transformer = Self {
            tcx,
            mir: ModuleMap::new(tir.metadata),
        };

        for func in tir.functions.values() {
            transformer.define_callable(func);
        }

        for func in tir.functions.values() {
            transformer.transform_callable(func);
        }

        transformer.mir
    }

    fn define_callable(&mut self, func: &lume_tir::Function) {
        let id = lume_mir::FunctionId(func.id.as_usize());
        let func = FunctionTransformer::define(self, id, func);

        self.mir.functions.insert(id, func);
    }

    fn transform_callable(&mut self, func: &lume_tir::Function) {
        let id = lume_mir::FunctionId(func.id.as_usize());
        let func = FunctionTransformer::transform(self, id, func);

        self.mir.functions.insert(id, func);
    }
}

pub(crate) struct FunctionTransformer<'mir> {
    transformer: &'mir ModuleTransformer<'mir>,

    /// Defines the MIR function which is being created.
    func: Function,

    variables: HashMap<lume_tir::VariableId, RegisterId>,
}

impl<'mir> FunctionTransformer<'mir> {
    /// Defines the MIR function which is being created.
    pub fn define(transformer: &'mir ModuleTransformer, id: FunctionId, func: &lume_tir::Function) -> Function {
        let mut transformer = Self {
            transformer,
            func: Function::new(id, func.name.to_string()),
            variables: HashMap::new(),
        };

        transformer.lower_signature(func);

        transformer.func
    }

    /// Transforms the supplied context into a MIR map.
    pub fn transform(transformer: &'mir ModuleTransformer, id: FunctionId, func: &lume_tir::Function) -> Function {
        let mut transformer = Self {
            transformer,
            func: Function::new(id, func.name.to_string()),
            variables: HashMap::new(),
        };

        transformer.lower_signature(func);

        if let Some(body) = &func.block {
            transformer.lower(body);
        } else {
            transformer.func.signature.external = true;
        }

        transformer.run_passes();

        transformer.func
    }

    fn lower_signature(&mut self, func: &lume_tir::Function) {
        for param in &func.parameters {
            let param_ty = self.lower_type(&param.ty);

            self.func.signature.parameters.push(param_ty.clone());

            // Offset the register counter by the number of parameters
            self.func.registers.allocate_param(param_ty);

            if param.vararg {
                self.func.signature.vararg = true;
            }
        }

        self.func.signature.return_type = self.lower_type(&func.return_type);
    }

    fn lower(&mut self, block: &lume_tir::Block) {
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

    /// Adds a new edge between the two blocks.
    fn add_edge(&mut self, from: lume_mir::BasicBlockId, to: lume_mir::BasicBlockId) {
        self.func.block_mut(to).push_predecessor(from);
        self.func.block_mut(from).push_successor(to);
    }

    /// Defines a new register with a value of `null` and return an reference to it.
    fn null_operand(&mut self) -> lume_mir::Operand {
        let reg = self.func.add_register(lume_mir::Type::pointer(lume_mir::Type::void()));

        self.func
            .current_block_mut()
            .assign(reg, lume_mir::Operand::Boolean { value: false });

        lume_mir::Operand::Reference { id: reg }
    }

    /// Defines a new operand declaration in the current function block.
    fn declare_value(&mut self, value: lume_mir::Operand) -> RegisterId {
        self.declare(lume_mir::Declaration::Operand(value))
    }

    /// Defines a new call instruction in the current function block.
    fn call(&mut self, func_id: FunctionId, mut args: Vec<lume_mir::Operand>) -> lume_mir::Operand {
        let func = self.transformer.mir.function(func_id);
        let params = &func.signature.parameters;

        #[cfg(debug_assertions)]
        if func.signature.vararg {
            debug_assert!(params.len() <= args.len());
        } else {
            debug_assert!(params.len() == args.len());
        }

        // Generic parameters are lowering into accepting pointer types, so all
        // types of argument can be passed.
        //
        // When passing a non-reference argument into a generic parameter, we then
        // need to pass an address to the argument, so the callee can load it. When
        // lowering these arguments, we create a slot in the stack to store the argument,
        // then we pass the address of the stack slot to the function.
        for (arg, param) in args.iter_mut().zip(params.iter()) {
            // If the parameter isn't generic, we let it be.
            if !param.is_generic {
                continue;
            }

            // If the passed type is already a reference type, we can pass it without
            // allocating room for it.
            if self.type_of_value(arg).kind.is_reference_type() {
                continue;
            }

            let arg_ty = self.type_of_value(arg);
            let slot = self.func.alloc_slot(arg_ty);
            self.func.current_block_mut().store_slot(slot, arg.clone());

            *arg = lume_mir::Operand::SlotAddress { id: slot };
        }

        let call_inst = self.declare(lume_mir::Declaration::Call { func_id, args });

        lume_mir::Operand::Reference { id: call_inst }
    }
}
