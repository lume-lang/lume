use std::collections::HashMap;

use inkwell::{
    basic_block::BasicBlock,
    types::BasicTypeEnum,
    values::{BasicValue, BasicValueEnum, PointerValue},
};
use lume_mir::{Function, RegisterId};

mod inst;
mod term;
mod ty;
mod value;
pub(crate) mod wrap;

use lume_session::Options;
pub(crate) use wrap::*;

pub struct Generator<'ctx> {
    package: &'ctx lume_session::Package,
    mir: lume_mir::ModuleMap,
    context: Context,
    options: &'ctx Options,
}

impl<'ctx> Generator<'ctx> {
    pub fn codegen(package: &'ctx lume_session::Package, mir: lume_mir::ModuleMap, opts: &'ctx Options) {
        Self::new(package, mir, opts).build();
    }

    pub fn new(package: &'ctx lume_session::Package, mir: lume_mir::ModuleMap, opts: &'ctx Options) -> Self {
        Self {
            package,
            mir,
            context: Context::new(),
            options: opts,
        }
    }

    pub fn build(&self) {
        let module = self.context.create_module(&self.package.name);

        for func in self.mir.functions.values() {
            module.add_function(func);
        }

        for func in self.mir.functions.values() {
            let function_ty = module.find_function(func.id);
            let builder = self.context.create_builder(function_ty);

            FunctionLower::lower(&module, builder, func);
        }

        if self.options.print_llvm_ir {
            module.print_to_stdout();
        }
    }
}

struct FunctionLower<'func, 'ctx> {
    func: &'func Function,

    builder: Builder<'ctx>,
    module: &'func Module<'ctx>,

    variables: HashMap<RegisterId, BasicValueEnum<'ctx>>,
    variable_types: HashMap<RegisterId, BasicTypeEnum<'ctx>>,
}

impl<'func, 'ctx> FunctionLower<'func, 'ctx> {
    pub fn lower(module: &'func Module<'ctx>, builder: Builder<'ctx>, func: &'func Function) {
        let mut lower = Self::new(module, builder, func);
        lower.build();
    }

    pub fn new(module: &'func Module<'ctx>, builder: Builder<'ctx>, func: &'func Function) -> Self {
        Self {
            func,
            builder,
            module,
            variables: HashMap::new(),
            variable_types: HashMap::new(),
        }
    }

    pub fn build(&mut self) {
        // Pre-allocate blocks for the function, so that they can be referenced
        // before they are visited by the transformer.
        for block in &self.func.blocks {
            self.builder.register_block(block.id, self.builder.add_block());
        }

        self.create_func_registers();

        for block in &self.func.blocks {
            self.build_block(block);
        }
    }

    fn create_func_registers(&mut self) {
        for (id, reg) in self.func.registers.iter() {
            self.builder.switch_to_block_id(reg.block);

            let llvm_ty = self.builder.ctx.lower_type(&reg.ty);

            self.variable_types.insert(id, llvm_ty);
            self.variables
                .insert(id, self.builder.alloca(llvm_ty).as_basic_value_enum());
        }
    }

    fn build_block(&self, block: &lume_mir::BasicBlock) {
        self.builder.switch_to_block(self.builder.block(block.id));

        for inst in block.instructions() {
            self.instruction(inst);
        }

        if let Some(terminator) = block.terminator() {
            self.terminator(terminator);
        }
    }

    pub(crate) fn var(&self, id: RegisterId) -> BasicValueEnum<'ctx> {
        self.variables[&id]
    }

    pub(crate) fn var_type(&self, id: RegisterId) -> BasicTypeEnum<'ctx> {
        self.variable_types[&id]
    }

    pub(crate) fn load(&self, id: RegisterId) -> (BasicValueEnum<'ctx>, BasicTypeEnum<'ctx>) {
        (self.var(id), self.var_type(id))
    }

    pub(crate) fn load_ptr(&self, id: RegisterId) -> (PointerValue<'ctx>, BasicTypeEnum<'ctx>) {
        (self.var(id).into_pointer_value(), self.var_type(id))
    }
}
