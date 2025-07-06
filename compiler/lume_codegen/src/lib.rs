use std::collections::HashMap;

use inkwell::{basic_block::BasicBlock, types::BasicTypeEnum, values::PointerValue};
use lume_mir::{Function, RegisterId};

mod inst;
mod term;
mod ty;
mod value;
pub(crate) mod wrap;

pub(crate) use wrap::*;

pub struct Generator<'ctx> {
    package: &'ctx lume_session::Package,
    mir: lume_mir::ModuleMap,
    context: Context,
}

impl<'ctx> Generator<'ctx> {
    pub fn codegen(package: &'ctx lume_session::Package, mir: lume_mir::ModuleMap) {
        Self::new(package, mir).build();
    }

    pub fn new(package: &'ctx lume_session::Package, mir: lume_mir::ModuleMap) -> Self {
        Self {
            package,
            mir,
            context: Context::new(),
        }
    }

    pub fn build(&self) {
        let module = self.context.create_module(&self.package.name);
        module.build(&self.mir.functions);

        module.inner.print_to_stderr();
    }
}

struct FunctionLower<'ctx> {
    builder: Builder<'ctx>,
    func: &'ctx Function,
    variables: HashMap<RegisterId, PointerValue<'ctx>>,
    variable_types: HashMap<RegisterId, BasicTypeEnum<'ctx>>,
}

impl<'ctx> FunctionLower<'ctx> {
    pub fn lower(builder: Builder<'ctx>, func: &'ctx Function) {
        let mut lower = Self::new(builder, func);
        lower.build();
    }

    pub fn new(builder: Builder<'ctx>, func: &'ctx Function) -> Self {
        Self {
            builder,
            func,
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
            self.variables.insert(id, self.builder.alloca(llvm_ty));
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

    pub(crate) fn var(&self, id: RegisterId) -> PointerValue<'ctx> {
        self.variables[&id]
    }

    pub(crate) fn var_type(&self, id: RegisterId) -> BasicTypeEnum<'ctx> {
        self.variable_types[&id]
    }

    pub(crate) fn load(&self, id: RegisterId) -> (PointerValue<'ctx>, BasicTypeEnum<'ctx>) {
        (self.var(id), self.var_type(id))
    }
}
