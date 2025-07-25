use std::collections::HashMap;

use inkwell::{
    targets::InitializationConfig,
    types::BasicTypeEnum,
    values::{BasicValue, BasicValueEnum, PointerValue},
};
use lume_mir::{Function, RegisterId};

mod inst;
mod term;
mod ty;
mod value;
pub(crate) mod wrap;

use lume_session::OptimizationLevel;
pub(crate) use wrap::*;

use crate::CompiledModule;

pub struct LlvmBackend<'ctx> {
    codegen_context: crate::Context<'ctx>,
    llvm_context: Context,
}

impl<'ctx> LlvmBackend<'ctx> {
    pub fn new(context: crate::Context<'ctx>) -> Self {
        Self {
            codegen_context: context,
            llvm_context: Context::new(),
        }
    }

    pub fn build(&'_ self) -> Module<'_> {
        let module = self.llvm_context.create_module(&self.codegen_context.package.name);

        for func in self.codegen_context.mir.functions.values() {
            module.add_function(func);
        }

        for func in self.codegen_context.mir.functions.values() {
            let function_ty = module.find_function(func.id);
            let builder = self.llvm_context.create_builder(function_ty);

            FunctionLower::lower(&module, builder, func);
        }

        module
    }
}

impl<'ctx> crate::Backend<'ctx> for LlvmBackend<'ctx> {
    #[tracing::instrument(level = "DEBUG", skip(self), err)]
    fn initialize(&mut self) -> lume_errors::Result<()> {
        inkwell::targets::Target::initialize_native(&InitializationConfig::default()).unwrap();

        Ok(())
    }

    #[tracing::instrument(level = "DEBUG", skip(self), err)]
    fn generate(&mut self) -> lume_errors::Result<CompiledModule> {
        let module = self.build();

        if self.codegen_context.options.optimize != OptimizationLevel::O0 {
            module.optimize(self.codegen_context.options.optimize);
        }

        if self.codegen_context.options.print_codegen_ir {
            module.print_to_stdout();
        }

        module.verify();

        let bytecode = module.to_bytecode();

        Ok(CompiledModule {
            name: self.codegen_context.package.name.clone(),
            bytecode,
        })
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
        for reg in self.func.registers.iter() {
            let id = reg.id;

            if let Some(block) = reg.block {
                self.builder.switch_to_block_id(block);

                let llvm_ty = self.builder.ctx.lower_type(&reg.ty);

                self.variable_types.insert(id, llvm_ty);
                self.variables
                    .insert(id, self.builder.alloca(llvm_ty).as_basic_value_enum());
            } else {
                #[allow(clippy::cast_possible_truncation)]
                let value = self.builder.func_value.get_nth_param(id.as_usize() as u32).unwrap();

                self.variable_types.insert(id, value.get_type());
                self.variables.insert(id, value);
            }
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

    pub(crate) fn retrieve_var_ptr(&self, id: RegisterId) -> (PointerValue<'ctx>, BasicTypeEnum<'ctx>) {
        (self.var(id).into_pointer_value(), self.var_type(id))
    }

    pub(crate) fn load(&self, id: RegisterId) -> BasicValueEnum<'ctx> {
        let (ptr, ty) = self.retrieve_var_ptr(id);

        self.builder.load(ptr, ty)
    }

    #[allow(clippy::cast_possible_truncation)]
    pub(crate) fn load_field(&self, id: RegisterId, field: usize) -> BasicValueEnum<'ctx> {
        let (ptr, struct_type) = self.retrieve_var_ptr(id);

        let struct_type = struct_type.into_struct_type();
        let field_ty = struct_type.get_field_type_at_index(field as u32).unwrap();

        self.builder.load_field(struct_type, ptr, field, field_ty)
    }
}
