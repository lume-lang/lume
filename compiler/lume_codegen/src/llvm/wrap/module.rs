use std::{collections::HashMap, sync::RwLock};

use inkwell::{
    attributes::AttributeLoc,
    execution_engine::ExecutionEngine,
    passes::PassBuilderOptions,
    targets::{CodeModel, RelocMode, Target, TargetMachine, TargetTriple},
    values::{FunctionValue, GlobalValue},
};
use lume_mir::FunctionId;
use lume_session::OptimizationLevel;

use crate::llvm::{Context, Function};

pub struct Module<'ctx> {
    context: &'ctx Context,
    inner: inkwell::module::Module<'ctx>,

    name: String,
    strings: RwLock<HashMap<String, GlobalValue<'ctx>>>,
    functions: RwLock<HashMap<FunctionId, FunctionValue<'ctx>>>,
}

impl<'ctx> Module<'ctx> {
    pub(crate) fn new(context: &'ctx Context, name: &str) -> Self {
        let inner = context.inner.create_module(name);

        Self {
            name: name.to_owned(),
            context,
            inner,
            strings: RwLock::new(HashMap::new()),
            functions: RwLock::new(HashMap::new()),
        }
    }

    /// Retrieves the default target triple for the current platform.
    pub(crate) fn target_triple() -> TargetTriple {
        TargetMachine::get_default_triple()
    }

    /// Retrieves the default target for the current platform.
    pub(crate) fn target() -> Target {
        Target::from_triple(&Self::target_triple()).unwrap()
    }

    /// Retrieves the default code model for the current platform.
    pub(crate) fn code_model() -> CodeModel {
        CodeModel::Default
    }

    /// Retrieves the default code model for the current platform.
    pub(crate) fn target_machine(opt: OptimizationLevel) -> TargetMachine {
        let reloc = RelocMode::Default;
        let target = Self::target();
        let target_triple = Self::target_triple();
        let model = Self::code_model();

        let opt = match opt {
            OptimizationLevel::O0 => inkwell::OptimizationLevel::None,
            OptimizationLevel::O1 => inkwell::OptimizationLevel::Less,
            OptimizationLevel::O3 => inkwell::OptimizationLevel::Aggressive,
            _ => inkwell::OptimizationLevel::Default,
        };

        target
            .create_target_machine(&target_triple, "", "", opt, reloc, model)
            .unwrap()
    }

    /// Optimizes the module using the specified optimization level.
    pub(crate) fn optimize(&self, opt: OptimizationLevel) {
        let target_machine = Self::target_machine(opt);

        let passes = match opt {
            OptimizationLevel::O0 => "default<O0>",
            OptimizationLevel::O1 => "function(mem2reg),default<O1>",
            OptimizationLevel::O2 => "function(mem2reg),default<O2>",
            OptimizationLevel::O3 => "function(mem2reg),default<O3>",
            OptimizationLevel::Os => "function(mem2reg),default<Os>",
            OptimizationLevel::Oz => "function(mem2reg),default<Oz>",
        };

        self.inner
            .run_passes(passes, &target_machine, PassBuilderOptions::create())
            .unwrap();
    }

    /// Verifies that the module is valid.
    pub(crate) fn verify(&self) {
        self.inner.verify().unwrap();
    }

    /// Prints the module to `stdout`.
    pub(crate) fn print_to_stdout(&self) {
        self.inner.print_to_stderr();
    }

    /// Gets the compiled object bytecode from the module.
    pub(crate) fn to_bytecode(&self) -> Vec<u8> {
        let machine = Self::target_machine(OptimizationLevel::O0);
        let buffer = machine
            .write_to_memory_buffer(&self.inner, inkwell::targets::FileType::Object)
            .unwrap();

        buffer.as_slice().to_vec()
    }

    /// Evaluates the function with the given name using LLVMs JIT execution engine.
    pub(crate) fn jit_execution_engine(&'_ self) -> ExecutionEngine<'_> {
        self.inner
            .create_jit_execution_engine(inkwell::OptimizationLevel::None)
            .unwrap()
    }

    /// Evaluates the function with the given name using LLVMs JIT execution engine.
    pub(crate) fn evaluate_function(&self, name: &str) -> u64 {
        let engine = self.jit_execution_engine();

        let func = self.inner.get_function(name).unwrap();
        let result = unsafe { engine.run_function(func, &[]) };

        result.as_int(false)
    }

    /// Evaluates the main function using LLVMs JIT execution engine.
    #[allow(dead_code)]
    pub(crate) fn evaluate_main(&self) -> u64 {
        self.evaluate_function("main")
    }

    #[expect(dead_code)]
    pub(crate) fn add_global_string(&self, string: &str) -> GlobalValue<'ctx> {
        if let Some(global) = self.strings.read().unwrap().get(string) {
            *global
        } else {
            let len = self.strings.read().unwrap().len();
            let name = format!("@__SYM_{}_{}", self.name, len);
            let global = self.inner.add_global(self.context.ptr_type(), None, &name);

            let string_value = self.context.inner.const_string(string.as_bytes(), false);
            global.set_initializer(&string_value);
            global.set_constant(true);
            global.set_linkage(inkwell::module::Linkage::Internal);

            self.strings.write().unwrap().insert(string.to_owned(), global);

            global
        }
    }

    pub(crate) fn add_function(&self, func: &Function) -> FunctionValue<'ctx> {
        self.inner.get_function(&func.name).unwrap_or_else(|| {
            let function_type = self.context.lower_fn_type(&func.signature, false);
            let function_value = self.inner.add_function(&func.name, function_type, None);

            for (idx, param_type) in func.signature.parameters.iter().enumerate() {
                #[allow(clippy::cast_possible_truncation)]
                let loc = AttributeLoc::Param(idx as u32);

                if param_type.is_reference_type() {
                    function_value.add_attribute(loc, self.context.attribute_flag("nonnull"));
                    function_value.add_attribute(loc, self.context.attribute_flag("noundef"));
                }
            }

            self.functions.write().unwrap().insert(func.id, function_value);

            function_value
        })
    }

    pub(crate) fn find_function(&self, id: FunctionId) -> FunctionValue<'ctx> {
        *self.functions.read().unwrap().get(&id).unwrap()
    }
}
