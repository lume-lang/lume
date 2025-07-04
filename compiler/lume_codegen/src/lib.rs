use inkwell::{basic_block::BasicBlock, values::BasicValue};
use lume_mir::Function;

mod ty;

#[allow(dead_code)]
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

struct Context {
    pub(crate) inner: inkwell::context::Context,
}

impl Context {
    pub fn new() -> Self {
        Self {
            inner: inkwell::context::Context::create(),
        }
    }

    pub fn create_builder<'ctx>(&'ctx self, func: inkwell::values::FunctionValue<'ctx>) -> Builder<'ctx> {
        Builder::new(self, func)
    }

    pub fn create_module(&self, name: &str) -> Module<'_> {
        Module::new(self, name)
    }
}

struct Module<'ctx> {
    context: &'ctx Context,
    inner: inkwell::module::Module<'ctx>,
}

impl<'ctx> Module<'ctx> {
    pub fn new(context: &'ctx Context, name: &str) -> Self {
        let inner = context.inner.create_module(name);

        Self { context, inner }
    }

    pub fn build(&self, funcs: &[Function]) {
        for func in funcs {
            let fn_type = self.context.void_type().fn_type(&[], false);
            let fn_val = self.inner.add_function(&func.name, fn_type, None);
            let builder = self.context.create_builder(fn_val);

            let lower = FunctionLower::new(builder, func);
            lower.build();
        }
    }
}

struct Builder<'ctx> {
    inner: inkwell::builder::Builder<'ctx>,
    pub(crate) func: inkwell::values::FunctionValue<'ctx>,
    pub(crate) ctx: &'ctx Context,
}

#[allow(dead_code)]
impl<'ctx> Builder<'ctx> {
    pub fn new(ctx: &'ctx Context, func: inkwell::values::FunctionValue<'ctx>) -> Self {
        Self {
            inner: ctx.inner.create_builder(),
            func,
            ctx,
        }
    }

    pub fn add_block(&self) -> BasicBlock<'ctx> {
        self.ctx.inner.append_basic_block(self.func, "")
    }

    pub fn switch_to_block(&self, block: BasicBlock<'ctx>) {
        self.inner.position_at_end(block);
    }

    pub fn unreachable(&self) {
        self.inner.build_unreachable().unwrap();
    }

    pub fn return_any(&self, value: Option<&dyn BasicValue<'ctx>>) {
        if let Some(value) = value {
            self.return_value(value);
        } else {
            self.return_void();
        }
    }

    pub fn return_value(&self, value: &dyn BasicValue<'ctx>) {
        self.inner.build_return(Some(value)).unwrap();
    }

    pub fn return_void(&self) {
        self.inner.build_return(None).unwrap();
    }
}

struct FunctionLower<'ctx> {
    builder: Builder<'ctx>,
    func: &'ctx Function,
}

impl<'ctx> FunctionLower<'ctx> {
    pub fn new(builder: Builder<'ctx>, func: &'ctx Function) -> Self {
        Self { builder, func }
    }

    pub fn build(&self) {
        let block = self.builder.add_block();
        self.builder.switch_to_block(block);
        self.builder.return_void();
    }
}
