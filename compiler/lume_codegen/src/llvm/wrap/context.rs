use inkwell::{attributes::Attribute, values::FunctionValue};

use crate::llvm::{Builder, Module};

pub(crate) struct Context {
    pub(crate) inner: inkwell::context::Context,
}

impl Context {
    pub fn new() -> Self {
        Self {
            inner: inkwell::context::Context::create(),
        }
    }

    pub fn create_builder<'ctx>(&'ctx self, func_value: FunctionValue<'ctx>) -> Builder<'ctx> {
        Builder::new(self, func_value)
    }

    pub fn create_module(&self, name: &str) -> Module<'_> {
        Module::new(self, name)
    }

    pub fn attribute_flag(&self, name: &str) -> Attribute {
        self.inner
            .create_enum_attribute(Attribute::get_named_enum_kind_id(name), 0)
    }
}
