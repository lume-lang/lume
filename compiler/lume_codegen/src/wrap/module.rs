use std::{collections::HashMap, sync::RwLock};

use inkwell::{
    attributes::AttributeLoc,
    values::{FunctionValue, GlobalValue},
};
use lume_mir::FunctionId;

use crate::{Context, Function};

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

    pub(crate) fn print_to_stdout(&self) {
        self.inner.print_to_stderr();
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
