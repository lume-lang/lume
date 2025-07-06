use std::{collections::HashMap, sync::RwLock};

use inkwell::values::GlobalValue;

use crate::{Context, Function, FunctionLower};

pub(crate) struct Module<'ctx> {
    pub(crate) name: String,
    pub(crate) context: &'ctx Context,
    pub(crate) inner: inkwell::module::Module<'ctx>,
    pub(crate) strings: RwLock<HashMap<String, GlobalValue<'ctx>>>,
}

impl<'ctx> Module<'ctx> {
    pub fn new(context: &'ctx Context, name: &str) -> Self {
        let inner = context.inner.create_module(name);

        Self {
            name: name.to_owned(),
            context,
            inner,
            strings: RwLock::new(HashMap::new()),
        }
    }

    pub fn build(&self, funcs: &'ctx [Function]) {
        for func in funcs {
            let ret_ty = self.context.lower_fn_type(&func.parameters, &func.return_type, false);
            let fn_ty = self.inner.add_function(&func.name, ret_ty, None);

            let builder = self.context.create_builder(fn_ty);

            FunctionLower::lower(builder, func);
        }
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
}
