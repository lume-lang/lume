use crate::Context;

impl Context {
    pub fn void_type(&self) -> inkwell::types::VoidType<'_> {
        self.inner.void_type()
    }
}
