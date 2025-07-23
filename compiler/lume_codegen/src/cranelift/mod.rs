use crate::{Backend, Context};

pub(crate) struct CraneliftBackend<'ctx> {
    context: Context<'ctx>,
}

impl<'ctx> CraneliftBackend<'ctx> {
    pub fn new(context: Context<'ctx>) -> Self {
        Self { context }
    }
}

impl<'ctx> Backend<'ctx> for CraneliftBackend<'ctx> {
    fn initialize(&mut self) -> lume_errors::Result<()> {
        Ok(())
    }

    fn generate(&mut self) -> lume_errors::Result<()> {
        Ok(())
    }
}
