use lume_errors::Result;
use lume_mir::ModuleMap;
use lume_session::{Options, Package};

#[cfg(feature = "codegen_cranelift")]
mod cranelift;
#[cfg(feature = "codegen_llvm")]
mod llvm;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum BackendKind {
    #[cfg(feature = "codegen_cranelift")]
    Cranelift,
    #[cfg(feature = "codegen_llvm")]
    Llvm,
}

pub struct Generator<'ctx> {
    backend: Box<dyn Backend<'ctx> + 'ctx>,
}

impl<'ctx> Generator<'ctx> {
    /// Generates object files using the given backend.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the selected backend returned an error while generating object files.
    pub fn codegen(backend: BackendKind, package: &'ctx Package, mir: ModuleMap, options: &'ctx Options) -> Result<()> {
        let context = Context { package, mir, options };

        let backend: Box<dyn Backend<'_>> = match backend {
            #[cfg(feature = "codegen_cranelift")]
            BackendKind::Cranelift => Box::new(cranelift::CraneliftBackend::new(context)),
            #[cfg(feature = "codegen_llvm")]
            BackendKind::Llvm => Box::new(llvm::LlvmBackend::new(context)),
        };

        let mut generator = Generator { backend };

        generator.backend.initialize()?;
        generator.backend.generate()?;

        Ok(())
    }
}

pub(crate) struct Context<'ctx> {
    pub package: &'ctx Package,
    pub mir: lume_mir::ModuleMap,
    pub options: &'ctx Options,
}

pub(crate) trait Backend<'ctx> {
    /// Initializes the backend, if required.
    fn initialize(&mut self) -> Result<()>;

    /// Initializes the backend, if required.
    fn generate(&mut self) -> Result<()>;
}
