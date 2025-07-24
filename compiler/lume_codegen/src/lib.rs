use std::path::PathBuf;

use lume_errors::Result;
use lume_mir::ModuleMap;
use lume_session::{Options, Package};

#[cfg(feature = "codegen_cranelift")]
mod cranelift;
#[cfg(feature = "codegen_llvm")]
mod llvm;

#[derive(Default, Clone)]
pub struct CodegenResult {
    pub modules: Vec<CompiledModule>,
}

#[derive(Clone)]
pub struct CompiledModule {
    pub name: String,
    pub bytecode: Vec<u8>,
}

#[derive(Default, Clone)]
pub struct CodegenObjects {
    pub objects: Vec<CodegenObject>,
}

#[derive(Clone)]
pub struct CodegenObject {
    pub name: String,
    pub path: PathBuf,
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
    pub fn codegen(package: &'ctx Package, mir: ModuleMap, options: &'ctx Options) -> Result<CompiledModule> {
        let context = Context { package, mir, options };

        let backend: Box<dyn Backend<'_>> = match context.options.backend {
            #[cfg(feature = "codegen_cranelift")]
            lume_session::Backend::Cranelift => Box::new(cranelift::CraneliftBackend::new(context)?),
            #[cfg(feature = "codegen_llvm")]
            lume_session::Backend::Llvm => Box::new(llvm::LlvmBackend::new(context)),
            _ => {
                return Err(error_snippet::SimpleDiagnostic::new(format!(
                    "selected backend not enabled in build: {}",
                    context.options.backend
                ))
                .into());
            }
        };

        let mut generator = Generator { backend };

        generator.backend.initialize()?;
        generator.backend.generate()
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
    fn generate(&mut self) -> Result<CompiledModule>;
}
