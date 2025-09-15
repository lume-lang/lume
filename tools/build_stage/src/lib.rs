use std::{path::PathBuf, sync::Arc};

use lume_codegen::CodegenResult;
use lume_errors::{DiagCtx, Result};
use lume_hir::map::Map;
use lume_infer::TyInferCtx;
use lume_mir::ModuleMap;
use lume_session::{DependencyGraph, GlobalCtx, Options, Package, Session};
use lume_span::SourceMap;
use lume_tir::TypedIR;
use lume_typech::TyCheckCtx;
use lume_types::TyCtx;

/// Creates a new stub [`Package`] with all default options.
pub fn stub_package() -> Package {
    Package::default()
}

/// Creates a new stub [`Package`] with all default options.
///
/// The package is passed to the given callback, which can be used to add source
/// files, set dependencies, etc.
pub fn stub_package_with<F: FnOnce(&mut Package)>(f: F) -> Package {
    let mut pkg = Package::default();
    f(&mut pkg);

    pkg
}

pub struct ManifoldDriver {
    /// Defines the package to compile.
    package: Package,

    /// Defines the global compilation context which is used across all compiler stages.
    gcx: Arc<GlobalCtx>,
}

#[expect(clippy::missing_errors_doc)]
impl ManifoldDriver {
    /// Creates a new manifold driver from the given package.
    pub fn new(package: Package, dcx: DiagCtx) -> Self {
        let session = Session {
            dep_graph: DependencyGraph::default(),
            workspace_root: package.path.clone(),
            options: Options::default(),
        };

        let gcx = Arc::new(GlobalCtx::new(session, dcx));

        Self { package, gcx }
    }

    /// Builds the HIR map from the current [`ManifoldDriver`] instance.
    pub fn build_hir(&self) -> Result<Map> {
        self.gcx.dcx.with(|dcx| {
            let mut source_map = SourceMap::default();

            lume_hir_lower::LowerState::lower(&self.package, &mut source_map, dcx)
        })
    }

    /// Infers the types of all expressions and statements within the source [`Package`].
    pub fn type_inference(&self) -> Result<TyInferCtx> {
        let hir = self.build_hir()?;
        let tcx = TyCtx::new(self.gcx.clone());

        let mut infer_ctx = TyInferCtx::new(tcx, hir);
        infer_ctx.infer()?;

        Ok(infer_ctx)
    }

    /// Type checks the types of all expressions and statements within the source [`Package`].
    pub fn type_check(&self) -> Result<TyCheckCtx> {
        let infer_ctx = self.type_inference()?;

        let mut check_ctx = TyCheckCtx::new(infer_ctx);
        check_ctx.typecheck()?;

        Ok(check_ctx)
    }

    /// Builds the TIR map from the current [`ManifoldDriver`] instance.
    pub fn build_tir(&self) -> Result<(TyCheckCtx, TypedIR)> {
        let check_ctx = self.type_check()?;
        let typed_ir = lume_tir_lower::Lower::build(&check_ctx)?;

        Ok((check_ctx, typed_ir))
    }

    /// Builds the MIR map from the current [`ManifoldDriver`] instance.
    pub fn build_mir(&self) -> Result<ModuleMap> {
        let (tcx, tir) = self.build_tir()?;
        let mir = lume_mir_lower::ModuleTransformer::transform(&tcx, tir);

        Ok(mir)
    }

    /// Compiles an object file from the current [`ManifoldDriver`] instance.
    pub fn compile(&self) -> Result<PathBuf> {
        let (tcx, tir) = self.build_tir()?;
        let mir = lume_mir_lower::ModuleTransformer::transform(&tcx, tir);

        let compiled_module = lume_codegen::generate(&self.package, mir, &self.gcx.session.options)?;
        let objects = lume_linker::write_object_files(
            &self.gcx,
            CodegenResult {
                modules: vec![compiled_module],
            },
        )?;

        let output_file_path = self.gcx.binary_output_path(&self.package.name);
        lume_linker::link_objects(&objects, &output_file_path, &self.gcx.session.options)?;

        Ok(output_file_path)
    }
}
