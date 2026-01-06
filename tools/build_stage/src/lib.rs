pub mod package;

use std::path::PathBuf;
use std::sync::Arc;

use lume_errors::{DiagCtx, Result};
use lume_hir::map::Map;
use lume_infer::TyInferCtx;
use lume_mir::ModuleMap;
use lume_session::{DependencyMap, GlobalCtx, Options, Package, Session};
use lume_span::SourceMap;
use lume_tir::TypedIR;
use lume_typech::TyCheckCtx;
use lume_types::TyCtx;
pub use package::PackageBuilder;

/// Creates a new stub [`Package`] with all default options.
pub fn stub_package() -> Package {
    Package::default()
}

/// Creates a new stub [`Package`] with all default options.
///
/// The package is passed to the given callback, which can be used to add source
/// files, set dependencies, etc.
pub fn stub_package_with<F: FnOnce(&mut Package)>(f: F) -> Package {
    let mut pkg = stub_package();
    f(&mut pkg);

    pkg
}

pub struct ManifoldDriver {
    /// Defines the package to compile.
    package: Package,

    /// Defines the global compilation context which is used across all compiler
    /// stages.
    gcx: Arc<GlobalCtx>,
}

impl ManifoldDriver {
    /// Creates a new manifold driver from the given package.
    pub fn new(package: Package, dcx: DiagCtx) -> Self {
        let mut dependency_map = DependencyMap::default();
        dependency_map.packages.insert(package.id, package.clone());

        let session = Session {
            dep_graph: dependency_map,
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
            let mut lower = lume_hir_lower::LowerState::new(&self.package, &mut source_map, dcx);

            lower.lower_into()
        })
    }

    /// Infers the types of all expressions and statements within the source
    /// [`Package`].
    pub fn type_inference(&self) -> Result<TyInferCtx> {
        let hir = self.build_hir()?;
        let tcx = TyCtx::new(self.gcx.clone());

        let mut infer_ctx = TyInferCtx::new(tcx, hir);
        infer_ctx.infer()?;

        lume_unification::unify(&mut infer_ctx)?;

        Ok(infer_ctx)
    }

    /// Type checks the types of all expressions and statements within the
    /// source [`Package`].
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

        let package = self.package.clone();
        let opts = self.gcx.session.options.clone();

        let mut transformer = lume_mir_lower::ModuleTransformer::create(package, &tcx, tir.metadata, opts);
        let mir = transformer.transform(&tir.functions.into_values().collect::<Vec<_>>());

        Ok(mir)
    }

    /// Compiles the MIR map from the current [`ManifoldDriver`] instance
    /// in-memory.
    pub fn compile(&self) -> Result<()> {
        let mir = self.build_mir()?;
        lume_codegen::generate(mir).unwrap();

        Ok(())
    }

    /// Compiles  from the current [`ManifoldDriver`] instance.
    pub fn link(&self) -> Result<PathBuf> {
        let mir = self.build_mir()?;

        let object_data = lume_codegen::generate(mir)?;
        let output_file_path = self.gcx.binary_output_path(&self.package.name);
        let object_file = lume_linker::write_object_files(&self.gcx, vec![lume_linker::ObjectSource::Compiled {
            name: self.package.name.clone(),
            data: object_data,
        }])?;

        lume_linker::link_objects(object_file, &output_file_path, &self.gcx.session.options)?;

        Ok(output_file_path)
    }
}
