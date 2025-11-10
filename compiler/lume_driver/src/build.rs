use lume_errors::{MapDiagnostic, SimpleDiagnostic};
use lume_metadata::PackageMetadata;
use lume_tir_lower::StaticMetadata;

use crate::*;

impl Driver {
    /// Locates the [`Package`] from the given path and builds it into an
    /// executable.
    ///
    /// # Errors
    ///
    /// Returns `Err` if:
    /// - the given path has no `Arcfile` within it,
    /// - an error occured while compiling the package,
    /// - an error occured while writing the output executable
    /// - or some unexpected error occured which hasn't been handled gracefully.
    #[allow(clippy::needless_pass_by_value)]
    pub fn build_package(root: &Path, opts: Options, dcx: DiagCtxHandle) -> Result<CompiledExecutable> {
        let driver = Self::from_root(root, dcx.clone())?;

        driver.build(opts)
    }

    /// Builds the given compiler state into an executable.
    ///
    /// # Errors
    ///
    /// Returns `Err` if:
    /// - an error occured while compiling the package,
    /// - an error occured while writing the output executable
    /// - or some unexpected error occured which hasn't been handled gracefully.
    #[tracing::instrument(skip_all, fields(root = %self.package.path.display()), err)]
    pub fn build(mut self, mut options: Options) -> Result<CompiledExecutable> {
        self.override_root_sources(&mut options);

        let session = Session {
            dep_graph: self.dependencies.clone(),
            workspace_root: self.package.path.clone(),
            options,
        };

        let gcx = Arc::new(GlobalCtx::new(session, self.dcx.to_context()));
        let mut dependencies = gcx.session.dep_graph.iter().cloned().collect::<Vec<_>>();

        // Build all the dependencies of the package in reverse, so all the
        // dependencies without any sub-dependencies can be built first.
        dependencies.reverse();

        let mut merged_map = lume_mir::ModuleMap::new(
            self.package.clone(),
            gcx.session.options.clone(),
            StaticMetadata::default(),
        );

        let mut dependency_hir = lume_hir::map::Map::empty(PackageId::empty());

        for dependency in dependencies {
            let compiled = Compiler::build_package(dependency, gcx.clone(), &dependency_hir)?;
            let metadata = compiled_pkg_metadata(&compiled);

            for (node_id, node) in &metadata.hir.nodes {
                dependency_hir.nodes.insert(*node_id, node.clone());
            }

            write_metadata_object(&gcx, &metadata)?;

            compiled.mir.merge_into(&mut merged_map);
        }

        let output_file_path = gcx.binary_output_path(&self.package.name);
        lume_fuse::fuse_binary_file(&gcx, merged_map, &output_file_path)?;

        Ok(CompiledExecutable {
            binary: output_file_path,
        })
    }
}

/// Writes the serialized representation of `metadata` to disk within the
/// metadata directory defined by `gcx` (via
/// [`GlobalCtx::obj_metadata_path()`]).
fn write_metadata_object(gcx: &Arc<GlobalCtx>, metadata: &PackageMetadata) -> Result<()> {
    // Ensure the parent directory exists first.
    let metadata_directory = gcx.obj_metadata_path();

    std::fs::create_dir_all(&metadata_directory).map_err(|err| {
        Box::new(
            SimpleDiagnostic::new(format!(
                "failed to create metadata directory ({})",
                metadata_directory.display()
            ))
            .add_cause(err),
        ) as lume_errors::Error
    })?;

    let metadata_filename = lume_metadata::metadata_filename_of(&metadata.header);
    let metadata_path = metadata_directory.join(metadata_filename);

    let serialized = postcard::to_allocvec(metadata).map_diagnostic()?;

    std::fs::write(metadata_path, serialized).map_err(|err| {
        Box::new(SimpleDiagnostic::new("failed to write metadata").add_cause(err)) as lume_errors::Error
    })?;

    Ok(())
}

pub struct CompiledPackage {
    /// Defines the type-checking context which was used under
    /// compilation of the package.
    tcx: TyCheckCtx,

    /// Defines the compiled MIR of the package.
    mir: lume_mir::ModuleMap,
}

fn compiled_pkg_metadata(pkg: &CompiledPackage) -> PackageMetadata {
    PackageMetadata::create(&pkg.mir.package, &pkg.tcx)
}

impl Compiler {
    /// Builds the [`Package`] with the given ID from the [`Project`] into an
    /// MIR map.
    ///
    /// # Errors
    ///
    /// Returns `Err` if:
    /// - an error occured while compiling the project,
    /// - or some unexpected error occured which hasn't been handled gracefully.
    #[tracing::instrument(skip_all, fields(package = %package.name), err)]
    pub fn build_package(
        package: Package,
        gcx: Arc<GlobalCtx>,
        dep_hir: &lume_hir::map::Map,
    ) -> Result<CompiledPackage> {
        let mut compiler = Self {
            package,
            gcx,
            source_map: SourceMap::default(),
        };

        let mut sources = compiler.parse()?;
        tracing::debug!(target: "driver", "finished parsing");

        for (node_id, node) in &dep_hir.nodes {
            sources.nodes.insert(*node_id, node.clone());
        }

        let (tcx, typed_ir) = compiler.type_check(sources)?;
        let mir = compiler.codegen(&tcx, typed_ir)?;

        Ok(CompiledPackage { tcx, mir })
    }
}
