use indexmap::IndexSet;
use lume_metadata::PackageMetadata;

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
    pub fn build_package(root: &Path, config: Config, dcx: DiagCtxHandle) -> Result<CompiledExecutable> {
        let driver = Self::from_root(root, config, dcx.clone())?;

        driver.build()
    }

    /// Builds the given compiler state into an executable.
    ///
    /// # Errors
    ///
    /// Returns `Err` if:
    /// - an error occured while compiling the package,
    /// - an error occured while writing the output executable
    /// - or some unexpected error occured which hasn't been handled gracefully.
    #[tracing::instrument(level = "INFO", skip(self), fields(root = %self.package.path.display()), err)]
    pub fn build(mut self) -> Result<CompiledExecutable> {
        self.override_root_sources();

        let session = Session {
            dep_graph: self.dependencies.clone(),
            workspace_root: self.package.path.clone(),
            options: self.config.options,
            loader: self.config.loader,
        };

        let gcx = Arc::new(GlobalCtx::new(session, self.dcx.to_context()));
        let mut dependencies = gcx.session.dep_graph.iter().cloned().collect::<Vec<_>>();

        // Build all the dependencies of the package in reverse, so all the
        // dependencies without any sub-dependencies can be built first.
        dependencies.reverse();

        let mut objects = Vec::new();
        let mut dependency_hir = lume_hir::map::Map::empty(PackageId::empty());

        let mut dirty_packages: IndexSet<PackageId> = IndexSet::with_capacity(dependencies.len());

        for dependency in dependencies {
            let has_hash_changed = needs_compilation(&gcx, &dependency);
            let bc_path = gcx.obj_bc_path_of(&dependency.name);

            // We can skip recompilation of a package if *all* the following circumstances
            // are true:
            // - none of it's dependencies have been marked as "dirty",
            // - the package hash within the package's `.mlib` file is the same as now,
            // - and the target object file already exists
            if !dirty_packages.contains(&dependency.id) && !has_hash_changed && bc_path.exists() {
                objects.push(lume_linker::ObjectSource::Cache {
                    name: dependency.name.clone(),
                    path: bc_path,
                });

                continue;
            }

            // If the package hash is different from the cached hash, mark all the depending
            // packages as dirty.
            //
            // The check is meant to prevent the re-compilation in the case where the hash
            // is the same, but re-compilation of a dependency was required since the object
            // file was missing.
            if has_hash_changed {
                dirty_packages.extend(gcx.session.dep_graph.dependents_of(dependency.id));
            }

            let span = tracing::info_span!(
                "compile_package",
                dependency.name,
                %dependency.version,
                dependency.path = %dependency.path.display()
            );

            span.in_scope(|| -> Result<()> {
                let compiled = Compiler::build_package(dependency, gcx.clone(), &dependency_hir)?;
                let metadata = compiled_pkg_metadata(&compiled);

                let object = lume_codegen::generate(compiled.mir)?;
                objects.push(lume_linker::ObjectSource::Compiled {
                    name: metadata.header.name.clone(),
                    data: object,
                });

                if gcx.session.options.enable_incremental && !self.config.dry_run {
                    lume_metadata::write_metadata_object(gcx.obj_metadata_path(), &metadata)?;
                }

                metadata.hir.merge_into(&mut dependency_hir);

                Ok(())
            })?;
        }

        let output_file_path = gcx.binary_output_path(&self.package.name);

        if !self.config.dry_run {
            let span = tracing::info_span!(
                "link_executable",
                output.path = %output_file_path.display()
            );

            span.in_scope(|| -> Result<()> {
                let object_files = lume_linker::write_object_files(&gcx, objects)?;
                lume_linker::link_objects(object_files, &output_file_path, &gcx.session.options)?;

                Ok(())
            })?;
        }

        Ok(CompiledExecutable {
            binary: output_file_path,
        })
    }
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
    /// Builds the [`Package`] with the given ID from the [`Package`] into an
    /// MIR map.
    ///
    /// # Errors
    ///
    /// Returns `Err` if:
    /// - an error occured while compiling the project,
    /// - or some unexpected error occured which hasn't been handled gracefully.
    #[tracing::instrument(level = "INFO", skip_all, fields(package = %package.name), err)]
    pub fn build_package(
        package: Package,
        gcx: Arc<GlobalCtx>,
        dep_hir: &lume_hir::map::Map,
    ) -> Result<CompiledPackage> {
        let mut compiler = Self { package, gcx };

        let mut sources = compiler.parse()?;
        tracing::debug!("finished parsing");

        dep_hir.clone().merge_into(&mut sources);

        let (tcx, typed_ir) = compiler.type_check(sources)?;
        let mir = compiler.codegen(&tcx, typed_ir);

        Ok(CompiledPackage { tcx, mir })
    }
}

/// Determines whether the given package needs to be compiled or re-compiled.
///
/// This takes the state of the current package metadata into account, as well
/// as if anything has changed within it' source code.
#[tracing::instrument(level = "DEBUG", skip_all, fields(package = %package.name), ret)]
pub(crate) fn needs_compilation(gcx: &Arc<GlobalCtx>, package: &Package) -> bool {
    // If incremental compilation is disabled, we should alwas re-compile.
    if !gcx.session.options.enable_incremental {
        tracing::debug!("re-compilation required: incremental compilation disabled");
        return true;
    }

    let metadata_directory = gcx.obj_metadata_path();
    let metadata_filename = lume_metadata::metadata_filename_of(&package.name);
    let metadata_path = metadata_directory.join(metadata_filename);

    // If no metadata file could be found, the package has likely not been built
    // yet - in which case it obviously needs to be built.
    let Ok(Some(metadata)) = lume_metadata::read_metadata_header(metadata_path) else {
        tracing::debug!("re-compilation required: could not read metadata header");
        return true;
    };

    let current_hash = package.package_hash();

    #[allow(clippy::needless_bool, reason = "lint only raised when tracing is disabled")]
    if metadata.hash == current_hash {
        tracing::debug!("hash matched, compilation not required");

        false
    } else {
        tracing::debug!(
            message = "hash mismatch between packages",
            current = %current_hash,
            build = %metadata.hash
        );

        true
    }
}
