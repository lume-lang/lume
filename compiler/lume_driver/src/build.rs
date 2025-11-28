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
    #[libftrace::traced(level = Info, fields(root = self.package.path.display()))]
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

        let mut objects = Vec::new();
        let mut dependency_hir = lume_hir::map::Map::empty(PackageId::empty());

        for dependency in dependencies {
            if !crate::incremental::needs_compilation(&gcx, &dependency)? {
                objects.push(lume_linker::ObjectSource::Cache {
                    name: dependency.name.clone(),
                    path: gcx.obj_bc_path_of(&dependency.name),
                });

                continue;
            }

            libftrace::info!(
                "compiling {} v{} ({})",
                dependency.name,
                dependency.version,
                dependency.path.display()
            );

            let compiled = Compiler::build_package(dependency, gcx.clone(), &dependency_hir)?;
            let metadata = compiled_pkg_metadata(&compiled);

            for (node_id, node) in &metadata.hir.nodes {
                dependency_hir.nodes.insert(*node_id, node.clone());
            }

            let object = lume_codegen::generate(compiled.mir)?;
            objects.push(lume_linker::ObjectSource::Compiled {
                name: metadata.header.name.clone(),
                data: object,
            });

            crate::incremental::write_metadata_object(&gcx, &metadata)?;
        }

        let output_file_path = gcx.binary_output_path(&self.package.name);

        let object_files = lume_linker::write_object_files(&gcx, objects)?;
        lume_linker::link_objects(object_files, &output_file_path, &gcx.session.options)?;

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
    #[libftrace::traced(level = Info, fields(package = package.name))]
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
        libftrace::debug!("finished parsing");

        for (node_id, node) in &dep_hir.nodes {
            sources.nodes.insert(*node_id, node.clone());
        }

        let (tcx, typed_ir) = compiler.type_check(sources)?;
        let mir = compiler.codegen(&tcx, typed_ir)?;

        Ok(CompiledPackage { tcx, mir })
    }
}
