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

        let GeneratedCode { gcx, objects } = pipeline(gcx)
            .lower_to_hir()?
            .type_check()?
            .lower_to_tir()?
            .lower_to_mir()?
            .codegen()?;

        let mut linker_objects = Vec::with_capacity(objects.len());

        for (package_id, stage_result) in objects {
            match stage_result {
                StageResult::Value(object) => {
                    let GeneratedObject { name, object, metadata } = *object;

                    linker_objects.push(lume_linker::ObjectSource::Compiled { name, data: object });

                    if gcx.session.options.enable_incremental && !self.config.dry_run {
                        lume_metadata::write_metadata_object(gcx.obj_metadata_path(), &metadata)?;
                    }
                }
                StageResult::Cached { bc_path } => {
                    let package_name = gcx.package_name(package_id).unwrap();

                    linker_objects.push(lume_linker::ObjectSource::Cache {
                        name: package_name.into(),
                        path: bc_path,
                    });
                }
            }
        }

        let output_file_path = gcx.binary_output_path(&self.package.name);

        if !self.config.dry_run {
            let span = tracing::info_span!(
                "link_executable",
                output.path = %output_file_path.display()
            );

            span.in_scope(|| -> Result<()> {
                let object_files = lume_linker::write_object_files(&gcx, linker_objects)?;
                lume_linker::link_objects(object_files, &output_file_path, &gcx.session.options)?;

                Ok(())
            })?;
        }

        Ok(CompiledExecutable {
            binary: output_file_path,
        })
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
