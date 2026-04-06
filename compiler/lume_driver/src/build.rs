use crate::*;

impl<IO> Driver<IO>
where
    IO: FileLoader,
{
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
    pub fn build_package(root: &Path, config: Config<IO>, dcx: DiagCtxHandle) -> Result<CompiledExecutable> {
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

        let dry_run = self.config.dry_run;
        let package_name = self.package.name.clone();

        let GeneratedCode { gcx, objects } = self
            .to_pipeline()
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

                    if gcx.session.options.enable_incremental && !dry_run {
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

        let output_file_path = gcx.binary_output_path(&package_name);

        if !dry_run {
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
