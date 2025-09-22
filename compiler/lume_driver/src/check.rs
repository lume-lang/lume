use crate::*;

impl Driver {
    /// Locates the [`Package`] from the given path and checks it for errors.
    ///
    /// # Errors
    ///
    /// Returns `Err` if:
    /// - the given path has no `Arcfile` within it,
    /// - an error occured while compiling the package,
    /// - or some unexpected error occured which hasn't been handled gracefully.
    #[allow(clippy::needless_pass_by_value)]
    pub fn check_package(root: &PathBuf, opts: Options, dcx: DiagCtxHandle) -> Result<()> {
        let driver = Self::from_root(root, dcx.clone())?;

        if let Err(err) = driver.check(opts) {
            dcx.emit_and_push(err);
        }

        Ok(())
    }

    /// Checks the packages within the current compiler state for errors.
    ///
    /// # Errors
    ///
    /// Returns `Err` if:
    /// - an error occured while compiling the package,
    /// - or some unexpected error occured which hasn't been handled gracefully.
    #[tracing::instrument(skip_all, fields(root = %self.package.path.display()), err)]
    pub fn check(mut self, options: Options) -> Result<CheckedPackageGraph> {
        let session = Session {
            dep_graph: std::mem::take(&mut self.package.dependencies.graph),
            workspace_root: self.package.path.clone(),
            options,
        };

        let gcx = Arc::new(GlobalCtx::new(session, self.dcx.to_context()));
        let mut dependencies = gcx.session.dep_graph.all();

        // Build all the dependencies of the package in reverse, so all the
        // dependencies without any sub-dependencies can be built first.
        dependencies.reverse();

        let mut graph = CheckedPackageGraph::default();

        for dependency in dependencies {
            let checked = Compiler::check_package(dependency, gcx.clone())?;

            graph.packages.insert(checked.package, checked);
        }

        Ok(graph)
    }
}

#[derive(Default)]
pub struct CheckedPackageGraph {
    /// Defines map of all checked packages, keyed by their ID.
    pub packages: HashMap<PackageId, CheckedPackage>,
}

pub struct CheckedPackage {
    /// Defines the ID of the checked [`Package`].
    pub package: PackageId,

    /// Defines the checked type context.
    pub tcx: TyCheckCtx,
}

impl<'a> Compiler<'a> {
    /// Checks the given [`Package`] for errors, such as parsing-, semantic- or configuration errors.
    ///
    /// # Errors
    ///
    /// Returns `Err` if:
    /// - an error occured while compiling the project,
    /// - or some unexpected error occured which hasn't been handled gracefully.
    #[tracing::instrument(skip_all, fields(package = %package.name), err)]
    pub fn check_package(package: &'a Package, gcx: Arc<GlobalCtx>) -> Result<CheckedPackage> {
        let mut compiler = Self {
            package,
            gcx,
            source_map: SourceMap::default(),
        };

        let sources = compiler.parse()?;
        tracing::debug!(target: "driver", "finished parsing");

        let (tcx, _) = compiler.type_check(sources)?;

        Ok(CheckedPackage {
            package: package.id,
            tcx,
        })
    }
}
