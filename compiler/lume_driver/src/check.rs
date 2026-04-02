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
    pub fn check_package(root: &Path, config: Config, dcx: DiagCtxHandle) -> Result<()> {
        let driver = Self::from_root(root, config, dcx.clone())?;

        if let Err(err) = driver.check() {
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
    #[tracing::instrument(level = "INFO", skip_all, fields(package = %self.package.path.display()), err)]
    pub fn check(mut self) -> Result<CheckedPackageGraph> {
        self.override_root_sources();

        let session = Session {
            dep_graph: self.dependencies.clone(),
            workspace_root: self.package.path.clone(),
            options: self.config.options,
            loader: self.config.loader,
        };

        let gcx = Arc::new(GlobalCtx::new(session, self.dcx.to_context()));
        let mut graph = CheckedPackageGraph::new(gcx.session.dep_graph.root);

        let TypeChecked { gcx, ctx } = pipeline(gcx).lower_to_hir()?.type_check()?;

        for (package_id, tcx) in ctx {
            let StageResult::Value(tcx) = tcx else {
                continue;
            };

            graph.packages.insert(package_id, CheckedPackage {
                package: gcx.package(package_id).unwrap().clone(),
                tcx: *tcx,
            });
        }

        Ok(graph)
    }
}

#[derive(Default)]
pub struct CheckedPackageGraph {
    /// Defines the ID of the root [`Package`] within the graph.
    pub root: PackageId,

    /// Defines a map of all checked packages, keyed by their ID.
    pub packages: HashMap<PackageId, CheckedPackage>,
}

impl CheckedPackageGraph {
    pub fn new(root: PackageId) -> Self {
        Self {
            root,
            packages: HashMap::new(),
        }
    }

    pub fn root_package(&self) -> &CheckedPackage {
        self.packages.get(&self.root).unwrap()
    }
}

pub struct CheckedPackage {
    /// Defines the checked [`Package`].
    pub package: Package,

    /// Defines the checked type context.
    pub tcx: TyCheckCtx,
}
