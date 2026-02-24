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
    #[libftrace::traced(level = Info, fields(root = self.package.path.display()))]
    pub fn check(mut self) -> Result<CheckedPackageGraph> {
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

        let mut graph = CheckedPackageGraph {
            root: gcx.session.dep_graph.root,
            packages: HashMap::new(),
        };

        for dependency in dependencies {
            let checked = Compiler::check_package(dependency, gcx.clone())?;

            graph.packages.insert(checked.package, checked);
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
    pub fn root_package(&self) -> &CheckedPackage {
        self.packages.get(&self.root).unwrap()
    }
}

pub struct CheckedPackage {
    /// Defines the ID of the checked [`Package`].
    pub package: PackageId,

    /// Defines the checked type context.
    pub tcx: TyCheckCtx,
}

impl Compiler {
    /// Checks the given [`Package`] for errors, such as parsing-, semantic- or
    /// configuration errors.
    ///
    /// # Errors
    ///
    /// Returns `Err` if:
    /// - an error occured while compiling the project,
    /// - or some unexpected error occured which hasn't been handled gracefully.
    #[libftrace::traced(level = Info, fields(package = package.name))]
    pub fn check_package(package: Package, gcx: Arc<GlobalCtx>) -> Result<CheckedPackage> {
        let package_id = package.id;
        let mut compiler = Self {
            package,
            gcx,
            source_map: SourceMap::default(),
        };

        let sources = compiler.parse()?;
        libftrace::debug!("finished parsing");

        let (tcx, _) = compiler.type_check(sources)?;

        Ok(CheckedPackage {
            package: package_id,
            tcx,
        })
    }
}
