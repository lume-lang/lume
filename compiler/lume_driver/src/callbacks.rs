use std::path::Path;

use lume_session::{GlobalCtx, Package};

pub struct Callbacks<'io> {
    pub arc_event: &'io dyn Fn(ArcEvent<'_>),
    pub on_package_state_change: &'io dyn Fn(&GlobalCtx, PackageState<'_>),
}

impl Default for Callbacks<'_> {
    fn default() -> Self {
        Self {
            arc_event: &|_| {},
            on_package_state_change: &|_, _| {},
        }
    }
}

#[derive(Debug, Clone)]
pub enum ArcEvent<'gcx> {
    /// Arc has started to look for the root package within the given path.
    FindingRootPackage { root: &'gcx Path },

    /// All package manifests have been loaded.
    PackagesLoaded { graph: &'gcx lume_session::DependencyMap },
}

#[derive(Debug, Clone)]
pub enum PackageState<'gcx> {
    /// The package is cached and does not need to be compiled.
    CompilationCached { package: &'gcx Package },

    /// Compilation of the given package has started.
    CompilationStarted { package: &'gcx Package },
}
