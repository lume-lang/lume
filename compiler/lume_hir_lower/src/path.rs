use error_snippet::Result;

use crate::LowerModule;
use crate::errors::*;

impl LowerModule {
    /// Lowers an import path to an HIR path.
    ///
    /// For example, the import path `std::io (File)` would become
    /// `std::io::File`.
    #[libftrace::traced(level = Debug)]
    pub(super) fn import_path(&mut self, path: lume_ast::ImportPath) -> Result<lume_hir::Path> {
        let location = self.location(path.location);

        let Some((name, root)) = path.path.split_last() else {
            return Err(InvalidNamespacePath {
                source: self.file.clone(),
                range: location.index.clone(),
                path: Box::new(path.path),
            }
            .into());
        };

        let name = lume_hir::PathSegment::namespace(self.identifier(name.clone()));

        let root = root
            .iter()
            .map(|r| lume_hir::PathSegment::namespace(self.identifier(r.clone())))
            .collect();

        Ok(lume_hir::Path { name, root, location })
    }
}
