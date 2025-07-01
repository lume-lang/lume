use crate::{LowerModule, err, errors::*};

use error_snippet::Result;

impl LowerModule<'_> {
    /// Lowers an import path to an HIR path.
    ///
    /// For example, the import path `std::io (File)` would become `std::io::File`.
    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    pub(super) fn import_path(&mut self, path: lume_ast::ImportPath) -> Result<lume_hir::Path> {
        let location = self.location(path.location);

        let Some((name, root)) = path.path.split_last() else {
            return Err(err!(self, location, InvalidNamespacePath, path, Box::new(path.path)));
        };

        let name = lume_hir::PathSegment::namespace(self.identifier(name.clone()));

        let root = root
            .iter()
            .map(|r| lume_hir::PathSegment::namespace(self.identifier(r.clone())))
            .collect();

        Ok(lume_hir::Path { name, root, location })
    }
}
