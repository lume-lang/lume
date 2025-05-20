use crate::{LowerModule, err, errors::*};

use error_snippet::Result;
use lume_ast::{self as ast};
use lume_hir::{self as hir};

impl LowerModule<'_> {
    /// Lowers an import path to an HIR path.
    ///
    /// For example, the import path `std::io (File)` would become `std::io::File`.
    pub(super) fn import_path(&mut self, path: ast::ImportPath) -> Result<hir::Path> {
        let location = self.location(path.location);

        let Some((name, root)) = path.path.split_last() else {
            return Err(err!(self, location, InvalidNamespacePath, path, Box::new(path.path)));
        };

        let name = Into::<hir::PathSegment>::into(self.identifier(name.clone()));

        let root = root
            .iter()
            .map(|r| Into::<hir::PathSegment>::into(self.identifier(r.clone())))
            .collect();

        Ok(hir::Path { name, root })
    }
}
