use error_snippet::Result;
use lume_hir::WithLocation;
use lume_span::Internable;

use crate::LowerModule;
use crate::errors::*;

impl LowerModule {
    pub(crate) fn identifier(&self, expr: lume_ast::Identifier) -> lume_hir::Identifier {
        let location = self.location(expr.location.clone());

        lume_hir::Identifier {
            name: expr.name,
            location,
        }
    }

    pub(crate) fn expand_name(&mut self, name: lume_ast::PathSegment) -> Result<lume_hir::Path> {
        if let Some(ns) = &self.namespace {
            Ok(lume_hir::Path::with_root(ns.clone(), self.path_segment(name)?))
        } else {
            Ok(lume_hir::Path::rooted(self.path_segment(name)?))
        }
    }

    pub(crate) fn path(&mut self, path: lume_ast::Path) -> Result<lume_hir::Path> {
        let root = self.path_root(path.root)?;
        let name = self.path_segment(path.name)?;

        let mut location = name.location().clone_inner();

        location.index.start = root
            .iter()
            .map(|r| r.name().location.start())
            .min()
            .unwrap_or(location.index.start);

        Ok(lume_hir::Path {
            root,
            name,
            location: location.intern(),
        })
    }

    pub(crate) fn path_root(&mut self, expr: Vec<lume_ast::PathSegment>) -> Result<Vec<lume_hir::PathSegment>> {
        expr.into_iter()
            .map(|seg| self.path_segment(seg))
            .collect::<Result<Vec<_>>>()
    }

    pub(crate) fn path_segment(&mut self, expr: lume_ast::PathSegment) -> Result<lume_hir::PathSegment> {
        match expr {
            lume_ast::PathSegment::Namespace { name } => Ok(lume_hir::PathSegment::namespace(self.identifier(name))),
            lume_ast::PathSegment::Type {
                name,
                bound_types,
                location,
            } => {
                let name = self.identifier(name);
                let location = self.location(location);
                let bound_types = bound_types
                    .into_iter()
                    .map(|arg| self.type_ref(arg))
                    .collect::<Result<Vec<_>>>()?;

                Ok(lume_hir::PathSegment::Type {
                    name,
                    bound_types,
                    location,
                })
            }
            lume_ast::PathSegment::Callable {
                name,
                bound_types,
                location,
            } => {
                let name = self.identifier(name);
                let location = self.location(location);
                let bound_types = bound_types
                    .into_iter()
                    .map(|arg| self.type_ref(arg))
                    .collect::<Result<Vec<_>>>()?;

                Ok(lume_hir::PathSegment::Callable {
                    name,
                    bound_types,
                    location,
                })
            }
            lume_ast::PathSegment::Variant { name, location } => {
                let name = self.identifier(name);
                let location = self.location(location);

                Ok(lume_hir::PathSegment::Variant { name, location })
            }
        }
    }

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
