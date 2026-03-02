use crate::*;

impl LoweringContext<'_> {
    /// Lowers an import path to an HIR path.
    ///
    /// For example, the import path `std::io (File)` would become
    /// `std::io::File`.
    #[libftrace::traced(level = Debug)]
    pub(crate) fn expand_import_path(&mut self, path: lume_ast::ImportPath) -> Result<lume_hir::Path> {
        let location = self.location(path.location.clone());

        let Some((name, root)) = path.path.split_last() else {
            return Err(crate::errors::InvalidNamespacePath {
                source: self.current_file().clone(),
                range: location.index.clone(),
                path: path.to_string(),
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

    /// Expands the given AST path segment to a full HIR path by prepending the
    /// current namespace.
    pub(crate) fn expand_to_path(&mut self, segment: lume_ast::PathSegment) -> Result<lume_hir::Path> {
        match self.namespace.as_ref() {
            Some(namespace) => Ok(lume_hir::Path::with_root(
                namespace.clone(),
                self.path_segment(segment)?,
            )),
            None => Ok(lume_hir::Path::rooted(self.path_segment(segment)?)),
        }
    }

    /// Expands the given AST type name to a full HIR path by prepending the
    /// current namespace.
    pub(crate) fn expand_type_name<'ast, N: Into<lume_ast::Identifier<'ast>>>(
        &mut self,
        name: N,
    ) -> Result<lume_hir::Path> {
        self.expand_to_path(lume_ast::PathSegment::ty(name.into()))
    }

    /// Expands the given AST callable name to a full HIR path by prepending the
    /// current namespace.
    pub(crate) fn expand_callable_name<'ast, N: Into<lume_ast::Identifier<'ast>>>(
        &mut self,
        name: N,
    ) -> Result<lume_hir::Path> {
        self.expand_to_path(lume_ast::PathSegment::callable(name.into()))
    }

    pub(crate) fn expand_generic_name<
        'ast,
        N: Into<lume_ast::Identifier<'ast>>,
        P: IntoIterator<Item = lume_ast::TypeParameter<'ast>>,
    >(
        &mut self,
        name: N,
        bound_types: P,
    ) -> Result<lume_hir::Path> {
        let name = name.into();
        let location = name.location.clone();

        self.expand_to_path(lume_ast::PathSegment::Type {
            name,
            bound_types: bound_types
                .into_iter()
                .map(|type_param| {
                    lume_ast::Type::Named(lume_ast::NamedType {
                        name: lume_ast::Path::rooted(lume_ast::PathSegment::ty(type_param.name)),
                    })
                })
                .collect(),
            location,
        })
    }

    pub(crate) fn path(&mut self, path: lume_ast::Path) -> Result<lume_hir::Path> {
        let root = self.path_segments(path.root)?;
        let name = self.path_segment(path.name)?;

        Ok(lume_hir::Path::new(root, name))
    }

    #[inline]
    pub(crate) fn path_segments(&mut self, segments: Vec<lume_ast::PathSegment>) -> Result<Vec<lume_hir::PathSegment>> {
        segments.into_iter().map(|seg| self.path_segment(seg)).collect()
    }

    pub(crate) fn path_segment(&mut self, segment: lume_ast::PathSegment) -> Result<lume_hir::PathSegment> {
        match segment {
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
                    .map(|arg| self.hir_type(arg))
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
                    .map(|arg| self.hir_type(arg))
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
}
