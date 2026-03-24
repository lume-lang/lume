use crate::*;

impl LoweringContext<'_> {
    /// Lowers an import path to an HIR path.
    ///
    /// For example, the import path `std::io (File)` would become
    /// `std::io::File`.
    #[tracing::instrument(level = "DEBUG", skip_all)]
    pub(crate) fn expand_import_path(&mut self, path: lume_ast::ImportPath) -> lume_hir::Path {
        let names = path.path().collect::<Vec<_>>();
        let location = self.location(path.location());

        let Some((name, root)) = names.split_last() else {
            self.dcx.emit_and_push(
                crate::errors::InvalidNamespacePath {
                    source: self.current_file().clone(),
                    range: location.index.clone(),
                    path: path.syntax().to_string(),
                }
                .into(),
            );

            return lume_hir::Path::missing();
        };

        let name = lume_hir::PathSegment::namespace(self.ident(name.clone()));

        let root = root
            .iter()
            .map(|r| lume_hir::PathSegment::namespace(self.ident(r.clone())))
            .collect();

        lume_hir::Path { name, root, location }
    }

    /// Expands the given AST path segment to a full HIR path by prepending the
    /// current namespace.
    pub(crate) fn expand_to_path(&mut self, segment: lume_hir::PathSegment) -> lume_hir::Path {
        match self.namespace.as_ref() {
            Some(namespace) => lume_hir::Path::with_root(namespace.clone(), segment),
            None => lume_hir::Path::rooted(segment),
        }
    }

    /// Expands the given AST type name to a full HIR path by prepending the
    /// current namespace.
    pub(crate) fn expand_type_name<N: Into<lume_ast::Name>>(&mut self, name: Option<N>) -> lume_hir::Path {
        self.expand_to_path(lume_hir::PathSegment::ty(self.ident_opt(name.map(Into::into))))
    }

    /// Expands the given AST callable name to a full HIR path by prepending the
    /// current namespace.
    pub(crate) fn expand_callable_name<N: Into<lume_ast::Name>>(&mut self, name: Option<N>) -> lume_hir::Path {
        self.expand_to_path(lume_hir::PathSegment::callable(self.ident_opt(name.map(Into::into))))
    }

    pub(crate) fn expand_generic_name<N: Into<lume_ast::Name>, P: IntoIterator<Item = lume_ast::BoundType>>(
        &mut self,
        name: Option<N>,
        bound_types: Option<P>,
    ) -> lume_hir::Path {
        let name = self.ident_opt(name.map(Into::into));
        let location = name.location();

        let type_segment = lume_hir::PathSegment::Type {
            name,
            bound_types: match bound_types {
                Some(bound_types) => bound_types
                    .into_iter()
                    .map(|type_param| {
                        let ident = self.ident_opt(type_param.name());
                        let id = self.existing_type_id_or_new(&ident.name);

                        lume_hir::Type {
                            id,
                            name: lume_hir::Path::rooted(lume_hir::PathSegment::ty(ident.clone())),
                            self_type: false,
                            location: self.location(type_param.location()),
                        }
                    })
                    .collect(),
                None => Vec::new(),
            },
            location,
        };

        self.expand_to_path(type_segment)
    }

    /// Gets the [`lume_hir::Path`] for the item with the given name.
    pub(crate) fn resolve_symbol_name(&mut self, path: lume_ast::Path) -> Path {
        let Some((root, name)) = path.split_root() else {
            return Path::missing();
        };

        if root.first().is_some_and(|segment| segment.is_self_type()) {
            return self.resolve_self_name(path);
        }

        let location = self.location(path.location());

        if let Some(symbol) = self.resolve_imported_symbol(path) {
            return symbol.clone();
        }

        let has_namespace_root = root
            .iter()
            .any(|root| matches!(root, lume_ast::PathSegment::PathNamespace(_)));

        let mut new_root = if let Some(namespace) = &self.namespace
            && !has_namespace_root
        {
            namespace.clone().as_root()
        } else {
            Vec::new()
        };

        new_root.extend(self.path_segments(root.clone()));

        Path {
            root: new_root,
            name: self.path_segment(name.clone()),
            location,
        }
    }

    /// Gets the [`lume_hir::Path`] for the item with the given name.
    pub(crate) fn resolve_symbol_name_opt(&mut self, path: Option<lume_ast::Path>) -> Path {
        match path {
            Some(path) => self.resolve_symbol_name(path),
            None => Path::missing(),
        }
    }

    /// Gets the [`lume_hir::Path`] for the item with the given name, within the
    /// current parent type.
    fn resolve_self_name(&mut self, path: lume_ast::Path) -> Path {
        let Some((root, _name)) = path.split_root() else {
            return Path::missing();
        };

        debug_assert!(root.first().is_some_and(|seg| seg.is_self_type()));

        let mut selfless_path = self.path(path.clone());
        let self_segment = selfless_path.root.remove(0);

        if let Some(self_path) = self.self_type.clone() {
            return Path::join(self_path, selfless_path);
        }

        self.dcx.emit_and_push(
            errors::InvalidSelfParameter {
                source: self.current_file().clone(),
                range: self_segment.location().index.clone(),
                ty: String::from("Self"),
            }
            .into(),
        );

        Path::missing()
    }

    /// Attemps to resolve a [`lume_hir::Path`] for an imported symbol.
    pub(crate) fn resolve_imported_symbol(&mut self, path: lume_ast::Path) -> Option<Path> {
        let (root, name) = path.split_root()?;
        let name_as_str = name.name();

        for (import, symbol) in &self.imports {
            // Match against imported paths, which match the first segment of the imported
            // path.
            //
            // This handles situations where a subpath was imported, which is then
            // referenced later, such as:
            //
            // ```lume
            //     import std (io);
            //
            //     io::File::from_path("foo.txt");
            // ```
            if let Some(root_segment) = root.first() {
                if root_segment.name() == *import {
                    // Since we only matched a subset of the imported symbol,
                    // we need to merge the two paths together, so it forms a fully-qualified path.
                    let mut symbol_root: Vec<PathSegment> = symbol.root.clone();

                    for segment in &root {
                        symbol_root.push(self.path_segment(segment.clone()));
                    }

                    return Some(Path {
                        root: symbol_root,
                        name: self.path_segment(name.clone()),
                        location: self.location(path.location()),
                    });
                }
            }
            // Match against the the imported symbol directly, such as:
            //
            // ```lume
            //     import std::io (File);
            //
            //     File::from_path("foo.txt");
            // ```
            else if name_as_str == *import {
                let root = symbol.root.clone();

                return Some(Path {
                    root,
                    name: self.path_segment(name.clone()),
                    location: self.location(path.location()),
                });
            }
        }

        None
    }

    pub(crate) fn path(&mut self, path: lume_ast::Path) -> lume_hir::Path {
        let mut root = self.path_segments(path.path_segment().collect());
        let name = root.pop().unwrap();

        lume_hir::Path::new(root, name)
    }

    #[inline]
    pub(crate) fn path_segments(&mut self, segments: Vec<lume_ast::PathSegment>) -> Vec<lume_hir::PathSegment> {
        segments.into_iter().map(|seg| self.path_segment(seg)).collect()
    }

    pub(crate) fn path_segment(&mut self, segment: lume_ast::PathSegment) -> lume_hir::PathSegment {
        match segment {
            lume_ast::PathSegment::PathNamespace(segment) => {
                let name = self.ident_opt(segment.name());

                lume_hir::PathSegment::namespace(name)
            }
            lume_ast::PathSegment::PathType(segment) => {
                let name = self.ident_opt(segment.name());
                let location = self.location(segment.location());

                let bound_types = if let Some(bound_types) = segment.generic_args() {
                    bound_types.args().map(|arg| self.hir_type(arg)).collect::<Vec<_>>()
                } else {
                    Vec::new()
                };

                lume_hir::PathSegment::Type {
                    name,
                    bound_types,
                    location,
                }
            }
            lume_ast::PathSegment::PathCallable(segment) => {
                let name = self.ident_opt(segment.name());
                let location = self.location(segment.location());

                let bound_types = if let Some(bound_types) = segment.generic_args() {
                    bound_types.args().map(|arg| self.hir_type(arg)).collect::<Vec<_>>()
                } else {
                    Vec::new()
                };

                lume_hir::PathSegment::Callable {
                    name,
                    bound_types,
                    location,
                }
            }
            lume_ast::PathSegment::PathVariant(segment) => {
                let name = self.ident_opt(segment.name());
                let location = self.location(segment.location());

                lume_hir::PathSegment::Variant { name, location }
            }
        }
    }
}
