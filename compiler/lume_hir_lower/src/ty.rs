use lume_hir::SELF_TYPE_NAME;

use crate::*;

impl LoweringContext<'_> {
    #[tracing::instrument(level = "DEBUG", skip_all)]
    pub(crate) fn hir_type(&mut self, ty: lume_ast::Type) -> lume_hir::Type {
        match ty {
            lume_ast::Type::NamedType(t) => self.named_type(t),
            lume_ast::Type::ArrayType(t) => self.array_type(t),
            lume_ast::Type::SelfType(t) => self.alloc_self_type(self.location(t.location())),
        }
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    pub(crate) fn type_or_void(&mut self, ty: Option<lume_ast::Type>) -> lume_hir::Type {
        match ty {
            Some(e) => self.hir_type(e),
            None => lume_hir::Type::void(),
        }
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn named_type(&mut self, expr: lume_ast::NamedType) -> lume_hir::Type {
        // If there is currently a visible type parameter with the same name, attempt to
        // use it's ID...
        let id = if let Some(path) = expr.path()
            && !path.has_root()
        {
            self.existing_type_id_or_new(path.as_text().trim())
        } else {
            // ...otherwise, just generate a new ID.
            lume_hir::TypeId::from(self.next_node_id())
        };

        let name = if let Some(path) = expr.path() {
            self.resolve_symbol_name(path)
        } else {
            lume_hir::Path::missing()
        };

        let hir_type = lume_hir::Type {
            id,
            name,
            self_type: false,
            location: self.location(expr.location()),
        };

        self.map.types.insert(hir_type.id, hir_type.clone());
        hir_type
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn array_type(&mut self, ty: lume_ast::ArrayType) -> lume_hir::Type {
        let id = self.next_node_id();

        let mut name = lume_hir::hir_std_type_path!(Array);
        let elemental_type = self.type_or_void(ty.elemental());
        name.name.place_bound_types(vec![elemental_type]);

        let hir_type = lume_hir::Type {
            id: lume_hir::TypeId::from(id),
            name,
            self_type: false,
            location: self.location(ty.location()),
        };

        self.map.types.insert(hir_type.id, hir_type.clone());
        hir_type
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    pub(crate) fn alloc_self_type(&mut self, location: Location) -> lume_hir::Type {
        let id = self.next_node_id();
        let name = if let Some(ty) = &self.self_type {
            ty.clone()
        } else {
            self.dcx.emit_and_push(
                crate::errors::InvalidSelfParameter {
                    source: self.current_file().clone(),
                    range: location.index.clone(),
                    ty: String::from(SELF_TYPE_NAME),
                }
                .into(),
            );

            lume_hir::Path::missing()
        };

        let hir_type = lume_hir::Type {
            id: lume_hir::TypeId::from(id),
            name,
            self_type: true,
            location,
        };

        self.map.types.insert(hir_type.id, hir_type.clone());
        hir_type
    }
}
