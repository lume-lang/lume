use lume_hir::SELF_TYPE_NAME;

use crate::*;

impl LoweringContext<'_> {
    #[libftrace::traced(level = Debug)]
    pub(crate) fn hir_type(&mut self, ty: lume_ast::Type) -> Result<lume_hir::Type> {
        match ty {
            lume_ast::Type::Named(t) => self.named_type(t),
            lume_ast::Type::Array(t) => self.array_type(t),
            lume_ast::Type::SelfType(t) => self.self_type(t),
        }
    }

    #[libftrace::traced(level = Debug)]
    pub(crate) fn hir_type_opt(&mut self, ty: Option<lume_ast::Type>) -> Result<lume_hir::Type> {
        match ty {
            Some(e) => Ok(self.hir_type(e)?),
            None => Ok(lume_hir::Type::void()),
        }
    }

    #[libftrace::traced(level = Debug)]
    fn named_type(&mut self, expr: lume_ast::NamedType) -> Result<lume_hir::Type> {
        // If there is currently a visible type parameter with the same name, attempt to
        // use it's ID...
        let id = if expr.name.root.is_empty()
            && let Some(id) = self.current_type_params.retrieve(expr.name.name.name().as_str())
        {
            *id
        } else {
            // ...otherwise, just generate a new ID.
            lume_hir::TypeId::from(self.next_node_id())
        };

        let name = self.resolve_symbol_name(&expr.name)?;

        let hir_type = lume_hir::Type {
            id,
            name,
            self_type: false,
            location: self.location(expr.location().clone()),
        };

        self.map.types.insert(hir_type.id, hir_type.clone());
        Ok(hir_type)
    }

    #[libftrace::traced(level = Debug)]
    fn array_type(&mut self, ty: lume_ast::ArrayType) -> Result<lume_hir::Type> {
        let id = self.next_node_id();

        let mut name = lume_hir::hir_std_type_path!(Array);
        let elemental_type = self.hir_type(*ty.element_type)?;
        name.name.place_bound_types(vec![elemental_type]);

        let hir_type = lume_hir::Type {
            id: lume_hir::TypeId::from(id),
            name,
            self_type: false,
            location: self.location(ty.location.clone()),
        };

        self.map.types.insert(hir_type.id, hir_type.clone());
        Ok(hir_type)
    }

    #[libftrace::traced(level = Debug)]
    fn self_type(&mut self, expr: lume_ast::SelfType) -> Result<lume_hir::Type> {
        let id = self.next_node_id();
        let location = self.location(expr.location);
        let name = match &self.self_type {
            Some(ty) => ty.clone(),
            None => {
                return Err(crate::errors::SelfOutsideObjectContext {
                    source: self.current_file().clone(),
                    range: location.index.clone(),
                    ty: String::from(SELF_TYPE_NAME),
                }
                .into());
            }
        };

        let hir_type = lume_hir::Type {
            id: lume_hir::TypeId::from(id),
            name,
            self_type: true,
            location,
        };

        self.map.types.insert(hir_type.id, hir_type.clone());
        Ok(hir_type)
    }
}
