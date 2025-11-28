use error_snippet::Result;
use lume_ast::Node;
use lume_hir::SELF_TYPE_NAME;

use crate::errors::*;
use crate::{ARRAY_STD_TYPE, LowerModule};

impl LowerModule<'_> {
    #[libftrace::traced(level = Debug)]
    pub(super) fn type_ref(&mut self, expr: lume_ast::Type) -> Result<lume_hir::Type> {
        match expr {
            lume_ast::Type::Named(t) => self.type_named(*t),
            lume_ast::Type::Array(t) => self.type_array(*t),
            lume_ast::Type::SelfType(t) => self.type_self(*t),
        }
    }

    #[libftrace::traced(level = Debug)]
    pub(super) fn opt_type_ref(&mut self, expr: Option<lume_ast::Type>) -> Result<lume_hir::Type> {
        match expr {
            Some(e) => Ok(self.type_ref(e)?),
            None => Ok(lume_hir::Type::void()),
        }
    }

    #[libftrace::traced(level = Debug)]
    fn type_named(&mut self, expr: lume_ast::NamedType) -> Result<lume_hir::Type> {
        // If there is currently a visible type parameter with the same name, attempt to
        // use it's ID...
        let id = if expr.name.root.is_empty()
            && let Some(id) = self.id_of_type_param(expr.name.name.name().as_str())
        {
            id
        } else {
            // ...otherwise, just generate a new ID.
            lume_hir::TypeId::from(self.next_node_id())
        };

        let name = self.resolve_symbol_name(&expr.name)?;
        let location = self.location(expr.location().clone());

        Ok(lume_hir::Type { id, name, location })
    }

    #[libftrace::traced(level = Debug)]
    fn type_array(&mut self, expr: lume_ast::ArrayType) -> Result<lume_hir::Type> {
        self.type_std(lume_ast::PathSegment::Type {
            name: ARRAY_STD_TYPE.into(),
            bound_types: vec![*expr.element_type],
            location: expr.location,
        })
    }

    #[libftrace::traced(level = Debug)]
    fn type_self(&mut self, expr: lume_ast::SelfType) -> Result<lume_hir::Type> {
        let id = self.next_node_id();
        let location = self.location(expr.location);
        let name = match &self.self_type {
            Some(ty) => ty.clone(),
            None => {
                return Err(SelfOutsideClass {
                    source: self.file.clone(),
                    range: location.index.clone(),
                    ty: String::from(SELF_TYPE_NAME),
                }
                .into());
            }
        };

        Ok(lume_hir::Type {
            id: lume_hir::TypeId::from(id),
            name,
            location,
        })
    }

    #[libftrace::traced(level = Debug)]
    fn type_std(&mut self, name: lume_ast::PathSegment) -> Result<lume_hir::Type> {
        let id = self.next_node_id();
        let location = self.location(name.location().clone());

        let name = self.path_segment(name)?;
        let path = lume_hir::Path::from_parts(Some(vec![lume_hir::PathSegment::namespace("std")]), name);

        Ok(lume_hir::Type {
            id: lume_hir::TypeId::from(id),
            name: path,
            location,
        })
    }
}
