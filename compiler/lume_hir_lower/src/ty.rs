use error_snippet::Result;
use lume_ast::Node;
use lume_hir::SELF_TYPE_NAME;

use crate::errors::*;
use crate::{ARRAY_STD_TYPE, LowerModule};

impl LowerModule<'_> {
    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    pub(super) fn type_ref(&self, expr: lume_ast::Type) -> Result<lume_hir::Type> {
        match expr {
            lume_ast::Type::Named(t) => self.type_named(*t),
            lume_ast::Type::Array(t) => self.type_array(*t),
            lume_ast::Type::SelfType(t) => self.type_self(*t),
        }
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    pub(super) fn opt_type_ref(&self, expr: Option<lume_ast::Type>) -> Result<lume_hir::Type> {
        match expr {
            Some(e) => Ok(self.type_ref(e)?),
            None => Ok(lume_hir::Type::void()),
        }
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn type_named(&self, expr: lume_ast::NamedType) -> Result<lume_hir::Type> {
        let name = self.resolve_symbol_name(&expr.name)?;
        let location = self.location(expr.location().clone());

        let id = lume_span::NodeId::from_name(self.current_node.package, &name);

        Ok(lume_hir::Type { id, name, location })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn type_array(&self, expr: lume_ast::ArrayType) -> Result<lume_hir::Type> {
        self.type_std(lume_ast::PathSegment::Type {
            name: ARRAY_STD_TYPE.into(),
            type_arguments: vec![*expr.element_type],
            location: expr.location,
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn type_self(&self, expr: lume_ast::SelfType) -> Result<lume_hir::Type> {
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

        let id = lume_span::NodeId::from_name(self.current_node.package, &name);

        Ok(lume_hir::Type { id, name, location })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn type_std(&self, name: lume_ast::PathSegment) -> Result<lume_hir::Type> {
        let id = lume_span::NodeId::from_name(lume_span::PackageId::empty(), &name);
        let location = self.location(name.location().clone());

        let name = self.path_segment(name)?;
        let path = lume_hir::Path::from_parts(Some(vec![lume_hir::PathSegment::namespace("std")]), name);

        Ok(lume_hir::Type {
            id,
            name: path,
            location,
        })
    }
}
