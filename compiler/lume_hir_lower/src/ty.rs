use error_snippet::Result;
use lume_ast::Node;

use crate::ARRAY_STD_TYPE;
use crate::LowerModule;
use crate::errors::*;

use lume_ast::{self as ast};
use lume_hir::{self as hir, SELF_TYPE_NAME};

impl LowerModule<'_> {
    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    pub(super) fn type_ref(&mut self, expr: ast::Type) -> Result<hir::Type> {
        match expr {
            ast::Type::Named(t) => self.type_named(*t),
            ast::Type::Array(t) => self.type_array(*t),
            ast::Type::SelfType(t) => self.type_self(*t),
        }
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    pub(super) fn opt_type_ref(&mut self, expr: Option<ast::Type>) -> Result<hir::Type> {
        match expr {
            Some(e) => Ok(self.type_ref(e)?),
            None => Ok(hir::Type::void()),
        }
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn type_named(&mut self, expr: ast::NamedType) -> Result<hir::Type> {
        let id = self.next_def_id();
        let name = self.resolve_symbol_name(&expr.name)?;
        let location = self.location(expr.location().clone());

        Ok(hir::Type { id, name, location })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn type_array(&mut self, expr: ast::ArrayType) -> Result<hir::Type> {
        self.type_std(ast::PathSegment::Type {
            name: ARRAY_STD_TYPE.into(),
            type_arguments: vec![*expr.element_type],
            location: expr.location,
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn type_self(&mut self, expr: ast::SelfType) -> Result<hir::Type> {
        let id = self.next_def_id();
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

        Ok(hir::Type { id, name, location })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn type_std(&mut self, name: ast::PathSegment) -> Result<hir::Type> {
        let id = self.next_def_id();
        let location = self.location(name.location().clone());

        let name = self.path_segment(name)?;
        let path = hir::Path::from_parts(Some(vec![hir::PathSegment::namespace("std")]), name);

        Ok(hir::Type {
            id,
            name: path,
            location,
        })
    }
}
