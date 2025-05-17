use error_snippet::Result;
use lume_types::SymbolName;

use crate::ARRAY_STD_TYPE;
use crate::LowerModule;
use crate::errors::*;

use lume_ast::{self as ast};
use lume_hir::{self as hir, SELF_TYPE_NAME};

impl LowerModule<'_> {
    pub(super) fn type_ref(&self, expr: ast::Type) -> Result<hir::Type> {
        match expr {
            ast::Type::Scalar(t) => Ok(self.type_scalar(*t)),
            ast::Type::Array(t) => self.type_array(*t),
            ast::Type::Generic(t) => self.type_generic(*t),
            ast::Type::SelfType(t) => self.type_self(*t),
        }
    }

    pub(super) fn opt_type_ref(&self, expr: Option<ast::Type>) -> Result<Option<hir::Type>> {
        match expr {
            Some(e) => Ok(Some(self.type_ref(e)?)),
            None => Ok(None),
        }
    }

    fn type_scalar(&self, expr: ast::ScalarType) -> hir::Type {
        let name = self.resolve_symbol_name(&expr.name);
        let location = self.location(expr.name.location);

        hir::Type {
            name,
            type_params: Vec::new(),
            location,
        }
    }

    fn type_array(&self, expr: ast::ArrayType) -> Result<hir::Type> {
        self.type_std(ARRAY_STD_TYPE, vec![*expr.element_type])
    }

    fn type_generic(&self, expr: ast::GenericType) -> Result<hir::Type> {
        let location = self.location(expr.location);
        let name = self.resolve_symbol_name(&expr.name);

        let type_params = expr
            .type_params
            .into_iter()
            .map(|c| self.type_ref(*c))
            .collect::<Result<Vec<hir::Type>>>()?;

        Ok(hir::Type {
            name,
            type_params: type_params.into_iter().map(Box::new).collect(),
            location,
        })
    }

    fn type_self(&self, expr: ast::SelfType) -> Result<hir::Type> {
        let location = self.location(expr.location);
        let name = match &self.self_type {
            Some(ty) => ty.clone(),
            None => {
                return Err(SelfOutsideClass {
                    source: self.file.clone(),
                    range: location.index,
                    ty: String::from(SELF_TYPE_NAME),
                }
                .into());
            }
        };

        Ok(hir::Type {
            name,
            type_params: Vec::new(),
            location,
        })
    }

    fn type_std(&self, name: &str, type_params: Vec<ast::Type>) -> Result<hir::Type> {
        let type_params = type_params
            .into_iter()
            .map(|ty| Ok(Box::new(self.type_ref(ty)?)))
            .collect::<Result<Vec<_>>>()?;

        Ok(hir::Type {
            name: SymbolName::from_parts(["std"], name),
            type_params,
            location: lume_span::Location::empty(),
        })
    }
}
