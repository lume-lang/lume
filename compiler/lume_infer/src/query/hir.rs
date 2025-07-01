use error_snippet::Result;
use lume_query::cached_query;
use lume_span::{DefId, ItemId};

use crate::TyInferCtx;

use super::diagnostics;

/// An iterator over the elements of a linked [`DefId`]s.
///
/// This `struct` is created by [`TyInferCtx::hir_parent_id_iter()`].
pub struct ParentHirIterator<'a> {
    tcx: &'a TyInferCtx,
    current: Option<DefId>,
}

impl Iterator for ParentHirIterator<'_> {
    type Item = DefId;

    fn next(&mut self) -> Option<Self::Item> {
        let item = self.current;

        self.current = match self.current {
            Some(c) => self.tcx.hir_parent_of(c),
            None => None,
        };

        item
    }
}

impl TyInferCtx {
    /// Returns the [`lume_hir::Item`] with the given ID, if any.
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn hir_item(&self, id: ItemId) -> Option<&lume_hir::Item> {
        self.hir.items.get(&id)
    }

    /// Returns the [`lume_hir::Expression`] with the given ID, if any.
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn hir_expr(&self, id: lume_span::ExpressionId) -> Option<&lume_hir::Expression> {
        self.hir.expressions.get(&id)
    }

    /// Returns the [`lume_hir::Statement`] with the given ID, if any.
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn hir_stmt(&self, id: lume_span::StatementId) -> Option<&lume_hir::Statement> {
        self.hir.statements.get(&id)
    }

    /// Returns the [`lume_hir::Def`] with the given ID, if any.
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn hir_def(&'_ self, id: lume_span::DefId) -> Option<lume_hir::Def<'_>> {
        match id {
            lume_span::DefId::Item(id) => self.hir_item(id).map(lume_hir::Def::Item),
            lume_span::DefId::Statement(id) => self.hir_stmt(id).map(lume_hir::Def::Statement),
            lume_span::DefId::Expression(id) => self.hir_expr(id).map(lume_hir::Def::Expression),
        }
    }

    /// Returns the [`lume_hir::Item`] with the given ID, if any.
    ///
    /// # Panics
    ///
    /// Panics if no [`lume_hir::Item`] with the given ID was found.
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn hir_expect_item(&self, id: ItemId) -> &lume_hir::Item {
        match self.hir.items.get(&id) {
            Some(item) => item,
            None => panic!("expected HIR item with ID of {id:?}"),
        }
    }

    /// Returns the [`lume_hir::TypeDefinition`] with the given ID, if any.
    ///
    /// # Panics
    ///
    /// Panics if no [`lume_hir::TypeDefinition`] with the given ID was found.
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn hir_expect_type(&self, id: ItemId) -> &lume_hir::TypeDefinition {
        let lume_hir::Item::Type(ty) = self.hir_expect_item(id) else {
            panic!("expected HIR type with ID of {id:?}")
        };

        ty
    }

    /// Returns the [`lume_hir::Expression`] with the given ID, if any.
    ///
    /// # Panics
    ///
    /// Panics if no [`lume_hir::Expression`] with the given ID was found.
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn hir_expect_expr(&self, id: lume_span::ExpressionId) -> &lume_hir::Expression {
        match self.hir.expressions.get(&id) {
            Some(item) => item,
            None => panic!("expected HIR expression with ID of {id:?}"),
        }
    }

    /// Returns the [`lume_hir::Statement`] with the given ID, if any.
    ///
    /// # Panics
    ///
    /// Panics if no [`lume_hir::Statement`] with the given ID was found.
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn hir_expect_stmt(&self, id: lume_span::StatementId) -> &lume_hir::Statement {
        match self.hir.statements.get(&id) {
            Some(item) => item,
            None => panic!("expected HIR statement with ID of {id:?}"),
        }
    }

    /// Returns the [`lume_hir::Def`] with the given ID, if any.
    ///
    /// # Panics
    ///
    /// Panics if no [`lume_hir::Def`] with the given ID was found.
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn hir_expect_def(&'_ self, id: lume_span::DefId) -> lume_hir::Def<'_> {
        match self.hir_def(id) {
            Some(item) => item,
            None => panic!("expected HIR def with ID of {id:?}"),
        }
    }

    /// Returns the parent of the given HIR element, if any is found.
    #[cached_query]
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn hir_parent_of(&self, def: DefId) -> Option<DefId> {
        self.ancestry.get(&def).copied()
    }

    /// Returns an iterator for the IDs in the ancestry tree above
    /// the given HIR [`DefId`] node.
    pub fn hir_parent_id_iter(&self, def: DefId) -> ParentHirIterator<'_> {
        ParentHirIterator {
            tcx: self,
            current: Some(def),
        }
    }

    /// Returns the parent of the given HIR element, if any is found.
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn hir_parent_iter(&self, def: DefId) -> impl Iterator<Item = lume_hir::Def<'_>> {
        self.hir_parent_id_iter(def).map(move |id| self.hir_expect_def(id))
    }

    /// Returns all the type parameters available for the [`lume_hir::Def`] with the given ID.
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn hir_avail_type_params(&self, def: DefId) -> Vec<&'_ lume_hir::TypeParameter> {
        let mut acc = Vec::new();

        for parent in self.hir_parent_iter(def) {
            let lume_hir::Def::Item(item) = parent else {
                continue;
            };

            for type_param in item.type_parameters().iter() {
                acc.push(type_param);
            }
        }

        acc
    }

    /// Returns the expected return type within the context where the given [`DefId`] is
    /// defined. Walks the ancestor tree until a function or method is found and returns it's
    /// return type.
    ///
    /// # Errors
    ///
    /// If no matching ancestor is found, returns [`Err`].
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub fn hir_ctx_return_type(&self, def: DefId) -> Result<lume_types::TypeRef> {
        for parent in self.hir_parent_iter(def) {
            let lume_hir::Def::Item(item) = parent else {
                continue;
            };

            let return_type = match &item {
                lume_hir::Item::Method(method) => &method.return_type,
                lume_hir::Item::Function(func) => &func.return_type,
                _ => continue,
            };

            let type_params = self.hir_avail_type_params(def);
            let type_ref = self.mk_type_ref_generic(return_type, &type_params)?;

            return Ok(type_ref);
        }

        let location = self.hir_expect_def(def).location();

        Err(diagnostics::NoReturningAncestor { source: location }.into())
    }
}
