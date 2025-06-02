use error_snippet::Result;
use lume_query::cached_query;
use lume_span::{DefId, ItemId};

use crate::ThirBuildCtx;

use super::diagnostics;

/// An iterator over the elements of a linked [`DefId`]s.
///
/// This `struct` is created by [`ThirBuildCtx::hir_parent_id_iter()`].
pub(crate) struct ParentHirIterator<'a> {
    tcx: &'a ThirBuildCtx,
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

impl ThirBuildCtx {
    /// Returns the [`lume_hir::Item`] with the given ID, if any.
    pub fn hir_item(&self, id: ItemId) -> Option<&lume_hir::Item> {
        self.hir.items.get(&id)
    }

    /// Returns the [`lume_hir::Expression`] with the given ID, if any.
    pub fn hir_expr(&self, id: lume_span::ExpressionId) -> Option<&lume_hir::Expression> {
        self.hir.expressions.get(&id)
    }

    /// Returns the [`lume_hir::Statement`] with the given ID, if any.
    pub fn hir_stmt(&self, id: lume_span::StatementId) -> Option<&lume_hir::Statement> {
        self.hir.statements.get(&id)
    }

    /// Returns the [`lume_hir::Def`] with the given ID, if any.
    pub fn hir_def(&self, id: lume_span::DefId) -> Option<lume_hir::Def> {
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
    pub fn hir_expect_item(&self, id: ItemId) -> &lume_hir::Item {
        match self.hir.items.get(&id) {
            Some(item) => item,
            None => panic!("expected HIR item with ID of {id:?}"),
        }
    }

    /// Returns the [`lume_hir::Expression`] with the given ID, if any.
    ///
    /// # Panics
    ///
    /// Panics if no [`lume_hir::Expression`] with the given ID was found.
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
    pub fn hir_expect_def(&self, id: lume_span::DefId) -> lume_hir::Def {
        match self.hir_def(id) {
            Some(item) => item,
            None => panic!("expected HIR def with ID of {id:?}"),
        }
    }

    /// Returns the parent of the given HIR element, if any is found.
    #[cached_query]
    pub(crate) fn hir_parent_of(&self, def: DefId) -> Option<DefId> {
        self.ancestry.get(&def).copied()
    }

    /// Returns an iterator for the IDs in the ancestry tree above
    /// the given HIR [`DefId`] node.
    pub(crate) fn hir_parent_id_iter(&self, def: DefId) -> ParentHirIterator<'_> {
        ParentHirIterator {
            tcx: self,
            current: Some(def),
        }
    }

    /// Returns the parent of the given HIR element, if any is found.
    pub(crate) fn hir_parent_iter(&self, def: DefId) -> impl Iterator<Item = lume_hir::Def> {
        self.hir_parent_id_iter(def).map(move |id| self.hir_expect_def(id))
    }

    /// Returns all the type parameters available for the [`lume_hir::Def`] with the given ID.
    pub(crate) fn hir_avail_type_params(&self, def: DefId) -> Vec<&lume_hir::TypeParameter> {
        let mut acc = Vec::new();

        for parent in self.hir_parent_iter(def) {
            let lume_hir::Def::Item(item) = parent else {
                continue;
            };

            for type_param in item.type_parameters() {
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
    pub(crate) fn hir_ctx_return_type(&self, def: DefId) -> Result<lume_types::TypeRef> {
        for parent in self.hir_parent_iter(def) {
            let lume_hir::Def::Item(item) = parent else {
                continue;
            };

            let return_type = match &item {
                lume_hir::Item::Method(method) => method.return_type.as_ref(),
                lume_hir::Item::Function(func) => func.return_type.as_ref(),
                _ => continue,
            };

            if let Some(ty) = &return_type {
                let type_params = self.hir_avail_type_params(def);
                let type_ref = self.mk_type_ref_generic(ty, &type_params)?;

                return Ok(type_ref);
            }

            return Ok(lume_types::TypeRef::void());
        }

        let location = self.hir_expect_def(def).location().clone();

        Err(diagnostics::NoReturningAncestor {
            source: location.file,
            range: location.index,
        }
        .into())
    }
}
