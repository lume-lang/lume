use error_snippet::Result;
use lume_query::cached_query;
use lume_span::{DefId, ExpressionId, FieldId, ItemId, Location, StatementId};

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
    #[tracing::instrument(level = "TRACE", skip(self), ret)]
    pub fn hir_item(&self, id: ItemId) -> Option<&lume_hir::Item> {
        self.hir.items.get(&id)
    }

    /// Returns the [`lume_hir::Field`] with the given ID, if any.
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn hir_prop(&self, id: lume_span::FieldId) -> Option<&lume_hir::Field> {
        self.hir_item(id.item).and_then(|item| match item {
            lume_hir::Item::Type(ty) => match &**ty {
                lume_hir::TypeDefinition::Struct(s) => s.fields.get(id.index.as_usize()),
                _ => None,
            },
            _ => None,
        })
    }

    /// Returns the [`lume_hir::Pattern`] with the given ID, if any.
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn hir_pat(&self, id: lume_span::PatternId) -> Option<&lume_hir::Pattern> {
        self.hir.patterns.get(&id)
    }

    /// Returns the [`lume_hir::Def`] with the given ID, if any.
    #[tracing::instrument(level = "TRACE", skip(self), ret)]
    pub fn hir_method(&self, id: lume_span::MethodId) -> Option<lume_hir::Def<'_>> {
        self.hir_item(id.item).and_then(|item| match item {
            lume_hir::Item::Impl(item) => Some(lume_hir::Def::Method(item.methods.get(id.index.as_usize())?)),
            lume_hir::Item::TraitImpl(item) => {
                Some(lume_hir::Def::TraitMethodImpl(item.methods.get(id.index.as_usize())?))
            }
            lume_hir::Item::Type(item) => match &**item {
                lume_hir::TypeDefinition::Trait(item) => {
                    Some(lume_hir::Def::TraitMethodDef(item.methods.get(id.index.as_usize())?))
                }
                _ => None,
            },
            lume_hir::Item::Function(_) => None,
        })
    }

    /// Returns the [`lume_hir::Statement`] with the given ID, if any.
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn hir_stmt(&self, id: lume_span::StatementId) -> Option<&lume_hir::Statement> {
        self.hir.statements.get(&id)
    }

    /// Returns the [`lume_hir::Expression`] with the given ID, if any.
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn hir_expr(&self, id: lume_span::ExpressionId) -> Option<&lume_hir::Expression> {
        self.hir.expressions.get(&id)
    }

    /// Returns the [`lume_hir::CallExpression`] with the given ID, if any.
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn hir_call_expr(&self, id: lume_span::ExpressionId) -> Option<lume_hir::CallExpression<'_>> {
        match &self.hir_expr(id)?.kind {
            lume_hir::ExpressionKind::InstanceCall(call) => Some(lume_hir::CallExpression::Instanced(call)),
            lume_hir::ExpressionKind::IntrinsicCall(call) => Some(lume_hir::CallExpression::Intrinsic(call)),
            lume_hir::ExpressionKind::StaticCall(call) => Some(lume_hir::CallExpression::Static(call)),
            _ => None,
        }
    }

    /// Returns the [`lume_hir::Def`] with the given ID, if any.
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn hir_def(&'_ self, id: lume_span::DefId) -> Option<lume_hir::Def<'_>> {
        match id {
            lume_span::DefId::Item(id) => self.hir_item(id).map(lume_hir::Def::Item),
            lume_span::DefId::Field(id) => self.hir_prop(id).map(lume_hir::Def::Field),
            lume_span::DefId::Method(id) => self.hir_method(id),
            lume_span::DefId::Pattern(id) => self.hir_pat(id).map(lume_hir::Def::Pattern),
            lume_span::DefId::Statement(id) => self.hir_stmt(id).map(lume_hir::Def::Statement),
            lume_span::DefId::Expression(id) => self.hir_expr(id).map(lume_hir::Def::Expression),
        }
    }

    /// Returns the [`lume_hir::Item`] with the given ID, if any.
    ///
    /// # Panics
    ///
    /// Panics if no [`lume_hir::Item`] with the given ID was found.
    #[track_caller]
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn hir_expect_item(&self, id: ItemId) -> &lume_hir::Item {
        match self.hir.items.get(&id) {
            Some(item) => item,
            None => panic!("expected HIR item with ID of {id:?}"),
        }
    }

    /// Returns the [`lume_hir::Field`] with the given ID, if any.
    ///
    /// # Panics
    ///
    /// Panics if no [`lume_hir::Field`] with the given ID was found.
    #[track_caller]
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn hir_expect_field(&self, id: FieldId) -> &lume_hir::Field {
        let Some(field) = self.hir_prop(id) else {
            panic!("expected HIR field with ID of {id:?}")
        };

        field
    }

    /// Returns the [`lume_hir::TypeDefinition`] with the given ID, if any.
    ///
    /// # Panics
    ///
    /// Panics if no [`lume_hir::TypeDefinition`] with the given ID was found.
    #[track_caller]
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn hir_expect_type(&self, id: ItemId) -> &lume_hir::TypeDefinition {
        let lume_hir::Item::Type(ty) = self.hir_expect_item(id) else {
            panic!("expected HIR type with ID of {id:?}")
        };

        ty
    }

    /// Returns the [`lume_hir::StructDefinition`] with the given ID, if any.
    ///
    /// # Panics
    ///
    /// Panics if no [`lume_hir::StructDefinition`] with the given ID was found.
    #[track_caller]
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn hir_expect_struct(&self, id: ItemId) -> &lume_hir::StructDefinition {
        let lume_hir::TypeDefinition::Struct(ty) = self.hir_expect_type(id) else {
            panic!("expected HIR struct with ID of {id:?}")
        };

        ty
    }

    /// Returns the [`lume_hir::EnumDefinition`] with the given ID, if any.
    ///
    /// # Panics
    ///
    /// Panics if no [`lume_hir::EnumDefinition`] with the given ID was found.
    #[track_caller]
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn hir_expect_enum(&self, id: ItemId) -> &lume_hir::EnumDefinition {
        let lume_hir::TypeDefinition::Enum(ty) = self.hir_expect_type(id) else {
            panic!("expected HIR enum with ID of {id:?}")
        };

        ty
    }

    /// Returns the [`lume_hir::TraitDefinition`] with the given ID, if any.
    ///
    /// # Panics
    ///
    /// Panics if no [`lume_hir::TraitDefinition`] with the given ID was found.
    #[track_caller]
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn hir_expect_trait(&self, id: ItemId) -> &lume_hir::TraitDefinition {
        let lume_hir::TypeDefinition::Trait(ty) = self.hir_expect_type(id) else {
            panic!("expected HIR trait with ID of {id:?}")
        };

        ty
    }

    /// Returns the [`lume_hir::Expression`] with the given ID, if any.
    ///
    /// # Panics
    ///
    /// Panics if no [`lume_hir::Expression`] with the given ID was found.
    #[track_caller]
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
    #[track_caller]
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
    #[track_caller]
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn hir_expect_def(&'_ self, id: lume_span::DefId) -> lume_hir::Def<'_> {
        match self.hir_def(id) {
            Some(item) => item,
            None => panic!("expected HIR def with ID of {id:?}"),
        }
    }

    /// Returns the [`lume_hir::Pattern`] with the given ID, if any.
    ///
    /// # Panics
    ///
    /// Panics if no [`lume_hir::Pattern`] with the given ID was found.
    #[track_caller]
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn hir_expect_pattern(&self, id: lume_span::DefId) -> &lume_hir::Pattern {
        match self.hir_expect_def(id) {
            lume_hir::Def::Pattern(pat) => pat,
            _ => panic!("expected HIR pattern with ID of {id:?}"),
        }
    }

    /// Returns the parent of the given HIR element, if any is found.
    #[cached_query]
    #[track_caller]
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn hir_parent_of(&self, def: DefId) -> Option<DefId> {
        self.ancestry.get(&def).copied()
    }

    /// Returns an iterator for the IDs in the ancestry tree above
    /// the given HIR [`DefId`] node.
    #[track_caller]
    pub fn hir_parent_id_iter(&self, def: DefId) -> ParentHirIterator<'_> {
        ParentHirIterator {
            tcx: self,
            current: Some(def),
        }
    }

    /// Returns the parent of the given HIR element, if any is found.
    #[tracing::instrument(level = "TRACE", skip(self))]
    #[track_caller]
    pub fn hir_parent_iter(&self, def: DefId) -> impl Iterator<Item = lume_hir::Def<'_>> {
        self.hir_parent_id_iter(def).filter_map(move |id| self.hir_def(id))
    }

    /// Attempts to find the closest switch expression from the given definition.
    #[track_caller]
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn hir_switch_expression(&self, source: DefId) -> Option<&lume_hir::Switch> {
        for parent in self.hir_parent_iter(source) {
            let lume_hir::Def::Expression(expr) = parent else {
                continue;
            };

            if let lume_hir::ExpressionKind::Switch(switch) = &expr.kind {
                return Some(switch);
            }
        }

        None
    }

    /// Attempts to find the closest loop from the given definition.
    #[track_caller]
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn hir_loop_target(&self, source: DefId) -> Option<&lume_hir::Statement> {
        for parent in self.hir_parent_iter(source) {
            let lume_hir::Def::Statement(stmt) = parent else {
                continue;
            };

            if stmt.is_loop() {
                return Some(stmt);
            }
        }

        None
    }

    /// Attempts to find the closes loop from the given statement.
    #[track_caller]
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn hir_loop_target_stmt(&self, id: StatementId) -> Option<&lume_hir::Statement> {
        self.hir_loop_target(DefId::Statement(id))
    }

    /// Returns the parameters available for the [`lume_hir::Def`] with the given ID.
    #[cached_query]
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn hir_avail_params(&self, def: DefId) -> Vec<lume_hir::Parameter> {
        let mut acc = Vec::new();

        for parent in self.hir_parent_iter(def) {
            let params = match parent {
                lume_hir::Def::Item(item) => {
                    if let lume_hir::Item::Function(func) = item {
                        &func.parameters
                    } else {
                        continue;
                    }
                }
                lume_hir::Def::Method(method) => &method.parameters,
                lume_hir::Def::TraitMethodDef(method) => &method.parameters,
                lume_hir::Def::TraitMethodImpl(method) => &method.parameters,
                _ => continue,
            };

            acc.extend_from_slice(params);
        }

        acc
    }

    /// Returns all the type parameters available for the [`lume_hir::Def`] with the given ID.
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn hir_avail_type_params(&self, def: DefId) -> Vec<lume_hir::TypeParameter> {
        let mut acc = Vec::new();

        for parent in self.hir_parent_iter(def) {
            acc.extend_from_slice(&parent.type_parameters().inner);
        }

        acc
    }

    /// Returns all the type parameters available for the [`lume_hir::Item`] with the given ID.
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn hir_avail_type_params_item(&self, item: ItemId) -> Vec<lume_hir::TypeParameter> {
        self.hir_avail_type_params(DefId::Item(item))
    }

    /// Returns all the type parameters available for the [`lume_hir::Expression`] with the given ID.
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn hir_avail_type_params_expr(&self, expr: ExpressionId) -> Vec<lume_hir::TypeParameter> {
        self.hir_avail_type_params(DefId::Expression(expr))
    }

    /// Gets the return type of the [`lume_hir::Item`] with the given ID.
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn hir_item_return_type<'a>(&self, item: &'a lume_hir::Item) -> Option<&'a lume_hir::Type> {
        if let lume_hir::Item::Function(func) = item {
            Some(&func.return_type)
        } else {
            None
        }
    }

    /// Gets the return type of the [`lume_hir::Def`] with the given ID.
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn hir_def_return_type<'a>(&self, def: lume_hir::Def<'a>) -> Option<&'a lume_hir::Type> {
        match &def {
            lume_hir::Def::Method(method) => Some(&method.return_type),
            lume_hir::Def::TraitMethodDef(method) => Some(&method.return_type),
            lume_hir::Def::TraitMethodImpl(method) => Some(&method.return_type),
            lume_hir::Def::Item(item) => self.hir_item_return_type(item),
            _ => None,
        }
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
        let type_params_hir = self.hir_avail_type_params(def);
        let type_params = type_params_hir.iter().map(AsRef::as_ref).collect::<Vec<_>>();

        for parent in self.hir_parent_iter(def) {
            let Some(return_type) = self.hir_def_return_type(parent) else {
                continue;
            };

            return self.mk_type_ref_generic(return_type, &type_params);
        }

        let location = self.hir_expect_def(def).location();

        Err(diagnostics::NoReturningAncestor { source: location }.into())
    }

    /// Attempts to get the body of the given [`ItemId`], if it contains a body.
    ///
    /// Otherwise, returns [`None`].
    ///
    /// # Panics
    ///
    /// This method panics if the given [`ItemId`] is not a valid item or if the
    /// item type cannot contain a body.
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn hir_body_of_item(&self, id: ItemId) -> Option<&lume_hir::Block> {
        if let lume_hir::Item::Function(func) = self.hir_expect_item(id) {
            func.block.as_ref()
        } else {
            None
        }
    }

    /// Attempts to get the body of the given [`DefId`], if it contains a body.
    ///
    /// Otherwise, returns [`None`].
    ///
    /// # Panics
    ///
    /// This method panics if the given [`DefId`] is not a valid item or if the
    /// item type cannot contain a body.
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn hir_body_of_def(&self, id: DefId) -> Option<&lume_hir::Block> {
        let def = self.hir_expect_def(id);

        match &def {
            lume_hir::Def::Method(method) => method.block.as_ref(),
            lume_hir::Def::TraitMethodDef(func) => func.block.as_ref(),
            lume_hir::Def::TraitMethodImpl(func) => func.block.as_ref(),
            lume_hir::Def::Item(item) => self.hir_body_of_item(item.id()),
            ty => panic!("bug!: item type cannot contain a body: {ty:?}"),
        }
    }

    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn hir_trait_def_of_impl(
        &self,
        trait_impl: &lume_hir::TraitImplementation,
    ) -> Result<&lume_hir::TraitDefinition> {
        let trait_name = &trait_impl.name.name;
        let Some(trait_def_ty) = self.tdb().find_type(trait_name) else {
            panic!("bug!: trait definition with name `{trait_name:+}` does not exist");
        };

        let lume_types::TypeKind::User(lume_types::UserType::Trait(trait_kind)) = &trait_def_ty.kind else {
            panic!("bug!: expected trait definition type to be trait");
        };

        Ok(self.hir_expect_trait(trait_kind.id))
    }

    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn hir_trait_def_of_method_impl(
        &self,
        trait_method_impl: &lume_hir::TraitMethodImplementation,
    ) -> Result<&lume_hir::TraitDefinition> {
        for parent in self.hir_parent_iter(trait_method_impl.id) {
            if let lume_hir::Def::Item(lume_hir::Item::TraitImpl(trait_impl)) = parent {
                return self.hir_trait_def_of_impl(trait_impl);
            }
        }

        panic!("bug!: trait method implementation defined outside trait implementation");
    }

    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn hir_trait_method_def_of_impl(
        &self,
        trait_method_impl: &lume_hir::TraitMethodImplementation,
    ) -> Result<&lume_hir::TraitMethodDefinition> {
        let trait_def = self.hir_trait_def_of_method_impl(trait_method_impl)?;

        let Some(trait_method_def) = trait_def
            .methods
            .iter()
            .find(|method| method.name == trait_method_impl.name)
        else {
            panic!("bug!: trait method implementation exists without trait method definition");
        };

        Ok(trait_method_def)
    }

    /// Returns the span of the HIR definition with the given ID.
    #[tracing::instrument(level = "TRACE", skip(self))]
    pub fn hir_span_of_def(&self, def: DefId) -> Location {
        self.hir_expect_def(def).location()
    }
}
