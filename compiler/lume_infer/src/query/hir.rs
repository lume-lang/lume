use error_snippet::Result;
use lume_architect::cached_query;
use lume_hir::{Node, NodeRef};
use lume_span::*;
use lume_types::TypeRef;

use super::diagnostics;
use crate::TyInferCtx;

/// An iterator over the elements of a linked [`NodeId`]s.
///
/// This `struct` is created by [`TyInferCtx::hir_parent_id_iter()`].
pub struct ParentHirIterator<'a> {
    tcx: &'a TyInferCtx,
    current: Option<NodeId>,
}

impl Iterator for ParentHirIterator<'_> {
    type Item = NodeId;

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
    pub fn hir_nodes(&self) -> impl Iterator<Item = &Node> {
        self.hir.nodes.values()
    }

    /// Returns the [`lume_hir::Node`] with the given ID, if any.
    #[libftrace::traced(level = Trace, fields(id))]
    pub fn hir_node(&self, id: NodeId) -> Option<&Node> {
        self.hir.node(id)
    }

    /// Returns the [`lume_hir::Node`] with the given ID, if any.
    #[libftrace::traced(level = Trace, fields(id))]
    pub fn hir_expect_node(&self, id: NodeId) -> &Node {
        match self.hir_node(id) {
            Some(item) => item,
            None => panic!("expected HIR node with ID of {id:?}"),
        }
    }

    /// Returns the [`lume_hir::NodeRef`] with the given ID, if any.
    #[libftrace::traced(level = Trace, fields(id))]
    pub fn hir_node_ref(&self, id: NodeId) -> Option<NodeRef<'_>> {
        self.hir_node(id).map(|n| n.as_ref())
    }

    /// Returns the method with the given ID, if any, boxed as [`NodeRef`].
    #[libftrace::traced(level = Trace, fields(id))]
    pub fn hir_method(&self, id: NodeId) -> Option<NodeRef<'_>> {
        self.hir_node(id).and_then(|item| match item {
            lume_hir::Node::Impl(item) => Some(lume_hir::NodeRef::Method(item.methods.get(id.index.as_usize())?)),
            lume_hir::Node::TraitImpl(item) => Some(lume_hir::NodeRef::TraitMethodImpl(
                item.methods.get(id.index.as_usize())?,
            )),
            lume_hir::Node::Type(lume_hir::TypeDefinition::Trait(item)) => Some(lume_hir::NodeRef::TraitMethodDef(
                item.methods.get(id.index.as_usize())?,
            )),
            _ => None,
        })
    }

    /// Returns the [`lume_hir::Statement`] with the given ID, if any.
    #[libftrace::traced(level = Trace)]
    pub fn hir_stmt(&self, id: NodeId) -> Option<&lume_hir::Statement> {
        self.hir.statement(id)
    }

    /// Returns the [`lume_hir::Statement`] with the given ID, if any.
    ///
    /// # Panics
    ///
    /// Panics if no [`lume_hir::Statement`] with the given ID was found.
    #[track_caller]
    #[libftrace::traced(level = Trace)]
    pub fn hir_expect_stmt(&self, id: NodeId) -> &lume_hir::Statement {
        match self.hir_stmt(id) {
            Some(item) => item,
            None => panic!("expected HIR statement with ID of {id:?}"),
        }
    }

    /// Returns the [`lume_hir::Expression`] with the given ID, if any.
    #[libftrace::traced(level = Trace)]
    pub fn hir_expr(&self, id: NodeId) -> Option<&lume_hir::Expression> {
        self.hir.expression(id)
    }

    /// Returns the [`lume_hir::Expression`] with the given ID, if any.
    ///
    /// # Panics
    ///
    /// Panics if no [`lume_hir::Expression`] with the given ID was found.
    #[track_caller]
    #[libftrace::traced(level = Trace)]
    pub fn hir_expect_expr(&self, id: NodeId) -> &lume_hir::Expression {
        match self.hir_expr(id) {
            Some(item) => item,
            None => panic!("expected HIR expression with ID of {id:?}"),
        }
    }

    /// Returns the [`lume_hir::Pattern`] with the given ID, if any.
    #[libftrace::traced(level = Trace)]
    pub fn hir_pat(&self, id: NodeId) -> Option<&lume_hir::Pattern> {
        if let lume_hir::Node::Pattern(pattern) = self.hir.nodes.get(&id)? {
            Some(pattern)
        } else {
            None
        }
    }

    /// Returns the [`lume_hir::Pattern`] with the given ID, if any.
    ///
    /// # Panics
    ///
    /// Panics if no [`lume_hir::Pattern`] with the given ID was found.
    #[track_caller]
    #[libftrace::traced(level = Trace)]
    pub fn hir_expect_pattern(&self, id: NodeId) -> &lume_hir::Pattern {
        match self.hir_expect_node(id) {
            lume_hir::Node::Pattern(pat) => pat,
            _ => panic!("expected HIR pattern with ID of {id:?}"),
        }
    }

    /// Returns the [`lume_hir::CallExpression`] with the given ID, if any.
    #[libftrace::traced(level = Trace)]
    pub fn hir_call_expr(&self, id: NodeId) -> Option<lume_hir::CallExpression<'_>> {
        match &self.hir_expr(id)?.kind {
            lume_hir::ExpressionKind::InstanceCall(call) => Some(lume_hir::CallExpression::Instanced(call)),
            lume_hir::ExpressionKind::IntrinsicCall(call) => Some(lume_hir::CallExpression::Intrinsic(call)),
            lume_hir::ExpressionKind::StaticCall(call) => Some(lume_hir::CallExpression::Static(call)),
            _ => None,
        }
    }

    /// Returns the [`lume_hir::Field`] with the given ID, if any.
    #[libftrace::traced(level = Trace)]
    pub fn hir_field(&self, id: NodeId) -> Option<&lume_hir::Field> {
        match self.hir_node(id)? {
            lume_hir::Node::Field(field) => Some(field),
            _ => None,
        }
    }

    /// Returns the [`lume_hir::Field`] with the given ID, if any.
    ///
    /// # Panics
    ///
    /// Panics if no [`lume_hir::Field`] with the given ID was found.
    #[track_caller]
    #[libftrace::traced(level = Trace)]
    pub fn hir_expect_field(&self, id: NodeId) -> &lume_hir::Field {
        let Some(field) = self.hir_field(id) else {
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
    #[libftrace::traced(level = Trace)]
    pub fn hir_expect_type(&self, id: NodeId) -> &lume_hir::TypeDefinition {
        let lume_hir::Node::Type(ty) = self.hir_expect_node(id) else {
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
    #[libftrace::traced(level = Trace)]
    pub fn hir_expect_struct(&self, id: NodeId) -> &lume_hir::StructDefinition {
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
    #[libftrace::traced(level = Trace)]
    pub fn hir_expect_enum(&self, id: NodeId) -> &lume_hir::EnumDefinition {
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
    #[libftrace::traced(level = Trace)]
    pub fn hir_expect_trait(&self, id: NodeId) -> &lume_hir::TraitDefinition {
        let lume_hir::TypeDefinition::Trait(ty) = self.hir_expect_type(id) else {
            panic!("expected HIR trait with ID of {id:?}")
        };

        ty
    }

    /// Returns the parent of the given HIR element, if any is found.
    #[cached_query]
    #[track_caller]
    #[libftrace::traced(level = Trace)]
    pub fn hir_parent_of(&self, id: NodeId) -> Option<NodeId> {
        self.ancestry.get(&id).copied()
    }

    /// Returns the parent of the given HIR element, if any is found.
    #[track_caller]
    #[libftrace::traced(level = Trace)]
    pub fn hir_parent_node_of<'a>(&'a self, id: NodeId) -> Option<&'a Node> {
        self.hir_parent_of(id).and_then(|id| self.hir.node(id))
    }

    /// Returns an iterator for the IDs in the ancestry tree above
    /// the given HIR [`NodeId`] node.
    #[track_caller]
    pub fn hir_parent_id_iter(&self, def: NodeId) -> ParentHirIterator<'_> {
        ParentHirIterator {
            tcx: self,
            current: Some(def),
        }
    }

    /// Returns the parent of the given HIR element, if any is found.
    #[track_caller]
    pub fn hir_parent_iter(&self, def: NodeId) -> impl Iterator<Item = &Node> {
        self.hir_parent_id_iter(def).filter_map(move |id| self.hir_node(id))
    }

    /// Attempts to find the closest switch expression from the given
    /// definition.
    #[track_caller]
    #[libftrace::traced(level = Trace)]
    pub fn hir_switch_expression(&self, source: NodeId) -> Option<&lume_hir::Switch> {
        for parent in self.hir_parent_iter(source) {
            let lume_hir::Node::Expression(expr) = parent else {
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
    #[libftrace::traced(level = Trace)]
    pub fn hir_loop_target(&self, source: NodeId) -> Option<&lume_hir::Statement> {
        for parent in self.hir_parent_iter(source) {
            let lume_hir::Node::Statement(stmt) = parent else {
                continue;
            };

            if stmt.is_loop() {
                return Some(stmt);
            }
        }

        None
    }

    /// Returns the parameters available for the [`lume_hir::Def`] with the
    /// given ID.
    #[cached_query]
    #[libftrace::traced(level = Trace)]
    pub fn hir_avail_params(&self, def: NodeId) -> Vec<lume_hir::Parameter> {
        let mut acc = Vec::new();

        for parent in self.hir_parent_iter(def) {
            let params = match parent {
                lume_hir::Node::Function(func) => &func.parameters,
                lume_hir::Node::Method(method) => &method.parameters,
                lume_hir::Node::TraitMethodDef(method) => &method.parameters,
                lume_hir::Node::TraitMethodImpl(method) => &method.parameters,
                _ => continue,
            };

            acc.extend_from_slice(params);
        }

        acc
    }

    /// Returns all the type parameters available for the [`lume_hir::Def`] with
    /// the given ID.
    #[libftrace::traced(level = Trace)]
    pub fn hir_avail_type_params(&self, def: NodeId) -> lume_hir::TypeParameters {
        let mut acc = Vec::new();

        for parent in self.hir_parent_iter(def) {
            acc.extend_from_slice(&parent.type_parameters().inner);
        }

        lume_hir::TypeParameters { inner: acc }
    }

    /// Gets the return type of the [`lume_hir::Node`] with the given ID.
    #[libftrace::traced(level = Trace)]
    pub fn hir_node_return_type<'a>(&self, item: &'a lume_hir::Node) -> Option<&'a lume_hir::Type> {
        match item {
            lume_hir::Node::Function(func) => Some(&func.return_type),
            lume_hir::Node::Method(method) => Some(&method.return_type),
            lume_hir::Node::TraitMethodDef(method) => Some(&method.return_type),
            lume_hir::Node::TraitMethodImpl(method) => Some(&method.return_type),
            _ => None,
        }
    }

    /// Returns the expected return type within the context where the given
    /// [`NodeId`] is defined. Walks the ancestor tree until a function or
    /// method is found and returns it's return type.
    ///
    /// # Errors
    ///
    /// If no matching ancestor is found, returns [`Err`].
    #[libftrace::traced(level = Trace, err)]
    pub fn hir_ctx_return_type(&self, def: NodeId) -> Result<lume_types::TypeRef> {
        let type_params_hir = self.hir_avail_type_params(def);
        let type_params = type_params_hir.iter().map(AsRef::as_ref).collect::<Vec<_>>();

        for parent in self.hir_parent_iter(def) {
            let Some(return_type) = self.hir_node_return_type(parent) else {
                continue;
            };

            return self.mk_type_ref_generic(return_type, &type_params);
        }

        let location = self.hir_expect_node(def).location();

        Err(diagnostics::NoReturningAncestor { source: location }.into())
    }

    /// Attempts to get the body of the given [`NodeId`], if it contains a body.
    ///
    /// Otherwise, returns [`None`].
    #[libftrace::traced(level = Trace)]
    pub fn hir_body_of_node(&self, id: NodeId) -> Option<&lume_hir::Block> {
        match self.hir_node(id)? {
            lume_hir::Node::Method(method) => method.block.as_ref(),
            lume_hir::Node::TraitMethodDef(func) => func.block.as_ref(),
            lume_hir::Node::TraitMethodImpl(func) => func.block.as_ref(),
            lume_hir::Node::Function(func) => func.block.as_ref(),
            ty => panic!("bug!: item type cannot contain a body: {ty:?}"),
        }
    }

    #[libftrace::traced(level = Trace)]
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

    #[libftrace::traced(level = Trace)]
    pub fn hir_trait_def_of_method_impl(
        &self,
        trait_method_impl: &lume_hir::TraitMethodImplementation,
    ) -> Result<&lume_hir::TraitDefinition> {
        for parent in self.hir_parent_iter(trait_method_impl.id) {
            if let lume_hir::Node::TraitImpl(trait_impl) = parent {
                return self.hir_trait_def_of_impl(trait_impl);
            }
        }

        panic!("bug!: trait method implementation defined outside trait implementation");
    }

    #[libftrace::traced(level = Trace)]
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

    /// Gets the target type of the given method, given the type within the
    /// parent `impl` block.
    #[libftrace::traced(level = Trace)]
    pub fn impl_type_of_method(&self, method_id: NodeId) -> Result<TypeRef> {
        for parent in self.hir_parent_iter(method_id) {
            if let lume_hir::Node::Impl(impl_block) = parent {
                return self.mk_type_ref_from(&impl_block.target, method_id);
            }
        }

        Err(diagnostics::NoParentImpl {
            source: self.hir_span_of_node(method_id),
        }
        .into())
    }

    /// Gets the parent type of the given node.
    #[libftrace::traced(level = Trace, fields(id), err, ret)]
    pub fn parent_type_of(&self, id: NodeId) -> Result<Option<TypeRef>> {
        for parent in self.hir_parent_iter(id) {
            match parent {
                Node::Impl(impl_def) => {
                    return self.mk_type_ref_from(&impl_def.target, id).map(Some);
                }
                Node::TraitImpl(trait_impl) => {
                    return self.mk_type_ref_from(&trait_impl.target, id).map(Some);
                }
                _ => {}
            }
        }

        Ok(None)
    }

    /// Gets the parent type of the given field.
    #[libftrace::traced(level = Trace)]
    pub fn owning_struct_of_field(&self, field_id: NodeId) -> Result<&lume_hir::StructDefinition> {
        for parent in self.hir_parent_iter(field_id) {
            if let lume_hir::Node::Type(lume_hir::TypeDefinition::Struct(struct_def)) = parent {
                return Ok(struct_def);
            }
        }

        Err(diagnostics::NoParentStruct {
            source: self.hir_span_of_node(field_id),
        }
        .into())
    }

    /// Returns the span of the HIR definition with the given ID.
    #[libftrace::traced(level = Trace)]
    pub fn hir_span_of_node(&self, def: NodeId) -> Location {
        self.hir_expect_node(def).location()
    }

    /// Determines whether the given node is part of the current package.
    pub fn hir_is_local_node(&self, node: NodeId) -> bool {
        self.hir.is_local_node(node)
    }

    /// Gets the name of the `![lang_item]` attribute and corresponding method,
    /// matching the given intrinsic.
    #[libftrace::traced(level = Trace)]
    pub fn lang_item_of_intrinsic(&self, intrinsic: &lume_hir::IntrinsicKind) -> (&'static str, &'static str) {
        match intrinsic {
            lume_hir::IntrinsicKind::Add { .. } => ("add_trait", "add"),
            lume_hir::IntrinsicKind::Sub { .. } => ("sub_trait", "sub"),
            lume_hir::IntrinsicKind::Mul { .. } => ("mul_trait", "mul"),
            lume_hir::IntrinsicKind::Div { .. } => ("div_trait", "div"),
            lume_hir::IntrinsicKind::And { .. } => ("and_trait", "and"),
            lume_hir::IntrinsicKind::Or { .. } => ("or_trait", "or"),
            lume_hir::IntrinsicKind::Negate { .. } => ("negate_trait", "negate"),
            lume_hir::IntrinsicKind::Increment { .. } => ("increment_trait", "increment"),
            lume_hir::IntrinsicKind::Decrement { .. } => ("decrement_trait", "decrement"),
            lume_hir::IntrinsicKind::BinaryAnd { .. } => ("band_trait", "band"),
            lume_hir::IntrinsicKind::BinaryOr { .. } => ("bor_trait", "bor"),
            lume_hir::IntrinsicKind::BinaryXor { .. } => ("bxor_trait", "bxor"),
            lume_hir::IntrinsicKind::Not { .. } => ("not_trait", "not"),
            lume_hir::IntrinsicKind::Equal { .. } => ("equal_trait", "eq"),
            lume_hir::IntrinsicKind::NotEqual { .. } => ("equal_trait", "ne"),
            lume_hir::IntrinsicKind::Less { .. } => ("cmp_trait", "lt"),
            lume_hir::IntrinsicKind::LessEqual { .. } => ("cmp_trait", "le"),
            lume_hir::IntrinsicKind::Greater { .. } => ("cmp_trait", "gt"),
            lume_hir::IntrinsicKind::GreaterEqual { .. } => ("cmp_trait", "gt"),
        }
    }

    /// Gets the human-readable name of the operation, which is performed by the
    /// given operation.
    #[libftrace::traced(level = Trace)]
    pub fn operation_name_of_intrinsic(&self, intrinsic: &lume_hir::IntrinsicKind) -> &'static str {
        match intrinsic {
            lume_hir::IntrinsicKind::Add { .. } => "addition",
            lume_hir::IntrinsicKind::Sub { .. } => "subtraction",
            lume_hir::IntrinsicKind::Mul { .. } => "multiplication",
            lume_hir::IntrinsicKind::Div { .. } => "division",
            lume_hir::IntrinsicKind::And { .. } => "logical AND",
            lume_hir::IntrinsicKind::Or { .. } => "logical OR",
            lume_hir::IntrinsicKind::Negate { .. } => "negation",
            lume_hir::IntrinsicKind::Increment { .. } => "increment",
            lume_hir::IntrinsicKind::Decrement { .. } => "decrement",
            lume_hir::IntrinsicKind::BinaryAnd { .. } => "binary AND",
            lume_hir::IntrinsicKind::BinaryOr { .. } => "binary OR",
            lume_hir::IntrinsicKind::BinaryXor { .. } => "binary XOR",
            lume_hir::IntrinsicKind::Not { .. } => "binary NOT",
            lume_hir::IntrinsicKind::Equal { .. } => "equality",
            lume_hir::IntrinsicKind::NotEqual { .. } => "inequality",
            lume_hir::IntrinsicKind::Less { .. }
            | lume_hir::IntrinsicKind::LessEqual { .. }
            | lume_hir::IntrinsicKind::Greater { .. }
            | lume_hir::IntrinsicKind::GreaterEqual { .. } => "comparison",
        }
    }

    /// Gets the name of the type which defines the given intrinsic.
    #[libftrace::traced(level = Trace)]
    pub fn type_name_of_intrinsic(&self, intrinsic: &lume_hir::IntrinsicKind) -> Option<&lume_hir::Path> {
        let (lang_item, _) = self.lang_item_of_intrinsic(intrinsic);
        let item_def = self.lang_item(lang_item)?;

        Some(&self.tdb().type_(item_def)?.name)
    }
}
