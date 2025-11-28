use lume_architect::cached_query;
use lume_errors::Result;
use lume_hir::Node;
use lume_span::NodeId;
use lume_types::TypeRef;

use crate::TyInferCtx;

impl TyInferCtx {
    /// Gets the parent type of the given node.
    #[libftrace::traced(level = Trace, fields(id), err, ret)]
    pub fn parent_type_of(&self, id: NodeId) -> Result<Option<TypeRef>> {
        for parent in self.hir_parent_iter(id) {
            match parent {
                Node::Type(lume_hir::TypeDefinition::Struct(struct_def)) => {
                    let struct_type = lume_hir::Type {
                        id: lume_hir::TypeId::from(struct_def.id),
                        name: struct_def.name.clone(),
                        location: struct_def.location,
                    };

                    return self.mk_type_ref_from(&struct_type, id).map(Some);
                }
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

        Err(super::diagnostics::NoParentStruct {
            source: self.hir_span_of_node(field_id),
        }
        .into())
    }

    /// Attempts to find the closest switch expression from the given
    /// definition.
    #[track_caller]
    #[libftrace::traced(level = Trace)]
    pub fn switch_expr_at(&self, source: NodeId) -> Option<&lume_hir::Switch> {
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
    pub fn loop_target_at(&self, source: NodeId) -> Option<&lume_hir::Statement> {
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

    /// Returns the parameters available for the [`lume_hir::Node`] with the
    /// given ID.
    #[cached_query]
    #[libftrace::traced(level = Trace)]
    pub fn available_params_at(&self, def: NodeId) -> Vec<lume_hir::Parameter> {
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

    /// Returns all the type parameters available for the [`lume_hir::Node`]
    /// with the given ID.
    #[libftrace::traced(level = Trace)]
    pub fn available_type_params_at(&self, def: NodeId) -> Vec<NodeId> {
        let mut acc = Vec::new();

        for parent in self.hir_parent_iter(def) {
            let Ok(type_params) = self.type_params_of(parent.id()) else {
                continue;
            };

            acc.extend_from_slice(type_params);
        }

        acc
    }

    /// Gets the return type of the [`lume_hir::Node`] with the given ID.
    #[libftrace::traced(level = Trace)]
    pub fn return_type_of<'a>(&'a self, id: NodeId) -> Option<&'a lume_hir::Type> {
        match self.hir_node(id)? {
            Node::Function(method) => Some(&method.return_type),
            Node::Method(method) => Some(&method.return_type),
            Node::TraitMethodDef(method) => Some(&method.return_type),
            Node::TraitMethodImpl(method) => Some(&method.return_type),
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
    pub fn return_type_within(&self, def: NodeId) -> Result<lume_types::TypeRef> {
        let type_parameters_id = self.available_type_params_at(def);
        let type_parameters = self.as_type_params(&type_parameters_id)?;

        for parent in self.hir_parent_iter(def) {
            let Some(return_type) = self.return_type_of(parent.id()) else {
                continue;
            };

            return self.mk_type_ref_generic(return_type, &type_parameters);
        }

        let location = self.hir_expect_node(def).location();

        Err(super::diagnostics::NoReturningAncestor { source: location }.into())
    }

    /// Gets the documentation for the given node as a string.
    pub fn documentation_string_of(&self, id: NodeId) -> Option<&String> {
        match self.hir_node(id)? {
            lume_hir::Node::Function(func) => func.doc_comment.as_ref(),
            lume_hir::Node::Type(ty) => match ty {
                lume_hir::TypeDefinition::Struct(def) => def.doc_comment.as_ref(),
                lume_hir::TypeDefinition::Trait(def) => def.doc_comment.as_ref(),
                lume_hir::TypeDefinition::Enum(def) => def.doc_comment.as_ref(),
                _ => None,
            },
            lume_hir::Node::Method(method) => method.doc_comment.as_ref(),
            lume_hir::Node::TraitMethodDef(method) => method.doc_comment.as_ref(),
            _ => None,
        }
    }
}
