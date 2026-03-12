//! This module handles the introduction of type variables into an existing HIR,
//! in response to type arguments not being provided for a generic item.
//!
//! For example, in code like this:
//! ```lm
//! fn main() {
//!     // Type parameter `T` of `Array` explicitly defined...
//!     let array = Array<Int32>::new();
//!
//!     // ...but not here. As such, insert a type variable.
//!     let iter = Array::iter(array);
//!
//!     // And also no type argument here either - insert another type variable.
//!     let next = std::iter::Iterator::next(iter);
//! }
//! ```
//!
//! It's important to notice that, even though `std::iter::Iterator::next` would
//! still refer to the same type parameter as `Array::iter`, a new type variable
//! is introduced. The idea is to create as many type variables as is required,
//! then unify them in the next stage.
//!
//! The above sample would effectively look something like this:
//! ```lm
//! fn main() {
//!     let array = Array<Int32>::new();
//!     let iter = Array<T?D4170AE284F4DD9C>::iter(array);
//!     let next = std::iter::Iterator<T?F9722F66044E5AFA>::next(iter);
//! }
//! ```
//!
//! The `T?...` notation is used to refer to type variables and is actually not
//! valid syntax for any Lume programs.
//!
//! Type variables are meant to be ephemeral and should only be used when
//! unifying. After type inference is completed, all type variables will be
//! replaced with their unified type substitute.

use lume_errors::Result;
use lume_infer::TyInferCtx;
use lume_span::NodeId;

use crate::engine::{Engine, TypeVar};

/// Denotes all available types which can be inferred and unified.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum InferedNode {
    Cast(lume_hir::Cast),
    Construct(lume_hir::Construct),
    VariableDeclaration(lume_hir::VariableDeclaration),
    IntrinsicCall(lume_hir::IntrinsicCall),
    InstanceCall(lume_hir::InstanceCall),
    StaticCall(lume_hir::StaticCall),
    Variant(lume_hir::Variant),
    Pattern(lume_hir::Pattern),
}

impl TryFrom<&lume_hir::Node> for InferedNode {
    type Error = ();

    fn try_from(value: &lume_hir::Node) -> std::result::Result<Self, Self::Error> {
        match value {
            lume_hir::Node::Statement(stmt) => match &stmt.kind {
                lume_hir::StatementKind::Variable(stmt) => Ok(InferedNode::VariableDeclaration(stmt.clone())),
                _ => Err(()),
            },
            lume_hir::Node::Expression(expr) => match &expr.kind {
                lume_hir::ExpressionKind::Cast(expr) => Ok(InferedNode::Cast(expr.clone())),
                lume_hir::ExpressionKind::Construct(expr) => Ok(InferedNode::Construct(expr.clone())),
                lume_hir::ExpressionKind::IntrinsicCall(call) => Ok(InferedNode::IntrinsicCall(call.clone())),
                lume_hir::ExpressionKind::InstanceCall(call) => Ok(InferedNode::InstanceCall(call.clone())),
                lume_hir::ExpressionKind::StaticCall(call) => Ok(InferedNode::StaticCall(call.clone())),
                lume_hir::ExpressionKind::Variant(variant) => Ok(InferedNode::Variant(variant.clone())),
                _ => Err(()),
            },
            lume_hir::Node::Pattern(pattern) => Ok(InferedNode::Pattern(pattern.clone())),
            _ => Err(()),
        }
    }
}

impl From<InferedNode> for lume_hir::Node {
    fn from(value: InferedNode) -> lume_hir::Node {
        match value {
            InferedNode::Cast(expr) => lume_hir::Node::Expression(lume_hir::Expression {
                id: expr.id,
                location: expr.location,
                kind: lume_hir::ExpressionKind::Cast(expr),
            }),
            InferedNode::Construct(expr) => lume_hir::Node::Expression(lume_hir::Expression {
                id: expr.id,
                location: expr.location,
                kind: lume_hir::ExpressionKind::Construct(expr),
            }),
            InferedNode::VariableDeclaration(stmt) => lume_hir::Node::Statement(lume_hir::Statement {
                id: stmt.id,
                location: stmt.location,
                kind: lume_hir::StatementKind::Variable(stmt),
            }),
            InferedNode::IntrinsicCall(call) => lume_hir::Node::Expression(lume_hir::Expression {
                id: call.id,
                location: call.location,
                kind: lume_hir::ExpressionKind::IntrinsicCall(call),
            }),
            InferedNode::InstanceCall(call) => lume_hir::Node::Expression(lume_hir::Expression {
                id: call.id,
                location: call.location,
                kind: lume_hir::ExpressionKind::InstanceCall(call),
            }),
            InferedNode::StaticCall(call) => lume_hir::Node::Expression(lume_hir::Expression {
                id: call.id,
                location: call.location,
                kind: lume_hir::ExpressionKind::StaticCall(call),
            }),
            InferedNode::Variant(expr) => lume_hir::Node::Expression(lume_hir::Expression {
                id: expr.id,
                location: expr.location,
                kind: lume_hir::ExpressionKind::Variant(expr),
            }),
            InferedNode::Pattern(pat) => lume_hir::Node::Pattern(pat),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum InferedNodeRef<'hir> {
    Cast(&'hir lume_hir::Cast),
    Construct(&'hir lume_hir::Construct),
    VariableDeclaration(&'hir lume_hir::VariableDeclaration),
    IntrinsicCall(&'hir lume_hir::IntrinsicCall),
    InstanceCall(&'hir lume_hir::InstanceCall),
    StaticCall(&'hir lume_hir::StaticCall),
    Variant(&'hir lume_hir::Variant),
    Pattern(&'hir lume_hir::Pattern),
}

impl<'hir> TryFrom<&'hir lume_hir::Node> for InferedNodeRef<'hir> {
    type Error = ();

    fn try_from(value: &'hir lume_hir::Node) -> std::result::Result<Self, Self::Error> {
        match value {
            lume_hir::Node::Statement(stmt) => match &stmt.kind {
                lume_hir::StatementKind::Variable(stmt) => Ok(InferedNodeRef::VariableDeclaration(stmt)),
                _ => Err(()),
            },
            lume_hir::Node::Expression(expr) => match &expr.kind {
                lume_hir::ExpressionKind::Cast(expr) => Ok(InferedNodeRef::Cast(expr)),
                lume_hir::ExpressionKind::Construct(expr) => Ok(InferedNodeRef::Construct(expr)),
                lume_hir::ExpressionKind::IntrinsicCall(call) => Ok(InferedNodeRef::IntrinsicCall(call)),
                lume_hir::ExpressionKind::InstanceCall(call) => Ok(InferedNodeRef::InstanceCall(call)),
                lume_hir::ExpressionKind::StaticCall(call) => Ok(InferedNodeRef::StaticCall(call)),
                lume_hir::ExpressionKind::Variant(variant) => Ok(InferedNodeRef::Variant(variant)),
                _ => Err(()),
            },
            lume_hir::Node::Pattern(pattern) => Ok(InferedNodeRef::Pattern(pattern)),
            _ => Err(()),
        }
    }
}

#[derive(Debug, PartialEq)]
enum Call<'hir> {
    Intrinsic(&'hir mut lume_hir::IntrinsicCall),
    Instance(&'hir mut lume_hir::InstanceCall),
    Static(&'hir mut lume_hir::StaticCall),
}

impl Engine<'_, TyInferCtx> {
    #[tracing::instrument(level = "INFO", skip_all, err)]
    pub(crate) fn introduce_type_variables(&mut self) -> Result<()> {
        for id in self.ctx.hir().nodes.keys().copied().collect::<Vec<_>>() {
            let mut node = if let Some(node) = self.ctx.hir_node(id)
                && let Ok(inferred) = InferedNode::try_from(node)
            {
                inferred
            } else {
                continue;
            };

            match &mut node {
                InferedNode::Pattern(pattern) => {
                    if let lume_hir::PatternKind::Variant(variant) = &mut pattern.kind {
                        self.introduce_type_variables_on_variant(pattern.id, &mut variant.name)?;
                    }
                }
                InferedNode::VariableDeclaration(decl) => {
                    if let Some(declared_type) = &mut decl.declared_type {
                        self.introduce_type_variables_on_type(decl.id, &mut declared_type.name)?;
                    }
                }
                InferedNode::Cast(expr) => {
                    self.introduce_type_variables_on_type(expr.id, &mut expr.target.name)?;
                }
                InferedNode::Construct(expr) => {
                    self.introduce_type_variables_on_type(expr.id, &mut expr.path)?;
                }
                InferedNode::IntrinsicCall(expr) => {
                    self.introduce_type_variables_on_callable(expr.id, &mut Call::Intrinsic(expr))?;
                }
                InferedNode::InstanceCall(expr) => {
                    self.introduce_type_variables_on_callable(expr.id, &mut Call::Instance(expr))?;
                }
                InferedNode::StaticCall(expr) => {
                    self.introduce_type_variables_on_callable(expr.id, &mut Call::Static(expr))?;
                }
                InferedNode::Variant(expr) => {
                    self.introduce_type_variables_on_variant(expr.id, &mut expr.name)?;
                }
            }

            self.ctx.hir_mut().nodes.insert(id, Into::<lume_hir::Node>::into(node));
        }

        Ok(())
    }

    #[tracing::instrument(level = "TRACE", skip_all, fields(path = %path.to_wide_string()), err)]
    fn introduce_type_variables_on_type(&mut self, node_id: NodeId, path: &mut lume_hir::Path) -> Result<()> {
        let location = self.ctx.hir_span_of_node(node_id);

        let Some(type_def) = self.ctx.tdb().find_type(path) else {
            tracing::warn!(path = %path.to_wide_string(), "type_not_found");
            return Ok(());
        };

        let type_def_id = type_def.id;
        let declared_type_args = path.bound_types().len();
        let type_params = self.ctx.type_params_of(type_def.id)?.to_vec();

        for type_param_id in type_params.into_iter().skip(declared_type_args) {
            let type_variable = self.fresh_var(type_def_id, type_param_id, location);
            let type_variable_type = type_variable_as_type(self.ctx.hir(), type_variable);

            if let lume_hir::PathSegment::Type { bound_types, .. } = &mut path.name {
                bound_types.push(type_variable_type);
            }

            tracing::info!(
                type_var = type_variable.to_string(),
                type_def = self.ctx.hir_path_of_node(type_def_id).to_wide_string(),
                type_parameter = self.ctx.hir_path_of_node(type_param_id).to_string(),
                location = location.to_string(),
                "introduce_type_var",
            );

            self.add_affected_node(node_id, type_variable);
        }

        Ok(())
    }

    #[tracing::instrument(level = "TRACE", skip_all, err)]
    fn introduce_type_variables_on_callable(&mut self, expr: NodeId, path: &mut Call) -> Result<()> {
        let location = self.ctx.hir_span_of_node(expr);

        let call = self.ctx.hir_call_expr(expr).expect("expected call expression");
        let callable = self.ctx.probe_callable(call)?;
        let signature = self.ctx.signature_of(callable)?;

        let owner = if matches!(callable, lume_infer::query::Callable::Method(_)) {
            self.ctx
                .hir_parent_iter(signature.id)
                .find_map(|node| node.is_item().then_some(node.id()))
                .unwrap_or(signature.id)
        } else {
            signature.id
        };

        tracing::trace!(callable = %callable.name().to_wide_string());

        let type_params = self.ctx.available_type_params_at(callable.id());
        let type_args = call.all_type_arguments();

        // All all the type arguments have already been declared, theres
        // nothing for us to infer.
        if type_args.len() >= type_params.len() {
            return Ok(());
        }

        for type_parameter in type_params.into_iter().skip(type_args.len()) {
            let is_bound_to_method = signature.type_params.contains(&type_parameter);

            let type_variable = self.fresh_var(owner, type_parameter, location);
            let type_variable_type = type_variable_as_type(self.ctx.hir(), type_variable);

            match path {
                Call::Static(call) => {
                    // Push the type variable onto the type path segment or callable path segment,
                    // depending on which one the type parameter is bound to.
                    if is_bound_to_method {
                        if let lume_hir::PathSegment::Callable { bound_types, .. } = &mut call.name.name {
                            bound_types.push(type_variable_type);
                        }
                    } else if let Some(lume_hir::PathSegment::Type { bound_types, .. }) = &mut call.name.root.last_mut()
                    {
                        bound_types.push(type_variable_type);
                    }
                }
                Call::Instance(call) => {
                    // Push the type variable onto the type path segment or callable path segment,
                    // depending on which one the type parameter is bound to.
                    if is_bound_to_method {
                        if let lume_hir::PathSegment::Callable { bound_types, .. } = &mut call.name {
                            bound_types.push(type_variable_type);
                        }
                    } else {
                        call.bound_types.push(type_variable_type);
                    }
                }
                Call::Intrinsic(call) => {
                    call.bound_types.push(type_variable_type);
                }
            }

            tracing::info!(
                type_var = type_variable.to_string(),
                callable = self.ctx.hir_path_of_node(signature.id).to_wide_string(),
                type_parameter = self.ctx.hir_path_of_node(type_parameter).to_string(),
                location = location.to_string(),
                "introduce_type_var",
            );

            self.add_affected_node(expr, type_variable);
        }

        Ok(())
    }

    #[tracing::instrument(level = "TRACE", skip_all, err)]
    fn introduce_type_variables_on_variant(&mut self, node_id: NodeId, path: &mut lume_hir::Path) -> Result<()> {
        let location = self.ctx.hir_span_of_node(node_id);

        let parent_path = path
            .clone()
            .parent()
            .expect("expected Variant path segment to have Type parent");

        let enum_def = self.ctx.enum_def_with_name(&parent_path)?;
        let declared_type_args = path.all_bound_types().len();

        let owner_id = enum_def.id;

        let bound_types = enum_def
            .type_parameters
            .iter()
            .copied()
            .skip(declared_type_args)
            .collect::<Vec<_>>();

        for type_parameter in bound_types {
            let type_variable = self.fresh_var(owner_id, type_parameter, location);
            let type_variable_type = type_variable_as_type(self.ctx.hir(), type_variable);

            if let Some(lume_hir::PathSegment::Type { bound_types, .. }) = &mut path.root.last_mut() {
                bound_types.push(type_variable_type);
            }

            tracing::info!(
                type_var = type_variable.to_string(),
                variant = path.to_wide_string(),
                type_parameter = self.ctx.hir_path_of_node(type_parameter).to_string(),
                location = location.to_string(),
                "introduce_type_var",
            );

            self.add_affected_node(node_id, type_variable);
        }

        Ok(())
    }
}

fn type_variable_as_type(hir: &lume_hir::Map, id: TypeVar<TyInferCtx>) -> lume_hir::Type {
    let type_variable = hir.expect_type_variable(id.0).unwrap();
    let type_variable_name = lume_hir::PathSegment::Type {
        name: id.to_string().into(),
        bound_types: Vec::new(),
        location: type_variable.location,
    };

    lume_hir::Type {
        id: id.0.into(),
        name: lume_hir::Path::rooted(type_variable_name),
        self_type: false,
        location: type_variable.location,
    }
}
