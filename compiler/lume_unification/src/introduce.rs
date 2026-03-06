use lume_errors::Result;
use lume_hir::TypeId;
use lume_span::{Location, NodeId};

use crate::{TypeVariableId, UnificationPass, verify};

/// Denotes all available types which can be inferred and unified.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum InferedNode {
    Cast(lume_hir::Cast),
    Construct(lume_hir::Construct),
    VariableDeclaration(lume_hir::VariableDeclaration),
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

impl UnificationPass<'_> {
    #[tracing::instrument(level = "INFO", skip_all, err)]
    pub(crate) fn introduce_type_variables(&mut self) -> Result<()> {
        for id in self.tcx.hir().nodes.keys().copied().collect::<Vec<_>>() {
            let mut node = if let Some(node) = self.tcx.hir_node(id)
                && let Ok(inferred) = InferedNode::try_from(node)
            {
                inferred
            } else {
                continue;
            };

            match &mut node {
                InferedNode::Pattern(pattern) => {
                    if let lume_hir::PatternKind::Variant(variant) = &mut pattern.kind {
                        self.create_constraints_from_variant(pattern.id, &mut variant.name)?;
                    }
                }
                InferedNode::VariableDeclaration(decl) => {
                    if let Some(declared_type) = &decl.declared_type {
                        verify::verify_type_name(self.tcx, &declared_type.name, declared_type.location);
                    }
                }
                InferedNode::Cast(expr) => {
                    self.create_constraints_from_type(expr.id, &mut expr.target.name)?;
                }
                InferedNode::Construct(expr) => {
                    self.create_constraints_from_type(expr.id, &mut expr.path)?;
                }
                InferedNode::InstanceCall(expr) => {
                    self.create_constraints_from_callable(expr.id, lume_hir::PathingMut::Segment(&mut expr.name))?;
                }
                InferedNode::StaticCall(expr) => {
                    self.create_constraints_from_callable(expr.id, lume_hir::PathingMut::Full(&mut expr.name))?;
                }
                InferedNode::Variant(expr) => {
                    self.create_constraints_from_variant(expr.id, &mut expr.name)?;
                }
            }

            self.tcx.hir_mut().nodes.insert(id, Into::<lume_hir::Node>::into(node));
        }

        Ok(())
    }

    #[tracing::instrument(level = "TRACE", skip_all, err)]
    fn create_constraints_from_type(&mut self, node_id: NodeId, path: &mut lume_hir::Path) -> Result<()> {
        let location = self.tcx.hir_span_of_node(node_id);

        let Some(type_def) = self.tcx.tdb().find_type(path) else {
            tracing::warn!(path = %path.to_wide_string(), "type_not_found");
            return Ok(());
        };

        let type_def_id = type_def.id;
        let declared_type_args = path.bound_types().len();
        let type_params = self.tcx.type_params_of(type_def.id)?.to_vec();

        for type_param_id in type_params.into_iter().skip(declared_type_args) {
            let type_variable = allocate_type_variable(self.tcx.hir_mut(), type_param_id.into(), location);
            let type_variable_type = type_variable_as_type(self.tcx.hir(), type_variable);

            if let lume_hir::PathSegment::Type { bound_types, .. } = &mut path.name {
                bound_types.push(type_variable_type);
            }

            tracing::info!(
                type_var = type_variable.to_string(),
                type_def = self.tcx.hir_path_of_node(type_def_id).to_wide_string(),
                type_parameter = self.tcx.hir_path_of_node(type_param_id).to_string(),
                location = location.to_string(),
                "introduce_type_var",
            );

            self.add_affected_node(node_id, type_variable);
        }

        Ok(())
    }

    #[tracing::instrument(level = "TRACE", skip_all, err)]
    fn create_constraints_from_callable(&mut self, expr: NodeId, mut path: lume_hir::PathingMut<'_>) -> Result<()> {
        let location = self.tcx.hir_span_of_node(expr);

        let call = self.tcx.hir_call_expr(expr).expect("expected call expression");
        let callable = self.tcx.probe_callable(call)?;
        let signature = self.tcx.signature_of(callable)?;

        let is_instance = call.is_instance();

        let type_params = self.tcx.available_type_params_at(callable.id());
        let type_args = call.all_type_arguments();

        // All all the type arguments have already been declared, theres
        // nothing for us to infer.
        if type_args.len() >= type_params.len() {
            return Ok(());
        }

        for type_parameter in type_params.into_iter().skip(type_args.len()) {
            let is_bound_to_method = signature.type_params.contains(&type_parameter);

            // NOTE:
            // If the type parameter exists on the implementing type (as opposed to the
            // method) of an instance call, the type variable CANNOT be added!
            //
            // This is because the `name` of instance call expressions only contain the name
            // of the method, not anything about the type.
            //
            // So, instead of creating a new type variable which we wouldn't be able to
            // resolve anyway, we skip it and infer the type arguments from the
            // instance parameter (which is done in the constraint creation stage).
            if !is_bound_to_method && is_instance {
                continue;
            }

            let type_variable = allocate_type_variable(self.tcx.hir_mut(), type_parameter.into(), location);
            let type_variable_type = type_variable_as_type(self.tcx.hir(), type_variable);

            match &mut path {
                lume_hir::PathingMut::Full(path) => {
                    // Push the type variable onto the type path segment or callable path segment,
                    // depending on which one the type parameter is bound to.
                    if is_bound_to_method {
                        if let lume_hir::PathSegment::Callable { bound_types, .. } = &mut path.name {
                            bound_types.push(type_variable_type);
                        }
                    } else if let Some(lume_hir::PathSegment::Type { bound_types, .. }) = &mut path.root.last_mut() {
                        bound_types.push(type_variable_type);
                    }
                }
                lume_hir::PathingMut::Segment(name) => {
                    if let lume_hir::PathSegment::Callable { bound_types, .. } = name {
                        bound_types.push(type_variable_type);
                    }
                }
            }

            tracing::info!(
                type_var = type_variable.to_string(),
                callable = self.tcx.hir_path_of_node(signature.id).to_wide_string(),
                type_parameter = self.tcx.hir_path_of_node(type_parameter).to_string(),
                location = location.to_string(),
                "introduce_type_var",
            );

            self.add_affected_node(expr, type_variable);
        }

        Ok(())
    }

    #[tracing::instrument(level = "TRACE", skip_all, err)]
    fn create_constraints_from_variant(&mut self, node_id: NodeId, path: &mut lume_hir::Path) -> Result<()> {
        let location = self.tcx.hir_span_of_node(node_id);

        let parent_path = path
            .clone()
            .parent()
            .expect("expected Variant path segment to have Type parent");

        let enum_def = self.tcx.enum_def_with_name(&parent_path)?;
        let declared_type_args = path.all_bound_types().len();

        let bound_types = enum_def
            .type_parameters
            .iter()
            .copied()
            .skip(declared_type_args)
            .collect::<Vec<_>>();

        for type_parameter in bound_types {
            let type_variable = allocate_type_variable(self.tcx.hir_mut(), type_parameter.into(), location);
            let type_variable_type = type_variable_as_type(self.tcx.hir(), type_variable);

            if let Some(lume_hir::PathSegment::Type { bound_types, .. }) = &mut path.root.last_mut() {
                bound_types.push(type_variable_type);
            }

            tracing::info!(
                type_var = type_variable.to_string(),
                variant = path.to_wide_string(),
                type_parameter = self.tcx.hir_path_of_node(type_parameter).to_string(),
                location = location.to_string(),
                "introduce_type_var",
            );

            self.add_affected_node(node_id, type_variable);
        }

        Ok(())
    }
}

fn allocate_type_variable(hir: &mut lume_hir::Map, binding: TypeId, location: Location) -> TypeVariableId {
    let next_id = hir.nodes.last().unwrap().0.next();
    let type_definition = lume_hir::Node::TypeVariable(lume_hir::TypeVariable {
        id: next_id,
        binding,
        location,
    });

    assert!(hir.nodes.insert(next_id, type_definition).is_none());

    TypeVariableId(TypeId::from(next_id))
}

fn type_variable_as_type(hir: &lume_hir::Map, id: TypeVariableId) -> lume_hir::Type {
    let type_variable = hir.expect_type_variable(id.0.as_node_id()).unwrap();
    let type_variable_name = lume_hir::PathSegment::Type {
        name: id.to_string().into(),
        bound_types: Vec::new(),
        location: type_variable.location,
    };

    lume_hir::Type {
        id: id.0,
        name: lume_hir::Path::rooted(type_variable_name),
        self_type: false,
        location: type_variable.location,
    }
}
