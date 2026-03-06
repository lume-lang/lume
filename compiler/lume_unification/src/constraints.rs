use lume_errors::Result;
use lume_hir::TypeId;
use lume_span::{Location, NodeId};
use lume_types::TypeRef;

use crate::{TypeVariableId, UnificationPass, is_type_contained_within, verify};

/// Denotes all available types which can be inferred and unified.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum InferedNode {
    Cast(lume_hir::Cast),
    Construct(lume_hir::Construct),
    VariableDeclaration(lume_hir::VariableDeclaration),
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Constraint {
    Equal { lhs: TypeRef, rhs: TypeRef },
    Subtype { of: TypeRef, param: NodeId },
}

impl UnificationPass<'_> {
    #[tracing::instrument(level = "INFO", skip_all, err)]
    pub(crate) fn create_constraints(&mut self) -> Result<()> {
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
                    if let lume_hir::PatternKind::Variant(variant) = &pattern.kind {
                        self.create_constraints_from_variant(pattern.id, &variant.name)?;
                    }
                }
                InferedNode::VariableDeclaration(decl) => {
                    if let Some(declared_type) = &decl.declared_type {
                        verify::verify_type_name(self.tcx, &declared_type.name, declared_type.location);
                    }
                }
                InferedNode::Cast(expr) => {
                    self.create_constraints_from_type(expr.id, &expr.target.name)?;
                }
                InferedNode::Construct(expr) => {
                    self.create_constraints_from_type(expr.id, &expr.path)?;
                }
                InferedNode::StaticCall(expr) => {
                    self.create_constraints_from_callable(expr.id, &expr.name)?;
                }
                InferedNode::Variant(expr) => {
                    self.create_constraints_from_variant(expr.id, &expr.name)?;
                }
            }
        }

        Ok(())
    }

    #[tracing::instrument(level = "TRACE", skip_all, err)]
    fn create_constraints_from_type(&mut self, expr: NodeId, path: &lume_hir::Path) -> Result<()> {
        let location = self.tcx.hir_span_of_node(expr);

        let Some(type_def) = self.tcx.tdb().find_type(path) else {
            tracing::warn!(path = %path.to_wide_string(), "type_not_found");
            return Ok(());
        };

        let type_def_id = type_def.id;
        let declared_type_args = path.bound_types().len();
        let struct_definition = self.tcx.hir_expect_struct(type_def.id);

        for type_param_id in struct_definition
            .type_parameters
            .clone()
            .into_iter()
            .skip(declared_type_args)
        {
            let type_variable = allocate_type_variable(self.tcx.hir_mut(), type_param_id.into(), location);

            tracing::info!(
                type_var = type_variable.to_string(),
                type_def = self.tcx.hir_path_of_node(type_def_id).to_wide_string(),
                type_parameter = self.tcx.hir_path_of_node(type_param_id).to_string(),
                location = location.to_string(),
                "introduce_type_var",
            );

            self.create_constraints_of(expr, path, type_variable)?;
        }

        Ok(())
    }

    #[tracing::instrument(level = "TRACE", skip_all, err)]
    fn create_constraints_from_callable(&mut self, expr: NodeId, path: &lume_hir::Path) -> Result<()> {
        let location = self.tcx.hir_span_of_node(expr);

        let Some(callable) = self.tcx.callable_with_name(path) else {
            tracing::warn!(path = %path.to_wide_string(), "callable_not_found");
            return Ok(());
        };

        let signature = self.tcx.signature_of(callable)?;

        let path_segments = path.segments();
        let callable_segments = callable.name().segments().into_iter().cloned().collect::<Vec<_>>();

        for (idx, callable_segment) in callable_segments.into_iter().enumerate() {
            let declared_type_args = path_segments[idx].bound_types().len();

            let bound_types = callable_segment
                .bound_types()
                .iter()
                .skip(declared_type_args)
                .map(|ty| ty.id)
                .collect::<Vec<_>>();

            for type_param_id in bound_types {
                let type_variable = allocate_type_variable(self.tcx.hir_mut(), type_param_id, location);

                tracing::info!(
                    type_var = type_variable.to_string(),
                    callable = self.tcx.hir_path_of_node(signature.id).to_wide_string(),
                    type_parameter = self.tcx.hir_path_of_node(type_param_id.as_node_id()).to_string(),
                    location = location.to_string(),
                    "introduce_type_var",
                );

                self.create_constraints_of(expr, path, type_variable)?;
            }
        }

        Ok(())
    }

    #[tracing::instrument(level = "TRACE", skip_all, err)]
    fn create_constraints_from_variant(&mut self, expr: NodeId, path: &lume_hir::Path) -> Result<()> {
        let location = self.tcx.hir_span_of_node(expr);

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

        for type_param_id in bound_types {
            let type_variable = allocate_type_variable(self.tcx.hir_mut(), type_param_id.into(), location);

            tracing::info!(
                type_var = type_variable.to_string(),
                variant = path.to_wide_string(),
                type_parameter = self.tcx.hir_path_of_node(type_param_id).to_string(),
                location = location.to_string(),
                "introduce_type_var",
            );

            self.create_constraints_of(expr, path, type_variable)?;
        }

        Ok(())
    }
}

impl UnificationPass<'_> {
    #[tracing::instrument(level = "TRACE", skip_all, err)]
    fn create_constraints_of(&self, expr: NodeId, path: &lume_hir::Path, type_variable: TypeVariableId) -> Result<()> {
        self.ensure_entry_for(type_variable);

        let type_parameter_id = self
            .tcx
            .hir()
            .expect_type_variable(type_variable.0.as_node_id())?
            .binding;

        match &path.name {
            lume_hir::PathSegment::Type { .. } => {
                let Some(type_def) = self.tcx.tdb().find_type(path) else {
                    return Ok(());
                };

                for bound_type in type_def.name.all_bound_types() {
                    if bound_type.id != type_parameter_id {
                        continue;
                    }

                    let type_param_id = bound_type.id.as_node_id();
                    let type_param = self.tcx.hir_expect_type_parameter(type_param_id);

                    for type_param_constraint in &type_param.constraints {
                        let constraint_type = self.tcx.mk_type_ref_from(type_param_constraint, type_param.id)?;

                        self.sub(type_variable, constraint_type, type_param.id);
                    }
                }
            }
            lume_hir::PathSegment::Callable { .. } => {
                let Some(callable) = self.tcx.callable_with_name(path) else {
                    return Ok(());
                };

                let Some(call_expr) = self.tcx.hir_call_expr(expr) else {
                    return Ok(());
                };

                let args = call_expr.arguments();
                let params = self.tcx.signature_of(callable)?.params;

                for (param, arg) in params.into_iter().zip(args) {
                    if !is_type_contained_within(type_parameter_id, &param.ty) {
                        continue;
                    }

                    self.eq(type_variable, self.tcx.type_of(arg)?, param.ty.clone());
                }

                for bound_type in callable.name().all_bound_types() {
                    if bound_type.id != type_parameter_id {
                        continue;
                    }

                    let type_param_id = bound_type.id.as_node_id();
                    let type_param = self.tcx.hir_expect_type_parameter(type_param_id);

                    for type_param_constraint in &type_param.constraints {
                        let constraint_type = self.tcx.mk_type_ref_from(type_param_constraint, type_param.id)?;

                        self.sub(type_variable, constraint_type, type_param.id);
                    }
                }
            }
            lume_hir::PathSegment::Variant { .. } => {
                let parent_path = path
                    .clone()
                    .parent()
                    .expect("expected Variant path segment to have Type parent");

                let enum_def = self.tcx.enum_def_with_name(&parent_path)?;
                let enum_case_def = self.tcx.enum_case_with_name(path)?;

                let arguments = match &self.tcx.hir_expect_node(expr) {
                    lume_hir::Node::Expression(expr) => {
                        if let lume_hir::ExpressionKind::Variant(variant) = &expr.kind {
                            let argument_ids = &variant.arguments;

                            argument_ids
                                .iter()
                                .map(|id| self.tcx.type_of(*id))
                                .collect::<Result<Vec<_>>>()?
                        } else {
                            return Ok(());
                        }
                    }
                    lume_hir::Node::Pattern(pattern) => {
                        if let lume_hir::PatternKind::Variant(pattern) = &pattern.kind {
                            let field_ids = &pattern.fields;

                            field_ids
                                .iter()
                                .map(|subpattern| self.tcx.type_of_pattern(subpattern))
                                .collect::<Result<Vec<_>>>()?
                        } else {
                            return Ok(());
                        }
                    }
                    _ => return Ok(()),
                };

                let params = &enum_case_def.parameters;

                for (param, arg) in params.iter().zip(arguments) {
                    let param_ty = self.tcx.mk_type_ref_from(param, enum_def.id)?;

                    if !is_type_contained_within(type_parameter_id, &param_ty) {
                        continue;
                    }

                    self.eq(type_variable, arg, param_ty);
                }

                for bound_type in enum_def.name().all_bound_types() {
                    if bound_type.id != type_parameter_id {
                        continue;
                    }

                    let type_param_id = bound_type.id.as_node_id();
                    let type_param = self.tcx.hir_expect_type_parameter(type_param_id);

                    for type_param_constraint in &type_param.constraints {
                        let constraint_type = self.tcx.mk_type_ref_from(type_param_constraint, type_param.id)?;

                        self.sub(type_variable, constraint_type, type_param.id);
                    }
                }
            }
            lume_hir::PathSegment::Namespace { .. } => {}
        }

        if let Some(lume_hir::Node::Pattern(pattern)) = self.tcx.hir_node(expr)
            && let Some(expected_type) = self.tcx.expected_type_of(expr)?
        {
            self.eq(type_variable, self.tcx.type_of_pattern(pattern)?, expected_type);
        }

        if self.tcx.hir_expr(expr).is_some()
            && !path.is_variant()
            && let Some(expected_type) = self.tcx.expected_type_of(expr)?
        {
            self.eq(type_variable, self.tcx.type_of(expr)?, expected_type);
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
