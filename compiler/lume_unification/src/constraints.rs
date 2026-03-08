use lume_errors::Result;
use lume_hir::TypeId;
use lume_span::NodeId;
use lume_types::TypeRef;

use crate::introduce::InferedNodeRef;
use crate::{TypeVariableId, UnificationPass};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Constraint {
    Equal { lhs: TypeRef, rhs: TypeRef },
    Subtype { of: TypeRef, param: NodeId },
}

impl UnificationPass<'_> {
    #[tracing::instrument(level = "INFO", skip_all, err)]
    pub(crate) fn create_constraints(&self) -> Result<()> {
        for (id, type_variable) in self.affected_nodes() {
            if let Err(err) = self.create_constraints_for(id, type_variable) {
                self.tcx.dcx().emit(err);
            }
        }

        Ok(())
    }

    #[tracing::instrument(level = "TRACE", skip_all, err)]
    fn create_constraints_for(&self, node_id: NodeId, type_variable: TypeVariableId) -> Result<()> {
        self.env.try_write().unwrap().ensure_entry_for(type_variable);

        let node = if let Some(node) = self.tcx.hir_node(node_id)
            && let Ok(inferred) = InferedNodeRef::try_from(node)
        {
            inferred
        } else {
            return Ok(());
        };

        let type_parameter_id = self
            .tcx
            .hir()
            .expect_type_variable(type_variable.0.as_node_id())?
            .binding;

        match node {
            InferedNodeRef::Pattern(pattern) => {
                if let lume_hir::PatternKind::Variant(variant) = &pattern.kind {
                    self.create_constraints_for_variant(pattern.id, &variant.name, type_parameter_id, type_variable)?;
                }
            }
            InferedNodeRef::VariableDeclaration(_decl) => {}
            InferedNodeRef::Cast(expr) => {
                self.create_constraints_for_type(&expr.target.name, type_parameter_id, type_variable)?;
            }
            InferedNodeRef::Construct(expr) => {
                self.create_constraints_for_type(&expr.path, type_parameter_id, type_variable)?;
            }
            InferedNodeRef::InstanceCall(expr) => {
                self.create_constraints_for_callable(
                    lume_hir::CallExpression::Instanced(expr),
                    type_parameter_id,
                    type_variable,
                )?;
            }
            InferedNodeRef::StaticCall(expr) => {
                self.create_constraints_for_callable(
                    lume_hir::CallExpression::Static(expr),
                    type_parameter_id,
                    type_variable,
                )?;
            }
            InferedNodeRef::Variant(expr) => {
                self.create_constraints_for_variant(expr.id, &expr.name, type_parameter_id, type_variable)?;
            }
        }

        Ok(())
    }

    #[tracing::instrument(level = "TRACE", skip_all, err)]
    fn create_constraints_for_type(
        &self,
        path: &lume_hir::Path,
        type_parameter_id: TypeId,
        type_variable: TypeVariableId,
    ) -> Result<()> {
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

        Ok(())
    }

    #[tracing::instrument(
        level = "TRACE",
        skip_all,
        fields(
            name = %call.name(),
            location = %self.tcx.hir_span_of_node(call.id()),
        ),
        err
    )]
    fn create_constraints_for_callable(
        &self,
        call: lume_hir::CallExpression,
        type_parameter_id: TypeId,
        type_variable: TypeVariableId,
    ) -> Result<()> {
        let callable = self.tcx.probe_callable(call)?;
        let signature = self.tcx.signature_of(callable)?;

        let type_parameters = self.tcx.available_type_params_at(callable.id());
        let type_arguments = call.all_type_arguments();

        let mut arguments = call.arguments();
        let parameters = signature.params.as_slice();

        if let lume_hir::CallExpression::Instanced(instance_call) = call {
            arguments.insert(0, instance_call.callee);
        }

        assert_eq!(
            type_parameters.len(),
            type_arguments.len(),
            "type variables to match type parameters on {}",
            call.location()
        );

        for (parameter, &argument) in self.tcx.zip_call_args(parameters, &arguments) {
            let argument_type = self.tcx.type_of(argument)?;

            // If any arguments within the argument list contain any type variables AND the
            // corresponding parameter contains the type parameter, we equate the two type
            // variables to be equal.
            //
            // For example, given a sample like this:
            // ```lm
            // fn main() {
            //     let arr = Array::new(); // ?T1 introduced here
            //     arr.push("Hello, world!"); // ?T2 introduced here
            // }
            // ```
            //
            // Since `?T1` and `?T2` refer to the same type parameter, and functions as the
            // same instance of the type argument here, the two type variables must equal.
            if let Some((argument_constraint, parameter_type)) =
                argument_type.corresponding_of(&parameter.ty, |ty| self.tcx.is_type_variable(ty))
                && parameter_type.instance_of == type_parameter_id.as_node_id()
            {
                let argument_type_var = self.tcx.as_type_variable(argument_constraint).unwrap();

                self.eq(
                    type_variable,
                    TypeRef::new(type_variable.0.as_node_id(), parameter.location),
                    TypeRef::new(argument_type_var.id, argument_type.location),
                );
            }

            // If the parameter holds the type parameter, which is guaranteed to resolve a
            // type variable, we ensure that the type variable is constrained to
            // the corresponding argument type.
            //
            // For example, given a sample like this:
            // ```lm
            // fn identity<T>(value: T) -> T {
            //     value
            // }
            //
            // fn main() {
            //     // implicit type variable inserted here for `identity::T`
            //     let _ = identity("Hello world!");
            // }
            // ```
            //
            // Since the parameter `value` contains the type parameter `T`, the argument
            // would constrain the type variable to equal `String`.
            if parameter.ty.contains(type_parameter_id) {
                self.eq(type_variable, parameter.ty.clone(), argument_type);
            }
        }

        for bound_type in type_parameters {
            if bound_type != type_parameter_id.as_node_id() {
                continue;
            }

            let type_param = self.tcx.hir_expect_type_parameter(bound_type);

            for type_param_constraint in &type_param.constraints {
                let constraint_type = self.tcx.mk_type_ref_from(type_param_constraint, type_param.id)?;

                self.sub(type_variable, constraint_type, type_param.id);
            }
        }

        Ok(())
    }

    #[tracing::instrument(level = "TRACE", skip_all, err)]
    fn create_constraints_for_variant(
        &self,
        node_id: NodeId,
        path: &lume_hir::Path,
        type_parameter_id: TypeId,
        type_variable: TypeVariableId,
    ) -> Result<()> {
        let parent_path = path
            .clone()
            .parent()
            .expect("expected Variant path segment to have Type parent");

        let enum_def = self.tcx.enum_def_with_name(&parent_path)?;
        let enum_case_def = self.tcx.enum_case_with_name(path)?;

        let arguments = match &self.tcx.hir_expect_node(node_id) {
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
            let parameter_type = self.tcx.mk_type_ref_from(param, enum_def.id)?;
            if !parameter_type.contains(type_parameter_id) {
                continue;
            }

            self.eq(type_variable, arg, parameter_type);
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

        Ok(())
    }
}
