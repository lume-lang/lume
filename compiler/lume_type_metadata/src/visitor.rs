use std::collections::HashSet;

use lume_errors::Result;
use lume_infer::TyInferCtx;
use lume_span::NodeId;

/// Visitor for traversing types and related nodes in a type context.
///
/// This trait is used with the [`traverse`] function.
pub trait Visitor {
    fn visit_type(&mut self, ty: &lume_types::TypeRef) -> Result<()>;

    fn visit_field(&mut self, field: &lume_hir::Field) -> Result<()>;

    fn visit_method(&mut self, method: &lume_types::Method) -> Result<()>;

    fn visit_parameter(&mut self, parameter: &lume_types::Parameter) -> Result<()>;

    fn visit_type_parameter(&mut self, type_param: &lume_hir::TypeParameter) -> Result<()>;
}

struct State<'tcx> {
    tcx: &'tcx TyInferCtx,
    visited: HashSet<NodeId>,
}

/// Traverses the given context using the provided visitor.
pub fn traverse<V: Visitor>(tcx: &TyInferCtx, visitor: &mut V) -> Result<()> {
    let mut state = State {
        tcx,
        visited: HashSet::new(),
    };

    for ty in tcx.db().types() {
        let type_ref = lume_types::TypeRef::new(ty.id, ty.name.location);

        visit_type(&mut state, visitor, &type_ref)?;
    }

    Ok(())
}

fn visit_type<V: Visitor>(state: &mut State<'_>, visitor: &mut V, ty: &lume_types::TypeRef) -> Result<()> {
    if !state.visited.insert(ty.instance_of) {
        return Ok(());
    }

    visitor.visit_type(ty)?;

    if let Some(type_param) = state.tcx.as_type_param(ty.instance_of) {
        visitor.visit_type_parameter(type_param)?;
    }

    for bound_type in &ty.bound_types {
        visit_type(state, visitor, bound_type)?;
    }

    for field in state.tcx.fields_on(ty.instance_of)? {
        visit_field(state, visitor, field)?;
    }

    for method in state.tcx.methods_defined_on(ty) {
        visit_method(state, visitor, method)?;
    }

    for type_param_id in state.tcx.available_type_params_at(ty.instance_of) {
        let type_param = state.tcx.hir_expect_type_parameter(type_param_id);

        visit_type_parameter(state, visitor, type_param)?;
    }

    Ok(())
}

fn visit_field<V: Visitor>(state: &mut State<'_>, visitor: &mut V, field: &lume_hir::Field) -> Result<()> {
    if !state.visited.insert(field.id) {
        return Ok(());
    }

    visitor.visit_field(field)?;

    let field_type = state.tcx.mk_type_ref_from(&field.field_type, field.id)?;
    visit_type(state, visitor, &field_type)?;

    for type_param_id in state.tcx.available_type_params_at(field.id) {
        let type_param = state.tcx.hir_expect_type_parameter(type_param_id);

        visit_type_parameter(state, visitor, type_param)?;
    }

    Ok(())
}

fn visit_method<V: Visitor>(state: &mut State<'_>, visitor: &mut V, method: &lume_types::Method) -> Result<()> {
    if !state.visited.insert(method.id) {
        return Ok(());
    }

    visitor.visit_method(method)?;

    let signature = state.tcx.signature_of(lume_infer::query::Callable::Method(method))?;

    for parameter in &signature.params {
        visit_parameter(state, visitor, parameter)?;
    }

    for type_param_id in state.tcx.available_type_params_at(method.id) {
        let type_param = state.tcx.hir_expect_type_parameter(type_param_id);

        visit_type_parameter(state, visitor, type_param)?;
    }

    visit_type(state, visitor, &signature.ret_ty)?;

    Ok(())
}

fn visit_parameter<V: Visitor>(
    state: &mut State<'_>,
    visitor: &mut V,
    parameter: &lume_types::Parameter,
) -> Result<()> {
    visitor.visit_parameter(parameter)?;

    visit_type(state, visitor, &parameter.ty)?;

    Ok(())
}

fn visit_type_parameter<V: Visitor>(
    state: &mut State<'_>,
    visitor: &mut V,
    type_parameter: &lume_hir::TypeParameter,
) -> Result<()> {
    if !state.visited.insert(type_parameter.id) {
        return Ok(());
    }

    visitor.visit_type_parameter(type_parameter)?;

    for constraint in &type_parameter.constraints {
        let constraint = state.tcx.mk_type_ref_from(constraint, type_parameter.id)?;

        visit_type(state, visitor, &constraint)?;
    }

    Ok(())
}
