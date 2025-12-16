use lume_errors::Result;
use lume_hir::WithLocation;
use lume_span::Location;

use crate::state::State;
use crate::symbols::lookup::SymbolKind;

pub(crate) fn definition_of(state: &State, location: Location) -> Result<Option<Location>> {
    let package = state.checked.graph.packages.get(&location.file.package).unwrap();

    let Some(sym) = state.checked.symbols.lookup_position(location) else {
        log::warn!("could not find matching node for {location}");
        return Ok(None);
    };

    match &sym.kind {
        SymbolKind::Type { name } => {
            let Some(type_id) = package.tcx.tdb().find_type(name).map(|ty| ty.id) else {
                return Ok(None);
            };

            let package = state.checked.graph.packages.get(&type_id.package).unwrap();
            let Some(lume_hir::Node::Type(type_def)) = package.tcx.hir_node(type_id) else {
                return Ok(None);
            };

            Ok(Some(type_def.location()))
        }
        &SymbolKind::Callable { reference } => {
            let callable = package.tcx.callable_of(reference)?;
            let location = package.tcx.hir_span_of_node(callable.id());

            Ok(Some(location))
        }
        SymbolKind::Variant { name } => {
            let enum_name = name.clone().parent().unwrap();
            let enum_def = package.tcx.enum_def_with_name(&enum_name)?;

            Ok(Some(enum_def.location))
        }
        &SymbolKind::Call { id } => {
            let package = state.checked.graph.packages.get(&id.package).unwrap();
            let Some(call_expr) = package.tcx.hir_call_expr(id) else {
                return Ok(None);
            };

            let callable = package.tcx.lookup_callable(call_expr)?;
            let location = package.tcx.hir_span_of_node(callable.id());

            Ok(Some(location))
        }
        &SymbolKind::Pattern { id } => {
            let package = state.checked.graph.packages.get(&id.package).unwrap();
            let pattern = package.tcx.hir_expect_pattern(id);

            match &pattern.kind {
                lume_hir::PatternKind::Literal(_)
                | lume_hir::PatternKind::Identifier(_)
                | lume_hir::PatternKind::Wildcard(_) => Ok(None),
                lume_hir::PatternKind::Variant(pattern) => {
                    let enum_case_def = package.tcx.enum_case_with_name(&pattern.name)?;

                    Ok(Some(enum_case_def.location))
                }
            }
        }
        SymbolKind::Member { callee, field } => {
            let callee_type = package.tcx.type_of(*callee)?;
            let Some(field) = package.tcx.field_on(callee_type.instance_of, &field.name)? else {
                return Ok(None);
            };

            let location = package.tcx.hir_span_of_node(field.id);

            Ok(Some(location))
        }
        &SymbolKind::VariableReference { id } => {
            let lume_hir::ExpressionKind::Variable(variable_ref) = &package.tcx.hir_expect_expr(id).kind else {
                return Ok(None);
            };

            match &variable_ref.reference {
                lume_hir::VariableSource::Parameter(param) => Ok(Some(param.location)),
                lume_hir::VariableSource::Variable(var_decl) => Ok(Some(var_decl.location)),
                lume_hir::VariableSource::Pattern(pattern) => Ok(Some(pattern.location)),
            }
        }
        SymbolKind::VariableDeclaration { .. } | SymbolKind::Field { .. } | SymbolKind::Literal { .. } => Ok(None),
    }
}
