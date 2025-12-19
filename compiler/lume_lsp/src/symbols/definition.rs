use lume_hir::WithLocation;
use lume_span::Location;

use crate::engine::Engine;
use crate::symbols::lookup::SymbolKind;

pub(crate) fn definition_of(engine: &Engine, location: Location) -> Option<Location> {
    let Some(sym) = engine.locate_node(location) else {
        log::warn!("could not find matching node for {location}");
        return None;
    };

    let package = engine.package(sym.location.file.package)?;

    match &sym.kind {
        SymbolKind::Type { name } => {
            let type_id = package.tcx.tdb().find_type(name).map(|ty| ty.id)?;
            let owning_package = engine.package(type_id.package)?;

            let lume_hir::Node::Type(type_def) = owning_package.tcx.hir_node(type_id)? else {
                return None;
            };

            Some(type_def.location())
        }
        &SymbolKind::Callable { reference } => {
            let callable = package.tcx.callable_of(reference).ok()?;

            Some(callable.name().location)
        }
        SymbolKind::Variant { name } => {
            let enum_name = name.clone().parent().unwrap();
            let enum_def = package.tcx.enum_def_with_name(&enum_name).ok()?;

            Some(enum_def.location)
        }
        &SymbolKind::Call { id } => {
            let call_expr = package.tcx.hir_call_expr(id)?;
            let callable = package.tcx.lookup_callable(call_expr).ok()?;

            Some(callable.name().location)
        }
        &SymbolKind::Pattern { id } => {
            let pattern = package.tcx.hir_expect_pattern(id);

            match &pattern.kind {
                lume_hir::PatternKind::Literal(_)
                | lume_hir::PatternKind::Identifier(_)
                | lume_hir::PatternKind::Wildcard(_) => None,
                lume_hir::PatternKind::Variant(pattern) => {
                    let enum_case_def = package.tcx.enum_case_with_name(&pattern.name).ok()?;

                    Some(enum_case_def.location)
                }
            }
        }
        SymbolKind::Member { callee, field } => {
            let callee_type = package.tcx.type_of(*callee).ok()?;
            let field = package.tcx.field_on(callee_type.instance_of, &field.name).ok()??;

            Some(field.name.location)
        }
        &SymbolKind::VariableReference { id } => {
            let lume_hir::ExpressionKind::Variable(variable_ref) = &package.tcx.hir_expect_expr(id).kind else {
                return None;
            };

            match &variable_ref.reference {
                lume_hir::VariableSource::Parameter(param) => Some(param.name.location),
                lume_hir::VariableSource::Variable(var_decl) => Some(var_decl.name.location),
                lume_hir::VariableSource::Pattern(pattern) => Some(pattern.location),
            }
        }
        SymbolKind::VariableDeclaration { .. } | SymbolKind::Field { .. } | SymbolKind::Literal { .. } => None,
    }
}
