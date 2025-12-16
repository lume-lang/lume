use lume_errors::Result;
use lume_hir::Identifier;
use lume_infer::query::CallReference;
use lume_span::{Location, NodeId};

use crate::state::State;
use crate::symbols::lookup::SymbolKind;

pub(crate) fn hover_content_of(state: &State, location: Location) -> Result<String> {
    let Some(sym) = state.checked.symbols.lookup_position(location) else {
        log::warn!("could not find matching node for {location}");
        return Ok(String::new());
    };

    match &sym.kind {
        SymbolKind::Type { name } => hover_content_of_type(state, location, name),
        SymbolKind::Callable { reference } => hover_content_of_callable(state, location, *reference),
        SymbolKind::Variant { name } => hover_content_of_variant(state, location, name),
        SymbolKind::Pattern { id } => hover_content_of_pattern(state, location, *id),
        SymbolKind::Field { id } => hover_content_of_field(state, location, *id),
        SymbolKind::Call { id } => hover_content_of_call(state, location, *id),
        SymbolKind::Literal { id } => hover_content_of_literal(state, location, *id),
        SymbolKind::Member { callee, field } => hover_content_of_member(state, location, *callee, field),
        SymbolKind::VariableDeclaration { id } => hover_content_of_variable_decl(state, location, *id),
        SymbolKind::VariableReference { id } => hover_content_of_variable_ref(state, location, *id),
    }
}

pub(crate) fn hover_content_of_type(state: &State, location: Location, type_name: &lume_hir::Path) -> Result<String> {
    let package = state.checked.graph.packages.get(&location.file.package).unwrap();
    let Some(type_id) = package.tcx.tdb().find_type(type_name).map(|ty| ty.id) else {
        return Ok(String::new());
    };

    let Some(lume_hir::Node::Type(type_def)) = package.tcx.hir_node(type_id) else {
        return Ok(String::new());
    };

    let documentation = match package.tcx.documentation_string_of(type_id) {
        Some(str) => format!("\n\n{str}"),
        None => String::new(),
    };

    match type_def {
        lume_hir::TypeDefinition::Struct(struct_def) => {
            let builtin = if struct_def.builtin {
                String::from("builtin ")
            } else {
                String::new()
            };

            Ok(format!(
                "```lm\n{} struct {builtin}{:+}\n```{documentation}",
                struct_def.visibility, struct_def.name
            ))
        }
        lume_hir::TypeDefinition::Trait(trait_def) => Ok(format!(
            "```lm\n{} trait {:+}\n```{documentation}",
            trait_def.visibility, trait_def.name
        )),
        lume_hir::TypeDefinition::Enum(enum_def) => Ok(format!(
            "```lm\n{} enum {:+}\n```{documentation}",
            enum_def.visibility, enum_def.name
        )),
        lume_hir::TypeDefinition::TypeParameter(type_param) => {
            Ok(format!("```lm\n{}\n```{documentation}", type_param.name))
        }
    }
}

pub(crate) fn hover_content_of_callable(state: &State, location: Location, reference: CallReference) -> Result<String> {
    let package = state.checked.graph.packages.get(&location.file.package).unwrap();
    let callable = package.tcx.callable_of(reference)?;

    let identifier = lume_hir::Identifier {
        name: format!("{:+}", callable.name()),
        location: callable.name().location,
    };

    let signature = package.tcx.signature_of(callable)?;
    let signature = package.tcx.sig_to_string(&identifier, signature.as_ref(), false)?;

    let visibility = match package.tcx.visibility_of(callable.id()) {
        Some(visibility) => format!("{visibility} "),
        None => String::new(),
    };

    let documentation = match package.tcx.documentation_string_of(callable.id()) {
        Some(str) => format!("\n\n{str}"),
        None => String::new(),
    };

    Ok(format!("```lm\n{visibility}{signature}\n```{documentation}"))
}

pub(crate) fn hover_content_of_member(
    state: &State,
    location: Location,
    callee: NodeId,
    field: &Identifier,
) -> Result<String> {
    let package = state.checked.graph.packages.get(&location.file.package).unwrap();

    let callee_type = package.tcx.type_of(callee)?;
    let Some(field) = package.tcx.field_on(callee_type.instance_of, &field.name)? else {
        return Ok(String::new());
    };

    let field_type_ref = package.tcx.mk_type_ref_from(&field.field_type, field.id)?;
    let field_type = package.tcx.new_named_type(&field_type_ref, true)?;

    let documentation = match package.tcx.documentation_string_of(field.id) {
        Some(str) => format!("\n\n{str}"),
        None => String::new(),
    };

    Ok(format!(
        "```lm\n{} {}: {field_type};\n```{documentation}",
        field.visibility, field.name
    ))
}

pub(crate) fn hover_content_of_variant(state: &State, location: Location, name: &lume_hir::Path) -> Result<String> {
    let package = state.checked.graph.packages.get(&location.file.package).unwrap();

    let enum_name = name.clone().parent().unwrap();
    let enum_def = package.tcx.enum_def_with_name(&enum_name)?;
    let enum_case = package.tcx.enum_case_with_name(name)?;

    let fields = if enum_case.parameters.is_empty() {
        String::new()
    } else {
        let fields = enum_case
            .parameters
            .iter()
            .map(std::string::ToString::to_string)
            .collect::<Vec<String>>()
            .join(", ");

        format!("({fields})")
    };

    let documentation = match package.tcx.documentation_string_of(enum_def.id) {
        Some(str) => format!("\n\n{str}"),
        None => String::new(),
    };

    Ok(format!(
        "```lm\n{:+}::{}{fields}\n```{documentation}",
        enum_def.name, enum_case.name
    ))
}

pub(crate) fn hover_content_of_pattern(state: &State, location: Location, id: NodeId) -> Result<String> {
    let package = state.checked.graph.packages.get(&location.file.package).unwrap();

    let Some(lume_hir::Node::Pattern(pattern)) = package.tcx.hir_node(id) else {
        return Ok(String::new());
    };

    let pattern_ty = package.tcx.type_of_pattern(pattern)?;
    let pattern_ty_name = package.tcx.new_named_type(&pattern_ty, true)?;

    let documentation = match package.tcx.documentation_string_of(pattern_ty.instance_of) {
        Some(str) => format!("\n\n{str}"),
        None => String::new(),
    };

    Ok(format!("```lm\n{pattern_ty_name}\n```{documentation}"))
}

pub(crate) fn hover_content_of_field(state: &State, location: Location, id: NodeId) -> Result<String> {
    let package = state.checked.graph.packages.get(&location.file.package).unwrap();

    let Some(lume_hir::Node::Field(field)) = package.tcx.hir_node(id) else {
        return Ok(String::new());
    };

    let struct_def = package.tcx.owning_struct_of_field(id)?;
    let field_type_ref = package.tcx.mk_type_ref_from(&field.field_type, struct_def.id)?;
    let field_type = package.tcx.new_named_type(&field_type_ref, true)?;

    let documentation = match package.tcx.documentation_string_of(id) {
        Some(str) => format!("\n\n{str}"),
        None => String::new(),
    };

    Ok(format!(
        "```lm\n{:+}\n\n{}: {field_type};\n```{documentation}",
        struct_def.name, field.name
    ))
}

pub(crate) fn hover_content_of_literal(state: &State, location: Location, id: NodeId) -> Result<String> {
    let package = state.checked.graph.packages.get(&location.file.package).unwrap();
    let literal_type = package.tcx.type_of(id)?;

    let Some(literal_type_def) = package.tcx.tdb().type_(literal_type.instance_of) else {
        return Ok(String::new());
    };

    hover_content_of_type(state, location, &literal_type_def.name)
}

pub(crate) fn hover_content_of_call(state: &State, location: Location, id: NodeId) -> Result<String> {
    let package = state.checked.graph.packages.get(&location.file.package).unwrap();
    let Some(expr) = package.tcx.hir_call_expr(id) else {
        return Ok(String::new());
    };

    let callable = package.tcx.probe_callable(expr)?;

    hover_content_of_callable(state, location, callable.to_call_reference())
}

pub(crate) fn hover_content_of_variable_decl(state: &State, location: Location, id: NodeId) -> Result<String> {
    let package = state.checked.graph.packages.get(&location.file.package).unwrap();

    let Some(lume_hir::StatementKind::Variable(var_decl)) = package.tcx.hir_stmt(id).map(|e| &e.kind) else {
        return Ok(String::new());
    };

    let variable_type = package.tcx.type_of_vardecl(var_decl)?;
    let variable_name = var_decl.name.as_str();
    let variable_type_name = package.tcx.new_named_type(&variable_type, true)?;

    Ok(format!("```lm\nlet {variable_name}: {variable_type_name};\n```"))
}

pub(crate) fn hover_content_of_variable_ref(state: &State, location: Location, id: NodeId) -> Result<String> {
    let package = state.checked.graph.packages.get(&location.file.package).unwrap();

    let Some(lume_hir::ExpressionKind::Variable(variable_ref)) = package.tcx.hir_expr(id).map(|e| &e.kind) else {
        return Ok(String::new());
    };

    let variable_type = match &variable_ref.reference {
        lume_hir::VariableSource::Variable(var_decl) => package.tcx.type_of_vardecl(var_decl)?,
        lume_hir::VariableSource::Parameter(param) => package.tcx.mk_type_ref_from(&param.param_type, id)?,
        lume_hir::VariableSource::Pattern(pattern) => package.tcx.type_of_pattern(pattern)?,
    };

    let variable_name = variable_ref.name.as_str();
    let variable_type_name = package.tcx.new_named_type(&variable_type, true)?;

    Ok(format!("```lm\nlet {variable_name}: {variable_type_name};\n```"))
}
