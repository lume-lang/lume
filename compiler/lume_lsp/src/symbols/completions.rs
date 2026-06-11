use lume_driver::CheckedPackage;
use lume_infer::IncludeReturnType;
use lume_infer::query::{CallReference, Callable};
use lume_span::{Location, NodeId};
use lume_types::TypeRef;

use crate::engine::Engine;
use crate::listener::{CompletionKind, FileLocation};

pub struct CompletionContext {
    /// Defines the kind of completion, based on the trigger character.
    pub kind: CompletionKind,

    /// Defines the character which triggered the completion, if any.
    pub trigger_character: Option<char>,

    /// Defines the location of the completion
    pub file_location: FileLocation,

    /// Defines the location of the completion
    pub location: Location,
}

impl CompletionContext {
    pub fn offset_location(&self) -> Location {
        if self.trigger_character.is_none() {
            return self.location;
        }

        let mut location = self.location.clone_inner();
        location.index.start = location.index.start.saturating_sub(1);
        location.index.end = location.index.end.saturating_sub(1);

        lume_span::Internable::intern(&location)
    }
}

pub(crate) fn completions_at(engine: &Engine, ctx: CompletionContext) -> Option<Vec<lsp_types::CompletionItem>> {
    let offset_location = ctx.offset_location();

    let Some(node_entry) = engine.locate_node(offset_location) else {
        tracing::warn!("could not find matching node for {offset_location}");
        return None;
    };

    let package = engine.package(node_entry.location.file.package)?;
    let mut completions = Vec::new();

    match ctx.kind {
        CompletionKind::Instance => {
            let node_type = package.tcx.type_of(node_entry.id).ok()?;

            completions.extend(field_completions_of(package, node_type.instance_of));
            completions.extend(method_completions_of(package, &ctx, &node_type));
        }
        CompletionKind::Static => {
            tracing::info!("found node => {:?} ({})", node_entry.id, node_entry.location);
        }
        CompletionKind::Scope => {}
    }

    Some(completions)
}

fn field_completions_of(package: &CheckedPackage, node: NodeId) -> impl Iterator<Item = lsp_types::CompletionItem> {
    let fields_on_node = package.tcx.hir_fields_on(node).unwrap_or_default();

    fields_on_node.iter().map(|field| {
        let field_name = field.name.to_string();

        let field_type = package
            .tcx
            .mk_type_ref_from(&field.field_type, field.id)
            .and_then(|ty| package.tcx.new_named_type(&ty, false))
            .unwrap_or_else(|_| String::from("<unknown>"));

        let documentation = package.tcx.documentation_string_of(field.id).cloned().map(|value| {
            lsp_types::Documentation::MarkupContent(lsp_types::MarkupContent {
                kind: lsp_types::MarkupKind::Markdown,
                value,
            })
        });

        lsp_types::CompletionItem {
            label: field.name.name.clone(),
            kind: Some(lsp_types::CompletionItemKind::FIELD),
            label_details: Some(lsp_types::CompletionItemLabelDetails {
                detail: None,
                description: Some(field_type),
            }),
            sort_text: Some(format!("0_{field_name}")),
            documentation,
            ..Default::default()
        }
    })
}

fn method_completions_of(
    package: &CheckedPackage,
    ctx: &CompletionContext,
    ty: &TypeRef,
) -> impl Iterator<Item = lsp_types::CompletionItem> {
    let methods_on_type = package.tcx.methods_defined_on(ty);
    let position = ctx.file_location.position;

    methods_on_type.into_iter().filter_map(move |method| {
        let method_name = method.name.name();
        let method_signature = package.tcx.signature_of(Callable::Method(method)).ok()?;

        let details = package
            .tcx
            .fn_stringifier(CallReference::Method(method.id))
            .include_parameters(true)
            .include_return_type(IncludeReturnType::NonVoid)
            .include_visibility(false)
            .stringify()
            .ok()?;

        let label_details = {
            let details = package
                .tcx
                .fn_stringifier(CallReference::Method(method.id))
                .include_name(false)
                .include_parameters(true)
                .include_return_type(IncludeReturnType::NonVoid)
                .include_visibility(false)
                .stringify()
                .ok()?;

            let description = package.tcx.new_named_type(&method_signature.ret_ty, false).ok()?;

            Some(lsp_types::CompletionItemLabelDetails {
                detail: Some(details),
                description: Some(description),
            })
        };

        let text_edit_args = method_signature
            .params
            .iter()
            .filter_map(|param| {
                // Skip the `self` parameter if completion is triggered on an instance
                if param.is_self() && ctx.kind == CompletionKind::Instance {
                    return None;
                }

                Some(format!("${{{idx}:{name}}}", idx = param.idx, name = param.name))
            })
            .collect::<Vec<String>>()
            .join(", ");

        let documentation = package.tcx.documentation_string_of(method.id).cloned().map(|value| {
            lsp_types::Documentation::MarkupContent(lsp_types::MarkupContent {
                kind: lsp_types::MarkupKind::Markdown,
                value,
            })
        });

        Some(lsp_types::CompletionItem {
            label: method_name.to_string(),
            kind: Some(lsp_types::CompletionItemKind::METHOD),
            insert_text_format: Some(lsp_types::InsertTextFormat::SNIPPET),
            insert_text_mode: Some(lsp_types::InsertTextMode::ADJUST_INDENTATION),
            detail: Some(details),
            sort_text: Some(format!("1_{method_name}")),
            label_details,
            text_edit: Some(lsp_types::CompletionTextEdit::InsertAndReplace(
                lsp_types::InsertReplaceEdit {
                    new_text: format!("{method_name}({text_edit_args})$0"),
                    insert: lsp_types::Range {
                        start: position,
                        end: position,
                    },
                    replace: lsp_types::Range {
                        start: position,
                        end: position,
                    },
                },
            )),
            documentation,
            ..Default::default()
        })
    })
}
