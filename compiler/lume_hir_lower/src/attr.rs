use crate::*;

impl LoweringContext<'_> {
    /// Handles the given attributes for the current node.
    ///
    /// Since this methods uses the current node, ensure that this method is
    /// called before any other node IDs are assigned.
    pub(crate) fn attributes<I>(&mut self, attrs: I) -> Vec<lume_hir::Attribute>
    where
        I: IntoIterator<Item = lume_ast::Attr>,
    {
        let current_node = self.current_node;
        let mut hir_attrs = Vec::new();

        for attr in attrs {
            let Some(name) = attr.name() else { continue };

            let name = self.ident(name);
            let arguments = if let Some(arg_list) = attr.arg_list() {
                arg_list
                    .args()
                    .map(|arg| {
                        let name = self.ident_opt(arg.name());
                        let value = self.literal_opt(arg.value());
                        let location = self.location(arg.location());

                        lume_hir::AttrArgument { name, value, location }
                    })
                    .collect::<Vec<_>>()
            } else {
                Vec::new()
            };

            let location = self.location(attr.location());

            let attr = lume_hir::Attribute {
                name,
                arguments,
                location,
            };

            match attr.name.as_str() {
                "lang_item" => {
                    let Some(name_arg) = attr.arguments.iter().find(|arg| arg.name.as_str() == "name") else {
                        self.dcx.emit_and_push(
                            errors::LangItemMissingName {
                                location: attr.location,
                            }
                            .into(),
                        );

                        continue;
                    };

                    let lume_hir::LiteralKind::String(lit_value) = &name_arg.value.kind else {
                        self.dcx.emit_and_push(
                            errors::LangItemInvalidNameType {
                                location: name_arg.value.location,
                            }
                            .into(),
                        );

                        continue;
                    };

                    if let Err(err) = self.map.lang_items.add_name(&lit_value.value, current_node) {
                        self.dcx.emit_and_push(err);
                    }
                }

                "external" | "intrinsic" => {
                    hir_attrs.push(attr);
                }

                _ => {
                    self.dcx.emit_and_push(
                        errors::UnknownAttribute {
                            location: attr.location,
                        }
                        .into(),
                    );
                }
            }
        }

        hir_attrs
    }
}
