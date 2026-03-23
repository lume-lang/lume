use crate::*;

impl LoweringContext<'_> {
    /// Handles the given attributes for the current node.
    ///
    /// Since this methods uses the current node, ensure that this method is
    /// called before any other node IDs are assigned.
    pub(crate) fn handle_attributes<I>(&mut self, attrs: I)
    where
        I: IntoIterator<Item = lume_ast::Attr>,
    {
        let current_node = self.current_node;

        for attr in attrs {
            let Some(name) = attr.name() else { continue };

            match name.as_text().as_ref() {
                "lang_item" => {
                    let Some(argument_list) = attr.arg_list() else {
                        continue;
                    };

                    let Some(name_arg) = argument_list
                        .args()
                        .find(|arg| arg.name().is_some_and(|n| n.syntax().text() == "name"))
                    else {
                        self.dcx.emit_and_push(
                            errors::LangItemMissingName {
                                source: self.current_file().clone(),
                                range: attr.range(),
                            }
                            .into(),
                        );

                        continue;
                    };

                    let Some(lang_name) = name_arg.syntax().descendants().find_map(lume_ast::StringLit::cast) else {
                        self.dcx.emit_and_push(
                            errors::LangItemInvalidNameType {
                                source: self.current_file().clone(),
                                range: attr.range(),
                            }
                            .into(),
                        );

                        continue;
                    };

                    if let Err(err) = self
                        .map
                        .lang_items
                        .add_name(lang_name.as_text().trim_matches('"'), current_node)
                    {
                        self.dcx.emit_and_push(err);
                    }
                }
                _ => {
                    self.dcx.emit_and_push(
                        errors::UnknownAttribute {
                            source: self.current_file().clone(),
                            range: attr.range(),
                        }
                        .into(),
                    );
                }
            }
        }
    }
}
