use crate::*;

impl LoweringContext<'_> {
    /// Handles the given attributes for the current node.
    ///
    /// Since this methods uses the current node, ensure that this method is
    /// called before any other node IDs are assigned.
    pub(crate) fn handle_attributes(&mut self, attrs: &[lume_ast::Attribute]) -> Result<()> {
        let current_node = self.current_node;

        for attr in attrs {
            match attr.name.as_str() {
                "lang_item" => {
                    let Some(name_arg) = attr.arguments.iter().find(|arg| arg.key.as_str() == "name") else {
                        return Err(errors::LangItemMissingName {
                            source: self.current_file().clone(),
                            range: attr.location.0.clone(),
                        }
                        .into());
                    };

                    let lume_ast::Literal::String(lang_name) = &name_arg.value else {
                        return Err(errors::LangItemInvalidNameType {
                            source: self.current_file().clone(),
                            range: attr.location.0.clone(),
                        }
                        .into());
                    };

                    self.map.lang_items.add_name(lang_name.value, current_node)?;
                }
                _ => {
                    return Err(errors::UnknownAttribute {
                        source: self.current_file().clone(),
                        range: attr.location.0.clone(),
                    }
                    .into());
                }
            }
        }

        Ok(())
    }
}
