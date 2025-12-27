use error_snippet::Result;
use lume_ast::*;
use lume_lexer::{TokenKind, TokenType};

use crate::errors::*;
use crate::{ItemKind, Parser};

impl Parser<'_> {
    #[libftrace::traced(level = Debug, err)]
    fn parse_item_prefix(&mut self) -> Result<()> {
        self.read_doc_comment();
        self.attributes = Some(self.parse_attributes()?);

        Ok(())
    }

    #[libftrace::traced(level = Debug, err)]
    pub(super) fn parse_item(&mut self) -> Result<Item> {
        self.parse_item_prefix()?;

        let visibility = self.parse_visibility()?;

        if let Some(visibility) = visibility.as_ref() {
            self.raise_if_visibility_invalid(visibility)?;
        }

        if let Some(item_kind) = self.item_kind() {
            self.raise_if_attributes(item_kind)?;
        }

        match self.token().kind {
            TokenKind::Import => self.parse_import(),
            TokenKind::Namespace => self.parse_namespace(),
            TokenKind::Impl => self.parse_implementation(),
            TokenKind::Fn => self.parse_function(visibility),
            TokenKind::Struct => self.parse_struct_definition(visibility),
            TokenKind::Trait => self.parse_trait_definition(visibility),
            TokenKind::Enum => self.parse_enum_definition(visibility),
            TokenKind::Use => self.parse_trait_implementation(),
            k => Err(InvalidTopLevelStatement {
                source: self.source.clone(),
                range: self.token().index,
                actual: k.as_type(),
            }
            .into()),
        }
    }

    #[libftrace::traced(level = Debug, err)]
    fn raise_if_attributes(&self, item: ItemKind) -> Result<()> {
        if item.supports_attributes() {
            return Ok(());
        }

        let attributes = if let Some(attributes) = self.attributes.as_ref()
            && !attributes.is_empty()
        {
            attributes
        } else {
            return Ok(());
        };

        let start = attributes.first().unwrap().location.start();
        let end = attributes.last().unwrap().location.end();

        Err(UnexpectedAttributes {
            source: self.source.clone(),
            range: start..end,
            item,
        }
        .into())
    }

    #[libftrace::traced(level = Debug, err)]
    fn raise_if_visibility_invalid(&self, visibility: &Visibility) -> Result<()> {
        let item = match self.token().kind {
            TokenKind::Import => ItemKind::Import,
            TokenKind::Namespace => ItemKind::Namespace,
            TokenKind::Impl => ItemKind::Impl,
            TokenKind::Use => ItemKind::TraitImpl,
            _ => return Ok(()),
        };

        Err(UnexpectedVisibility {
            source: self.source.clone(),
            range: visibility.location().0.clone(),
            item,
        }
        .into())
    }

    #[libftrace::traced(level = Debug, err)]
    fn parse_import(&mut self) -> Result<Item> {
        let start = self.consume(TokenType::Import)?.start();
        let path = self.parse_import_path()?;

        let names = match self.token().kind {
            TokenKind::LeftParen => self.consume_paren_seq(Parser::parse_identifier)?,
            kind => {
                return Err(InvalidImportPath {
                    source: self.source.clone(),
                    range: self.token().index,
                    found: kind.as_type(),
                }
                .into());
            }
        };

        let end = self.token_at(self.index - 1).end();

        let import_def = Import {
            path,
            names,
            location: (start..end).into(),
        };

        Ok(Item::Import(Box::new(import_def)))
    }

    #[libftrace::traced(level = Debug, err)]
    fn parse_namespace(&mut self) -> Result<Item> {
        let (path, location) = self.consume_with_loc(|p| {
            p.consume(TokenType::Namespace)?;

            p.parse_import_path()
        })?;

        Ok(Item::Namespace(Box::new(Namespace { path, location })))
    }

    #[libftrace::traced(level = Trace, err)]
    fn parse_signature(&mut self) -> Result<Signature> {
        let start = self.expect_fn()?.start();

        let external = self.check_external();
        let name = self.parse_callable_name_or_err(
            ExpectedFunctionName {
                source: self.source.clone(),
                range: self.token().index,
            }
            .into(),
        )?;

        let type_parameters = self.parse_type_parameters()?;
        let parameters = self.parse_parameters()?;
        let return_type = self.parse_return_type()?;

        let end = self.previous_token().end();

        let location = Location(start..end);

        Ok(Signature {
            external,
            name,
            type_parameters,
            parameters,
            return_type,
            location,
        })
    }

    #[libftrace::traced(level = Trace, err)]
    fn parse_visibility(&mut self) -> Result<Option<Visibility>> {
        match self.token().kind {
            TokenKind::Pub => {
                let token = self.consume(TokenType::Pub)?;

                if self.check(TokenType::LeftParen) {
                    self.consume(TokenType::Internal)?;

                    let start = token.start();
                    let end = self.consume(TokenType::RightParen)?.end();

                    return Ok(Some(Visibility::Internal {
                        location: (start..end).into(),
                    }));
                }

                Ok(Some(Visibility::Public {
                    location: token.index.into(),
                }))
            }
            TokenKind::Priv => Ok(Some(Visibility::Private {
                location: self.consume(TokenType::Priv)?.index.into(),
            })),
            _ => Ok(None),
        }
    }

    #[libftrace::traced(level = Trace, err)]
    fn parse_parameters(&mut self) -> Result<Vec<Parameter>> {
        // If no opening parenthesis, no parameters are defined.
        if !self.peek(TokenType::LeftParen) {
            return Ok(Vec::new());
        }

        self.consume_paren_seq(Parser::parse_parameter)
    }

    #[libftrace::traced(level = Trace, err)]
    fn parse_parameter(&mut self) -> Result<Parameter> {
        if let Some(token) = self.consume_if(TokenType::SelfRef) {
            let location = token.index;

            return Ok(Parameter {
                name: Identifier {
                    name: "self".into(),
                    location: location.clone().into(),
                },
                vararg: false,
                param_type: Type::SelfType(SelfType {
                    location: location.clone().into(),
                }),
                location: location.into(),
            });
        }

        let vararg = self.check(TokenType::DotDotDot);
        let name = self.parse_identifier()?;

        self.consume(TokenType::Colon)?;

        let param_type = self.parse_type()?;

        let start = name.location().start();
        let end = param_type.location().end();

        Ok(Parameter {
            name,
            param_type,
            vararg,
            location: (start..end).into(),
        })
    }

    /// Parses the return type of the current function definition.
    #[libftrace::traced(level = Trace, err)]
    fn parse_return_type(&mut self) -> Result<Option<Box<Type>>> {
        if self.consume_if(TokenType::Arrow).is_none() {
            return Ok(None);
        }

        Ok(Some(Box::new(self.parse_type()?)))
    }

    #[libftrace::traced(level = Debug, err)]
    fn parse_function(&mut self, visibility: Option<Visibility>) -> Result<Item> {
        let documentation = self.doc_token.take();
        let attributes = self.attributes.take().unwrap_or_default();

        let signature = self.parse_signature()?;
        let start = visibility
            .as_ref()
            .map_or(signature.location.start(), |vis| vis.location().start());

        let block = self.parse_opt_external_block(signature.external)?;

        let end = self.previous_token().end();
        let location = Location(start..end);

        let function_def = FunctionDefinition {
            attributes,
            visibility,
            signature,
            block,
            location,
            documentation,
        };

        Ok(Item::FunctionDefinition(Box::new(function_def)))
    }

    #[libftrace::traced(level = Debug, err)]
    fn parse_struct_definition(&mut self, visibility: Option<Visibility>) -> Result<Item> {
        let documentation = self.doc_token.take();
        let attributes = self.attributes.take().unwrap_or_default();

        let start = attributes
            .first()
            .map_or(self.consume(TokenType::Struct)?.start(), |attr| attr.location.start());

        let name = self.parse_ident_or_err(
            ExpectedStructName {
                source: self.source.clone(),
                range: self.token().index,
            }
            .into(),
        )?;

        let type_parameters = self.parse_type_parameters()?;
        let fields = self.consume_curly_seq(Parser::parse_struct_field)?;

        let end = self.previous_token().end();

        let struct_def = StructDefinition {
            attributes,
            visibility,
            name,
            fields,
            type_parameters,
            location: (start..end).into(),
            documentation,
        };

        Ok(Item::StructDefinition(Box::new(struct_def)))
    }

    #[libftrace::traced(level = Trace, err)]
    fn parse_struct_field(&mut self) -> Result<Field> {
        self.parse_item_prefix()?;
        self.raise_if_attributes(ItemKind::Field)?;

        let documentation = self.doc_token.take();
        let visibility = self.parse_visibility()?;

        let Ok(name) = self.parse_identifier() else {
            return Err(ExpectedStructField {
                source: self.source.clone(),
                range: self.token().index,
            }
            .into());
        };

        // Report a special error if we found an identifier, such as
        // a method declaration, which isn't allowed within a `struct` block.
        if self.consume_if(TokenType::Colon).is_none() && self.peek(TokenType::Identifier) {
            return Err(MethodInStruct {
                source: self.source.clone(),
                range: self.token().index,
            }
            .into());
        }

        let field_type = self.parse_type()?;
        let default_value = self.parse_opt_assignment()?;

        let start = visibility
            .as_ref()
            .map_or(name.location.start(), |vis| vis.location().start());

        let end = self.expect_semi()?.end();

        Ok(Field {
            visibility,
            name,
            field_type,
            default_value,
            location: (start..end).into(),
            documentation,
        })
    }

    #[libftrace::traced(level = Trace, err)]
    fn parse_implementation(&mut self) -> Result<Item> {
        let start = self.expect_impl()?.start();

        let type_parameters = self.parse_type_parameters()?;
        let name = self.parse_type()?;
        let methods = self.consume_curly_seq(Parser::parse_method_definition)?;

        let end = self.previous_token().end();

        Ok(Item::Implementation(Box::new(Implementation {
            name: Box::new(name),
            methods,
            type_parameters,
            location: (start..end).into(),
        })))
    }

    #[libftrace::traced(level = Debug, err)]
    fn parse_method_definition(&mut self) -> Result<MethodDefinition> {
        self.parse_item_prefix()?;
        self.raise_if_attributes(ItemKind::Method)?;

        let documentation = self.doc_token.take();
        let attributes = self.attributes.take().unwrap_or_default();

        let visibility = self.parse_visibility()?;
        let signature = self.parse_signature()?;
        let start = visibility
            .as_ref()
            .map_or(signature.location.start(), |v| v.location().start());

        let block = self.parse_opt_external_block(signature.external)?;

        let end = self.previous_token().end();
        let location = Location(start..end);

        Ok(MethodDefinition {
            attributes,
            visibility,
            signature,
            block,
            location,
            documentation,
        })
    }

    #[libftrace::traced(level = Debug, err)]
    fn parse_trait_definition(&mut self, visibility: Option<Visibility>) -> Result<Item> {
        let documentation = self.doc_token.take();
        let attributes = self.attributes.take().unwrap_or_default();

        let start = visibility
            .as_ref()
            .map_or(self.consume(TokenType::Trait)?.start(), |vis| vis.location().start());

        let name = self.parse_ident_or_err(
            ExpectedTraitName {
                source: self.source.clone(),
                range: self.token().index,
            }
            .into(),
        )?;

        let type_parameters = self.parse_type_parameters()?;
        let methods = self.consume_curly_seq(Parser::parse_trait_method)?;

        let end = self.previous_token().end();

        let trait_def = TraitDefinition {
            attributes,
            visibility,
            name,
            methods,
            type_parameters,
            location: (start..end).into(),
            documentation,
        };

        Ok(Item::TraitDefinition(Box::new(trait_def)))
    }

    #[libftrace::traced(level = Trace, err)]
    fn parse_trait_method(&mut self) -> Result<TraitMethodDefinition> {
        self.parse_item_prefix()?;
        self.raise_if_attributes(ItemKind::TraitMethod)?;

        let documentation = self.doc_token.take();

        let signature = self.parse_signature()?;
        let start = signature.location.start();

        let block = if self.check(TokenType::Semicolon) {
            None
        } else {
            Some(self.parse_block()?)
        };

        let end = self.previous_token().end();
        let location = Location(start..end);

        Ok(TraitMethodDefinition {
            signature,
            block,
            location,
            documentation,
        })
    }

    #[libftrace::traced(level = Debug, err)]
    fn parse_trait_implementation(&mut self) -> Result<Item> {
        let start = self.consume(TokenType::Use)?.start();
        let type_parameters = self.parse_type_parameters()?;

        let name = self.parse_type()?;

        self.consume(TokenType::Colon)?;
        let target = self.parse_type()?;

        let methods = self.consume_curly_seq(Parser::parse_trait_method_implementation)?;
        let end = self.previous_token().end();
        let location = Location(start..end);

        Ok(Item::TraitImplementation(Box::new(TraitImplementation {
            type_parameters,
            name: Box::new(name),
            target: Box::new(target),
            methods,
            location,
        })))
    }

    #[libftrace::traced(level = Trace, err)]
    fn parse_trait_method_implementation(&mut self) -> Result<TraitMethodImplementation> {
        let signature = self.parse_signature()?;
        let start = signature.location.start();

        let block = self.parse_opt_external_block(signature.external)?;
        let end = self.previous_token().end();
        let location = Location(start..end);

        Ok(TraitMethodImplementation {
            signature,
            block,
            location,
        })
    }

    /// Parses a single enum type definition, such as:
    ///
    /// ```lm
    /// enum IpAddrKind {
    ///   V4,
    ///   V6,
    /// }
    /// ```
    #[libftrace::traced(level = Debug, err)]
    fn parse_enum_definition(&mut self, visibility: Option<Visibility>) -> Result<Item> {
        self.raise_if_attributes(ItemKind::Enum)?;

        let documentation = self.doc_token.take();

        let start = visibility
            .as_ref()
            .map_or(self.consume(TokenType::Enum)?.start(), |vis| vis.location().start());

        let name = self.parse_identifier()?;
        let type_parameters = self.parse_type_parameters()?;
        let cases = self.consume_comma_seq(TokenType::LeftCurly, TokenType::RightCurly, Parser::parse_enum_case)?;

        let end = self.previous_token().end();

        Ok(Item::EnumDefinition(Box::new(EnumDefinition {
            visibility,
            name,
            type_parameters,
            cases,
            location: (start..end).into(),
            documentation,
        })))
    }

    /// Parses a single enum type case, such as `V4` or `V4(String)`.
    #[libftrace::traced(level = Trace, err)]
    fn parse_enum_case(&mut self) -> Result<EnumDefinitionCase> {
        self.parse_item_prefix()?;
        self.raise_if_attributes(ItemKind::Variant)?;

        let documentation = self.doc_token.take();

        let name = self.parse_identifier()?;

        let parameters = if self.peek(TokenType::LeftParen) {
            self.consume_comma_seq(TokenType::LeftParen, TokenType::RightParen, |p| {
                Ok(Box::new(p.parse_type()?))
            })?
        } else {
            Vec::new()
        };

        let start = name.location.start();
        let end = self.token_at(self.index - 1).end();

        Ok(EnumDefinitionCase {
            name,
            parameters,
            location: (start..end).into(),
            documentation,
        })
    }
}
