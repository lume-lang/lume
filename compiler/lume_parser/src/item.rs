use error_snippet::Result;
use lume_ast::*;
use lume_lexer::{TokenKind, TokenType};

use crate::Parser;
use crate::errors::*;

impl Parser<'_> {
    #[tracing::instrument(level = "DEBUG", skip(self), err)]
    pub(super) fn parse_top_level_expression(&mut self) -> Result<TopLevelExpression> {
        match self.token().kind {
            TokenKind::Pub | TokenKind::Priv => self.parse_top_level_expression_visibility(),
            TokenKind::Import => self.parse_import(),
            TokenKind::Namespace => self.parse_namespace(),
            TokenKind::Impl => self.parse_implementation(),
            TokenKind::Fn => self.parse_fn(),
            TokenKind::Struct => self.parse_struct(),
            TokenKind::Trait => self.parse_trait(),
            TokenKind::Enum => self.parse_enum(),
            TokenKind::Use => self.parse_trait_implementation(),
            k => Err(InvalidTopLevelStatement {
                source: self.source.clone(),
                range: self.token().index,
                actual: k.as_type(),
            }
            .into()),
        }
    }

    #[tracing::instrument(level = "DEBUG", skip(self), err)]
    fn parse_top_level_expression_visibility(&mut self) -> Result<TopLevelExpression> {
        let visibility = self.parse_visibility()?;

        match self.token().kind {
            TokenKind::Fn => self.parse_fn_visibility(visibility),
            TokenKind::Struct => self.parse_struct_visibility(visibility),
            TokenKind::Trait => self.parse_trait_visibility(visibility),
            TokenKind::Enum => self.parse_enum_visibility(visibility),
            k => Err(InvalidTopLevelStatement {
                source: self.source.clone(),
                range: self.token().index,
                actual: k.as_type(),
            }
            .into()),
        }
    }

    #[tracing::instrument(level = "DEBUG", skip(self), err)]
    fn parse_import(&mut self) -> Result<TopLevelExpression> {
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

        Ok(TopLevelExpression::Import(Box::new(import_def)))
    }

    #[tracing::instrument(level = "DEBUG", skip(self), err)]
    fn parse_namespace(&mut self) -> Result<TopLevelExpression> {
        let (path, location) = self.consume_with_loc(|p| {
            p.consume(TokenType::Namespace)?;

            p.parse_import_path()
        })?;

        Ok(TopLevelExpression::Namespace(Box::new(Namespace { path, location })))
    }

    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_implementation(&mut self) -> Result<TopLevelExpression> {
        let start = self.expect_impl()?.start();

        let type_parameters = self.parse_type_parameters()?;
        let name = self.parse_type()?;
        let methods = self.consume_curly_seq(Parser::parse_method)?;

        let end = self.previous_token().end();

        Ok(TopLevelExpression::Impl(Box::new(Implementation {
            name: Box::new(name),
            methods,
            type_parameters,
            location: (start..end).into(),
        })))
    }

    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_fn(&mut self) -> Result<TopLevelExpression> {
        let visibility = self.parse_visibility()?;

        self.parse_fn_visibility(visibility)
    }

    #[tracing::instrument(level = "DEBUG", skip(self), err)]
    fn parse_fn_visibility(&mut self, visibility: Option<Visibility>) -> Result<TopLevelExpression> {
        let start = visibility
            .as_ref()
            .map_or(self.expect_fn()?.start(), |vis| vis.location().start());

        let external = self.check_external();
        let name = self.parse_callable_name_or_err(
            ExpectedFunctionName {
                source: self.source.clone(),
                range: self.token().index,
            }
            .into(),
        )?;

        let type_parameters = self.parse_type_parameters()?;
        let parameters = self.parse_fn_params()?;
        let return_type = self.parse_fn_return_type()?;
        let block = self.parse_opt_external_block(external)?;

        let end = self.previous_token().end();

        let function_def = FunctionDefinition {
            visibility,
            external,
            name,
            parameters,
            type_parameters,
            return_type,
            block,
            location: (start..end).into(),
            documentation: self.doc_token.take(),
        };

        Ok(TopLevelExpression::FunctionDefinition(Box::new(function_def)))
    }

    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_fn_params(&mut self) -> Result<Vec<Parameter>> {
        // If no opening parenthesis, no parameters are defined.
        if !self.peek(TokenType::LeftParen) {
            return Ok(Vec::new());
        }

        self.consume_paren_seq(Parser::parse_fn_param)
    }

    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_fn_param(&mut self) -> Result<Parameter> {
        if let Some(token) = self.consume_if(TokenType::SelfRef) {
            let location = token.index;

            return Ok(Parameter {
                name: Identifier {
                    name: "self".into(),
                    location: location.clone().into(),
                },
                vararg: false,
                param_type: Type::SelfType(Box::new(SelfType {
                    location: location.clone().into(),
                })),
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
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_fn_return_type(&mut self) -> Result<Option<Box<Type>>> {
        if self.consume_if(TokenType::Arrow).is_none() {
            return Ok(None);
        }

        Ok(Some(Box::new(self.parse_type()?)))
    }

    #[tracing::instrument(level = "DEBUG", skip(self), err)]
    fn parse_struct(&mut self) -> Result<TopLevelExpression> {
        let visibility = self.parse_visibility()?;

        self.parse_struct_visibility(visibility)
    }

    #[tracing::instrument(level = "DEBUG", skip(self), err)]
    fn parse_struct_visibility(&mut self, visibility: Option<Visibility>) -> Result<TopLevelExpression> {
        let documentation = self.doc_token.take();
        let attributes = self.attributes.take().unwrap_or_default();

        let start = attributes
            .first()
            .map(|attr| attr.location.start())
            .unwrap_or(self.consume(TokenType::Struct)?.start());

        let builtin = self.check(TokenType::Builtin);

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
            builtin,
            fields,
            type_parameters,
            location: (start..end).into(),
            documentation,
        };

        Ok(TopLevelExpression::TypeDefinition(Box::new(TypeDefinition::Struct(
            Box::new(struct_def),
        ))))
    }

    #[tracing::instrument(level = "TRACE", skip(self), err)]
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

    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_struct_field(&mut self) -> Result<Field> {
        self.read_doc_comment()?;

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
            documentation: self.doc_token.take(),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip(self), err)]
    fn parse_method(&mut self) -> Result<MethodDefinition> {
        self.read_doc_comment()?;

        let visibility = self.parse_visibility()?;
        let start = visibility
            .as_ref()
            .map_or(self.token().start(), |vis| vis.location().start());

        // Report a special error if we found an identifier, such as
        // a field declaration, which isn't allowed within an `impl` block.
        if !self.check(TokenType::Fn) && self.peek(TokenType::Identifier) {
            return Err(FieldInImpl {
                source: self.source.clone(),
                range: self.token().index,
            }
            .into());
        }

        let external = self.check_external();

        let name = self.parse_method_name()?;
        let type_parameters = self.parse_type_parameters()?;
        let parameters = self.parse_fn_params()?;
        let return_type = self.parse_fn_return_type()?;
        let block = self.parse_opt_external_block(external)?;

        let end = self.token_at(self.index - 1).end();

        Ok(MethodDefinition {
            visibility,
            external,
            name,
            parameters,
            type_parameters,
            return_type,
            block,
            location: (start..end).into(),
            documentation: self.doc_token.take(),
        })
    }

    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_method_name(&mut self) -> Result<Identifier> {
        match self.token() {
            // Allow actual identifiers.
            t if t.kind.as_type() == TokenType::Identifier => self.parse_callable_name_or_err(
                ExpectedFunctionName {
                    source: self.source.clone(),
                    range: self.token().index,
                }
                .into(),
            ),

            // As well as operator tokens, so we can do operator overloading.
            t if t.kind.is_operator() => {
                let index = self.consume_any().index;
                let slice = &self.source.content[index.clone()];

                Ok(Identifier {
                    name: slice.to_string(),
                    location: index.into(),
                })
            }

            t => Err(ExpectedFunctionName {
                source: self.source.clone(),
                range: t.index,
            }
            .into()),
        }
    }

    #[tracing::instrument(level = "DEBUG", skip(self), err)]
    fn parse_trait(&mut self) -> Result<TopLevelExpression> {
        let visibility = self.parse_visibility()?;

        self.parse_trait_visibility(visibility)
    }

    #[tracing::instrument(level = "DEBUG", skip(self), err)]
    fn parse_trait_visibility(&mut self, visibility: Option<Visibility>) -> Result<TopLevelExpression> {
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

        Ok(TopLevelExpression::TypeDefinition(Box::new(TypeDefinition::Trait(
            Box::new(trait_def),
        ))))
    }

    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_trait_method(&mut self) -> Result<TraitMethodDefinition> {
        self.read_doc_comment()?;

        let start = self.expect_fn()?.start();

        let Ok(name) = self.parse_callable_name() else {
            return Err(ExpectedFunctionName {
                source: self.source.clone(),
                range: self.token().index,
            }
            .into());
        };

        let type_parameters = self.parse_type_parameters()?;
        let parameters = self.parse_fn_params()?;
        let return_type = self.parse_fn_return_type()?;
        let block = if self.check(TokenType::Semicolon) {
            None
        } else {
            Some(self.parse_block()?)
        };

        let end = self.previous_token().end();

        Ok(TraitMethodDefinition {
            name,
            parameters,
            type_parameters,
            return_type,
            block,
            location: (start..end).into(),
            documentation: self.doc_token.take(),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip(self), err)]
    fn parse_enum(&mut self) -> Result<TopLevelExpression> {
        let visibility = self.parse_visibility()?;

        self.parse_enum_visibility(visibility)
    }

    /// Parses a single enum type definition, such as:
    ///
    /// ```lm
    /// enum IpAddrKind {
    ///   V4,
    ///   V6,
    /// }
    /// ```
    #[tracing::instrument(level = "DEBUG", skip(self), err)]
    fn parse_enum_visibility(&mut self, visibility: Option<Visibility>) -> Result<TopLevelExpression> {
        let documentation = self.doc_token.take();

        let start = visibility
            .as_ref()
            .map_or(self.consume(TokenType::Enum)?.start(), |vis| vis.location().start());

        let name = self.parse_identifier()?;
        let type_parameters = self.parse_type_parameters()?;
        let cases = self.consume_comma_seq(TokenType::LeftCurly, TokenType::RightCurly, Parser::parse_enum_case)?;

        let end = self.previous_token().end();

        Ok(TopLevelExpression::TypeDefinition(Box::new(TypeDefinition::Enum(
            Box::new(EnumDefinition {
                visibility,
                name,
                type_parameters,
                cases,
                location: (start..end).into(),
                documentation,
            }),
        ))))
    }

    /// Parses a single enum type case, such as `V4` or `V4(String)`.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_enum_case(&mut self) -> Result<EnumDefinitionCase> {
        self.read_doc_comment()?;

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
            documentation: self.doc_token.take(),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip(self), err)]
    fn parse_trait_implementation(&mut self) -> Result<TopLevelExpression> {
        let start = self.consume(TokenType::Use)?.start();
        let type_parameters = self.parse_type_parameters()?;

        let name = self.parse_type()?;

        self.consume(TokenType::In)?;
        let target = self.parse_type()?;

        let methods = self.consume_curly_seq(Parser::parse_trait_method_implementation)?;
        let end = self.previous_token().end();

        Ok(TopLevelExpression::TraitImpl(Box::new(TraitImplementation {
            type_parameters,
            name: Box::new(name),
            target: Box::new(target),
            methods,
            location: (start..end).into(),
        })))
    }

    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_trait_method_implementation(&mut self) -> Result<TraitMethodImplementation> {
        let visibility = self.parse_visibility()?;

        let start = visibility.map_or(self.expect_fn()?.start(), |v| v.location().start());
        let external = self.check_external();

        let Ok(name) = self.parse_callable_name() else {
            return Err(ExpectedFunctionName {
                source: self.source.clone(),
                range: self.token().index,
            }
            .into());
        };

        let type_parameters = self.parse_type_parameters()?;
        let parameters = self.parse_fn_params()?;
        let return_type = self.parse_fn_return_type()?;
        let block = self.parse_opt_external_block(external)?;

        let end = self.previous_token().end();

        Ok(TraitMethodImplementation {
            external,
            name,
            parameters,
            type_parameters,
            return_type,
            block,
            location: (start..end).into(),
        })
    }
}
