use error_snippet::Result;
use lume_ast::*;
use lume_lexer::TokenKind;

use crate::{Parser, err, errors::*};

impl Parser {
    #[tracing::instrument(level = "DEBUG", skip(self), err)]
    pub(super) fn parse_top_level_expression(&mut self) -> Result<TopLevelExpression> {
        match self.token().kind {
            TokenKind::Import => self.parse_import(),
            TokenKind::Namespace => self.parse_namespace(),
            TokenKind::Impl => self.parse_impl(),
            TokenKind::Fn => self.parse_fn(),
            TokenKind::Pub => self.parse_pub_top_level_expression(),
            TokenKind::Struct => self.parse_struct(),
            TokenKind::Trait => self.parse_trait(),
            TokenKind::Enum => self.parse_enum(),
            TokenKind::Use => self.parse_use(),
            k => Err(err!(self, InvalidTopLevelStatement, actual, k)),
        }
    }

    #[tracing::instrument(level = "DEBUG", skip(self), err)]
    fn parse_pub_top_level_expression(&mut self) -> Result<TopLevelExpression> {
        let visibility = self.parse_visibility()?;

        match self.token().kind {
            TokenKind::Fn => self.parse_fn_visibility(visibility),
            TokenKind::Impl => self.parse_impl_visibility(visibility),
            TokenKind::Struct => self.parse_struct_visibility(visibility),
            TokenKind::Trait => self.parse_trait_visibility(visibility),
            TokenKind::Enum => self.parse_enum_visibility(visibility),
            TokenKind::Use => self.parse_use_visibility(visibility),
            k => Err(err!(self, InvalidTopLevelStatement, actual, k)),
        }
    }

    #[tracing::instrument(level = "DEBUG", skip(self), err)]
    fn parse_import(&mut self) -> Result<TopLevelExpression> {
        let start = self.consume(TokenKind::Import)?.start();
        let path = self.parse_import_path()?;

        if self.eof() {
            return Err(err!(self, InvalidImportPath, found, TokenKind::Eof));
        }

        let names = match self.token().kind {
            TokenKind::LeftParen => self.consume_paren_seq(Parser::parse_identifier)?,
            kind => return Err(err!(self, InvalidImportPath, found, kind)),
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
            p.consume(TokenKind::Namespace)?;

            p.parse_import_path()
        })?;

        Ok(TopLevelExpression::Namespace(Box::new(Namespace { path, location })))
    }

    #[tracing::instrument(level = "DEBUG", skip(self), err)]
    fn parse_impl(&mut self) -> Result<TopLevelExpression> {
        let visibility = self.parse_visibility()?;

        self.parse_impl_visibility(visibility)
    }

    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_impl_visibility(&mut self, visibility: Visibility) -> Result<TopLevelExpression> {
        let start = visibility.location().start();

        self.expect_impl()?;

        let type_parameters = self.parse_type_parameters()?;
        let name = self.parse_type()?;
        let methods = self.consume_curly_seq(Parser::parse_method)?;

        let end = self.previous_token().end();

        Ok(TopLevelExpression::Impl(Box::new(Implementation {
            visibility,
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
    fn parse_fn_visibility(&mut self, visibility: Visibility) -> Result<TopLevelExpression> {
        let start = visibility.location().start();

        self.expect_fn()?;

        let external = self.check_external();
        let name = self.parse_callable_name_or_err(err!(self, ExpectedFunctionName))?;
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
        if !self.peek(TokenKind::LeftParen) {
            return Ok(Vec::new());
        }

        self.consume_paren_seq(Parser::parse_fn_param)
    }

    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_fn_param(&mut self) -> Result<Parameter> {
        if let Some(token) = self.consume_if(TokenKind::SelfRef) {
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

        let vararg = self.check(TokenKind::DotDotDot);
        let name = self.parse_identifier()?;

        self.consume(TokenKind::Colon)?;

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
        if self.consume_if(TokenKind::Arrow).is_none() {
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
    fn parse_struct_visibility(&mut self, visibility: Visibility) -> Result<TopLevelExpression> {
        let start = self.consume(TokenKind::Struct)?.start();

        let builtin = self.consume_if(TokenKind::Builtin).is_some();

        let name = self.parse_ident_or_err(err!(self, ExpectedStructName))?;
        let type_parameters = self.parse_type_parameters()?;
        let properties = self.consume_curly_seq(Parser::parse_struct_property)?;

        let end = self.previous_token().end();

        let struct_def = StructDefinition {
            visibility,
            name,
            builtin,
            properties,
            type_parameters,
            location: (start..end).into(),
            documentation: self.doc_token.take(),
        };

        Ok(TopLevelExpression::TypeDefinition(Box::new(TypeDefinition::Struct(
            Box::new(struct_def),
        ))))
    }

    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_visibility(&mut self) -> Result<Visibility> {
        // If the member is marked as public, make it so.
        if let TokenKind::Pub = self.token().kind {
            let location = self.consume(TokenKind::Pub)?.index;

            Ok(Visibility::Public(Box::new(Public {
                location: location.into(),
            })))
        // By default, make it private.
        } else {
            let location = self.token().index;

            Ok(Visibility::Private(Box::new(Private {
                location: location.into(),
            })))
        }
    }

    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_struct_property(&mut self) -> Result<Property> {
        self.read_doc_comment();

        let visibility = self.parse_visibility()?;

        let Ok(name) = self.parse_identifier() else {
            return Err(err!(self, ExpectedStructProperty));
        };

        // Report a special error if we found an identifier, such as
        // a method declaration, which isn't allowed within a `struct` block.
        if self.consume_if(TokenKind::Colon).is_none() && self.peek(TokenKind::Identifier) {
            return Err(err!(self, MethodInStruct));
        }

        let property_type = self.parse_type()?;
        let default_value = self.parse_opt_assignment()?;

        let start = visibility.location().start();
        let end = self.expect_semi()?.end();

        Ok(Property {
            visibility,
            name,
            property_type,
            default_value,
            location: (start..end).into(),
            documentation: self.doc_token.take(),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip(self), err)]
    fn parse_method(&mut self) -> Result<MethodDefinition> {
        self.read_doc_comment();

        let visibility = self.parse_visibility()?;
        let start = visibility.location().start();

        // Report a special error if we found an identifier, such as
        // a property declaration, which isn't allowed within an `impl` block.
        if self.consume_if(TokenKind::Fn).is_none() && self.peek(TokenKind::Identifier) {
            return Err(err!(self, PropertyInImpl));
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
            t if t.kind == TokenKind::Identifier => self.parse_callable_name_or_err(err!(self, ExpectedFunctionName)),

            // As well as operator tokens, so we can do operator overloading.
            t if t.kind.is_operator() => Ok(self.consume_any().into()),

            _ => Err(err!(self, ExpectedFunctionName)),
        }
    }

    #[tracing::instrument(level = "DEBUG", skip(self), err)]
    fn parse_trait(&mut self) -> Result<TopLevelExpression> {
        let visibility = self.parse_visibility()?;

        self.parse_trait_visibility(visibility)
    }

    #[tracing::instrument(level = "DEBUG", skip(self), err)]
    fn parse_trait_visibility(&mut self, visibility: Visibility) -> Result<TopLevelExpression> {
        let start = self.consume(TokenKind::Trait)?.start();

        let name = self.parse_ident_or_err(err!(self, ExpectedTraitName))?;
        let type_parameters = self.parse_type_parameters()?;
        let methods = self.consume_curly_seq(Parser::parse_trait_method)?;

        let end = self.previous_token().end();

        let trait_def = TraitDefinition {
            visibility,
            name,
            methods,
            type_parameters,
            location: (start..end).into(),
            documentation: self.doc_token.take(),
        };

        Ok(TopLevelExpression::TypeDefinition(Box::new(TypeDefinition::Trait(
            Box::new(trait_def),
        ))))
    }

    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_trait_method(&mut self) -> Result<TraitMethodDefinition> {
        self.read_doc_comment();

        let visibility = self.parse_visibility()?;
        let start = self.expect_fn()?.start();

        let Ok(name) = self.parse_callable_name() else {
            return Err(err!(self, ExpectedFunctionName));
        };

        let type_parameters = self.parse_type_parameters()?;
        let parameters = self.parse_fn_params()?;
        let return_type = self.parse_fn_return_type()?;
        let block = if self.check(TokenKind::Semicolon) {
            None
        } else {
            Some(self.parse_block()?)
        };

        let end = self.previous_token().end();

        Ok(TraitMethodDefinition {
            visibility,
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
    fn parse_enum_visibility(&mut self, visibility: Visibility) -> Result<TopLevelExpression> {
        let start = self.consume(TokenKind::Enum)?.start();

        let name = self.parse_identifier()?;
        let cases = self.consume_comma_seq(TokenKind::LeftCurly, TokenKind::RightCurly, Parser::parse_enum_case)?;

        let end = self.previous_token().end();

        Ok(TopLevelExpression::TypeDefinition(Box::new(TypeDefinition::Enum(
            Box::new(EnumDefinition {
                visibility,
                name,
                cases,
                location: (start..end).into(),
                documentation: self.doc_token.take(),
            }),
        ))))
    }

    /// Parses a single enum type case, such as `V4` or `V4(String)`.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_enum_case(&mut self) -> Result<EnumDefinitionCase> {
        self.read_doc_comment();

        let name = self.parse_identifier()?;

        let parameters = if self.peek(TokenKind::LeftParen) {
            self.consume_comma_seq(TokenKind::LeftParen, TokenKind::RightParen, |p| {
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
    fn parse_use(&mut self) -> Result<TopLevelExpression> {
        let visibility = self.parse_visibility()?;

        self.parse_use_visibility(visibility)
    }

    #[tracing::instrument(level = "DEBUG", skip(self), err)]
    fn parse_use_visibility(&mut self, visibility: Visibility) -> Result<TopLevelExpression> {
        let start = self.consume(TokenKind::Use)?.start();
        let type_parameters = self.parse_type_parameters()?;

        let name = self.parse_type()?;

        self.consume(TokenKind::In)?;
        let target = self.parse_type()?;

        let methods = self.consume_curly_seq(Parser::parse_use_impl)?;
        let end = self.previous_token().end();

        let use_trait = UseTrait {
            visibility,
            type_parameters,
            name: Box::new(name),
            target: Box::new(target),
            methods,
            location: (start..end).into(),
        };

        Ok(TopLevelExpression::Use(Box::new(use_trait)))
    }

    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn parse_use_impl(&mut self) -> Result<TraitMethodImplementation> {
        let visibility = self.parse_visibility()?;
        self.expect_fn()?;

        let start = visibility.location().start();

        let Ok(name) = self.parse_callable_name() else {
            return Err(err!(self, ExpectedFunctionName));
        };

        let type_parameters = self.parse_type_parameters()?;
        let parameters = self.parse_fn_params()?;
        let return_type = self.parse_fn_return_type()?;
        let block = self.parse_block()?;

        let end = block.location.end();

        Ok(TraitMethodImplementation {
            visibility,
            name,
            parameters,
            type_parameters,
            return_type,
            block,
            location: (start..end).into(),
        })
    }
}
