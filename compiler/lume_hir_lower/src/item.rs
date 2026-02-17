use lume_hir::{SELF_PARAM_NAME, SELF_TYPE_NAME};

use crate::*;

fn visibility(expr: Option<&lume_ast::Visibility>) -> lume_hir::Visibility {
    match expr {
        Some(lume_ast::Visibility::Public { .. }) => lume_hir::Visibility::Public,
        Some(lume_ast::Visibility::Internal { .. }) => lume_hir::Visibility::Internal,
        None | Some(lume_ast::Visibility::Private { .. }) => lume_hir::Visibility::Private,
    }
}

impl LoweringContext<'_> {
    #[libftrace::traced(level = Debug)]
    pub(crate) fn item(&mut self, expr: lume_ast::Item) -> Result<()> {
        let hir_ast = match expr {
            lume_ast::Item::Import(i) => {
                self.import(*i)?;

                return Ok(());
            }
            lume_ast::Item::Namespace(namespace) => {
                self.namespace = Some(self.expand_import_path(namespace.path)?);
                return Ok(());
            }
            lume_ast::Item::StructDefinition(t) => self.struct_definition(*t)?,
            lume_ast::Item::TraitDefinition(t) => self.trait_definition(*t)?,
            lume_ast::Item::EnumDefinition(t) => self.enum_definition(*t)?,
            lume_ast::Item::FunctionDefinition(f) => self.function_definition(*f)?,
            lume_ast::Item::TraitImplementation(f) => self.trait_implementation(*f)?,
            lume_ast::Item::Implementation(f) => self.implementation(*f)?,
        };

        let id = hir_ast.id();

        // Ensure that the ID doesn't overwrite an existing entry.
        debug_assert!(!self.map.nodes.contains_key(&id));

        self.map.nodes.insert(id, hir_ast);

        Ok(())
    }

    #[libftrace::traced(level = Debug)]
    pub(crate) fn import(&mut self, expr: lume_ast::Import) -> Result<()> {
        for imported_name in expr.names {
            let namespace = self.expand_import_path(expr.path.clone())?;

            let import_path_name = if imported_name.is_lower() {
                lume_hir::PathSegment::callable(self.identifier(imported_name))
            } else {
                lume_hir::PathSegment::ty(self.identifier(imported_name))
            };

            let imported_path = lume_hir::Path::with_root(namespace, import_path_name);

            self.imports.insert(imported_path.name.to_string(), imported_path);
        }

        Ok(())
    }

    #[libftrace::traced(level = Debug)]
    fn signature(&mut self, sig: lume_ast::Signature) -> Result<lume_hir::FnSignature> {
        let name = self.expand_callable_name(sig.name)?;
        let type_parameters = self.type_parameters(sig.type_parameters)?;
        let parameters = self.parameters(sig.parameters, false)?;
        let return_type = self.hir_type_opt(sig.return_type.map(|f| *f))?;
        let location = self.location(sig.location);

        Ok(lume_hir::FnSignature {
            name,
            parameters,
            type_parameters,
            return_type,
            location,
        })
    }

    #[libftrace::traced(level = Debug)]
    fn function_definition(&mut self, expr: lume_ast::FunctionDefinition) -> Result<lume_hir::Node> {
        let id = self.next_node_id();
        self.handle_attributes(&expr.attributes)?;

        let name = self.expand_callable_name(expr.signature.name.clone())?;
        self.ensure_item_undefined(id, DefinedItem::Function(name.clone()))?;

        lume_hir::with_frame!(self.current_type_params, || {
            let visibility = visibility(expr.visibility.as_ref());
            let signature = self.signature(expr.signature)?;
            let location = self.location(expr.location);

            let block = expr
                .block
                .map(|block| self.isolated_block(block, &signature.parameters));

            Ok(lume_hir::Node::Function(lume_hir::FunctionDefinition {
                id,
                doc_comment: expr.documentation,
                visibility,
                signature,
                block,
                location,
            }))
        })
    }

    #[libftrace::traced(level = Debug)]
    fn parameters(&mut self, params: Vec<lume_ast::Parameter>, allow_self: bool) -> Result<Vec<lume_hir::Parameter>> {
        let param_len = params.len();
        let mut parameters = Vec::with_capacity(param_len);

        for (index, param) in params.into_iter().enumerate() {
            // Make sure that `self` is the first parameter.
            //
            // While it doesn't change must in the view of the compiler,
            // using `self` as the first parameter is a best practice, since it's
            // so much easier to see whether a method is an instance method or a static
            // method.
            if index > 0 && param.is_self() {
                return Err(crate::errors::SelfNotFirstParameter {
                    source: self.current_file().clone(),
                    range: param.location.0.clone(),
                    ty: String::from(SELF_PARAM_NAME),
                }
                .into());
            }

            // Using `self` outside of an object context is not allowed, such as functions.
            if !allow_self && param.param_type.is_self() {
                return Err(crate::errors::InvalidSelfParameter {
                    source: self.current_file().clone(),
                    range: param.location.0.clone(),
                    ty: String::from(SELF_TYPE_NAME),
                }
                .into());
            }

            // Naming a parameter `self` with an explicit type is not allowed.
            if param.name.as_str() == SELF_PARAM_NAME && !param.param_type.is_self() {
                return Err(crate::errors::SelfWithExplicitType {
                    source: self.current_file().clone(),
                    range: param.location.0.clone(),
                    ty: String::from(SELF_PARAM_NAME),
                }
                .into());
            }

            // Make sure that any vararg parameters exist only on the last position.
            if param.vararg && index + 1 < param_len {
                return Err(crate::errors::VarargNotLastParameter {
                    source: self.current_file().clone(),
                    range: param.location.0.clone(),
                }
                .into());
            }

            let id = self.next_node_id();
            let name = self.identifier(param.name);
            let param_type = self.hir_type(param.param_type)?;
            let location = self.location(param.location);

            parameters.push(lume_hir::Parameter {
                id,
                index,
                name,
                param_type,
                vararg: param.vararg,
                location,
            });
        }

        self.ensure_unique_series(&parameters, |duplicate, existing| {
            crate::errors::DuplicateParameter {
                duplicate_range: duplicate.location,
                original_range: existing.location,
                name: existing.name.to_string(),
            }
            .into()
        })?;

        Ok(parameters)
    }

    #[libftrace::traced(level = Debug)]
    fn struct_definition(&mut self, expr: lume_ast::StructDefinition) -> Result<lume_hir::Node> {
        let id = self.next_node_id();
        self.handle_attributes(&expr.attributes)?;

        lume_hir::with_frame!(self.current_type_params, || {
            let name = self.expand_generic_name(expr.name, expr.type_parameters.clone())?;

            let visibility = visibility(expr.visibility.as_ref());
            let type_parameters = self.type_parameters(expr.type_parameters)?;
            let location = self.location(expr.location);

            self.ensure_item_undefined(id, DefinedItem::Type(name.clone()))?;
            let mut fields = Vec::with_capacity(expr.fields.len());

            self.with_self_as(name.clone(), |ctx| {
                for (idx, field) in expr.fields.into_iter().enumerate() {
                    fields.push(ctx.struct_field_definition(idx, field)?);
                }

                ctx.ensure_unique_series(&fields, |duplicate, existing| {
                    crate::errors::DuplicateField {
                        duplicate_range: duplicate.name.location,
                        original_range: existing.location,
                        name: existing.name.to_string(),
                    }
                    .into()
                })
            })?;

            Ok(lume_hir::Node::Type(lume_hir::TypeDefinition::Struct(Box::new(
                lume_hir::StructDefinition {
                    id,
                    doc_comment: expr.documentation,
                    name,
                    visibility,
                    type_parameters,
                    fields,
                    location,
                },
            ))))
        })
    }

    #[libftrace::traced(level = Debug)]
    fn struct_field_definition(&mut self, index: usize, expr: lume_ast::Field) -> Result<lume_hir::Field> {
        let id = self.next_node_id();

        let visibility = visibility(expr.visibility.as_ref());
        let name = self.identifier(expr.name);
        let field_type = self.hir_type(expr.field_type)?;
        let location = self.location(expr.location);

        let default_value = if let Some(def) = expr.default_value {
            Some(self.expression(def)?)
        } else {
            None
        };

        let field = lume_hir::Field {
            id,
            index,
            doc_comment: expr.documentation,
            name,
            visibility,
            field_type,
            default_value,
            location,
        };

        self.map.nodes.insert(id, lume_hir::Node::Field(field.clone()));

        Ok(field)
    }

    #[libftrace::traced(level = Debug)]
    fn implementation(&mut self, expr: lume_ast::Implementation) -> Result<lume_hir::Node> {
        let id = self.next_node_id();

        lume_hir::with_frame!(self.current_type_params, || {
            let impl_name = expr.name.to_string();
            let type_name = self.expand_type_name(impl_name)?;

            let type_parameters = self.type_parameters(expr.type_parameters)?;
            let target = self.hir_type(*expr.name)?;
            let location = self.location(expr.location);

            let mut methods = Vec::with_capacity(expr.methods.len());
            self.with_self_as(target.name.clone(), |ctx| {
                for method in expr.methods {
                    let method = ctx.implementation_method(type_name.clone(), method)?;
                    ctx.map.nodes.insert(method.id, lume_hir::Node::Method(method.clone()));

                    methods.push(method);
                }

                ctx.ensure_unique_series(&methods, |duplicate, existing| {
                    crate::errors::DuplicateMethod {
                        duplicate_range: duplicate.signature.name.location,
                        original_range: existing.location,
                        name: existing.signature.name.to_string(),
                    }
                    .into()
                })
            })?;

            Ok(lume_hir::Node::Impl(lume_hir::Implementation {
                id,
                target: Box::new(target),
                methods,
                type_parameters,
                location,
            }))
        })
    }

    #[libftrace::traced(level = Debug)]
    fn implementation_method(
        &mut self,
        type_name: lume_hir::Path,
        expr: lume_ast::MethodDefinition,
    ) -> Result<lume_hir::MethodDefinition> {
        let id = self.next_node_id();
        self.handle_attributes(&expr.attributes)?;

        let method_name = self.path_segment(lume_ast::PathSegment::callable(expr.signature.name.clone()))?;
        self.ensure_item_undefined(id, DefinedItem::Method(type_name.clone(), method_name.clone()))?;

        lume_hir::with_frame!(self.current_type_params, || {
            let visibility = visibility(expr.visibility.as_ref());
            let name = lume_hir::Path::with_root(type_name, method_name);
            let type_parameters = self.type_parameters(expr.signature.type_parameters)?;
            let parameters = self.parameters(expr.signature.parameters, true)?;
            let return_type = self.hir_type_opt(expr.signature.return_type.map(|f| *f))?;
            let location = self.location(expr.location);

            let block = expr.block.map(|block| self.isolated_block(block, &parameters));

            Ok(lume_hir::MethodDefinition {
                id,
                doc_comment: expr.documentation,
                signature: lume_hir::FnSignature {
                    name,
                    parameters,
                    type_parameters,
                    return_type,
                    location,
                },
                visibility,
                block,
                location,
            })
        })
    }

    #[libftrace::traced(level = Debug)]
    fn trait_definition(&mut self, expr: lume_ast::TraitDefinition) -> Result<lume_hir::Node> {
        let id = self.next_node_id();
        self.handle_attributes(&expr.attributes)?;

        let name = self.expand_generic_name(expr.name.clone(), expr.type_parameters.clone())?;
        self.ensure_item_undefined(id, DefinedItem::Type(name.clone()))?;

        lume_hir::with_frame!(self.current_type_params, || {
            let visibility = visibility(expr.visibility.as_ref());
            let type_parameters = self.type_parameters(expr.type_parameters)?;
            let location = self.location(expr.location);

            let mut methods = Vec::with_capacity(expr.methods.len());
            self.with_self_as(name.clone(), |ctx| {
                for method in expr.methods {
                    let method = ctx.trait_definition_method(name.clone(), method)?;
                    ctx.map
                        .nodes
                        .insert(method.id, lume_hir::Node::TraitMethodDef(method.clone()));

                    methods.push(method);
                }

                ctx.ensure_unique_series(&methods, |duplicate, existing| {
                    crate::errors::DuplicateMethod {
                        duplicate_range: duplicate.signature.name.location,
                        original_range: existing.location,
                        name: existing.signature.name.to_string(),
                    }
                    .into()
                })
            })?;

            Ok(lume_hir::Node::Type(lume_hir::TypeDefinition::Trait(Box::new(
                lume_hir::TraitDefinition {
                    id,
                    doc_comment: expr.documentation,
                    name,
                    visibility,
                    type_parameters,
                    methods,
                    location,
                },
            ))))
        })
    }

    #[libftrace::traced(level = Debug)]
    fn trait_definition_method(
        &mut self,
        type_name: lume_hir::Path,
        expr: lume_ast::TraitMethodDefinition,
    ) -> Result<lume_hir::TraitMethodDefinition> {
        let id = self.next_node_id();

        lume_hir::with_frame!(self.current_type_params, || {
            let ident = self.identifier(expr.signature.name);

            let name = lume_hir::Path::with_root(type_name, lume_hir::PathSegment::callable(ident));
            let type_parameters = self.type_parameters(expr.signature.type_parameters)?;
            let parameters = self.parameters(expr.signature.parameters, true)?;
            let return_type = self.hir_type_opt(expr.signature.return_type.map(|f| *f))?;
            let location = self.location(expr.location);
            let block = expr.block.map(|b| self.isolated_block(b, &parameters));

            Ok(lume_hir::TraitMethodDefinition {
                id,
                doc_comment: expr.documentation,
                signature: lume_hir::FnSignature {
                    name,
                    parameters,
                    type_parameters,
                    return_type,
                    location,
                },
                block,
                location,
            })
        })
    }

    #[libftrace::traced(level = Debug)]
    fn enum_definition(&mut self, expr: lume_ast::EnumDefinition) -> Result<lume_hir::Node> {
        let id = self.next_node_id();

        lume_hir::with_frame!(self.current_type_params, || {
            let name = self.expand_generic_name(expr.name, expr.type_parameters.clone())?;
            self.ensure_item_undefined(id, DefinedItem::Type(name.clone()))?;

            let type_parameters = self.type_parameters(expr.type_parameters)?;
            let visibility = visibility(expr.visibility.as_ref());
            let location = self.location(expr.location);

            let mut cases = Vec::with_capacity(expr.cases.len());
            for (idx, case) in expr.cases.into_iter().enumerate() {
                cases.push(self.enum_definition_case(idx, case)?);
            }

            self.ensure_unique_series(&cases, |duplicate, existing| {
                crate::errors::DuplicateVariant {
                    duplicate_range: duplicate.name.location,
                    original_range: existing.location,
                    name: existing.name.to_string(),
                }
                .into()
            })?;

            Ok(lume_hir::Node::Type(lume_hir::TypeDefinition::Enum(Box::new(
                lume_hir::EnumDefinition {
                    id,
                    doc_comment: expr.documentation,
                    name,
                    type_parameters,
                    visibility,
                    cases,
                    location,
                },
            ))))
        })
    }

    #[libftrace::traced(level = Debug)]
    fn enum_definition_case(
        &mut self,
        idx: usize,
        expr: lume_ast::EnumDefinitionCase,
    ) -> Result<lume_hir::EnumDefinitionCase> {
        let name = self.expand_type_name(expr.name)?;
        let location = self.location(expr.location);

        let mut parameters = Vec::with_capacity(expr.parameters.len());
        for param in expr.parameters {
            parameters.push(self.hir_type(*param)?);
        }

        let symbol = lume_hir::EnumDefinitionCase {
            idx,
            doc_comment: expr.documentation,
            name,
            parameters,
            location,
        };

        Ok(symbol)
    }

    #[libftrace::traced(level = Debug)]
    fn trait_implementation(&mut self, expr: lume_ast::TraitImplementation) -> Result<lume_hir::Node> {
        let id = self.next_node_id();

        lume_hir::with_frame!(self.current_type_params, || {
            let type_parameters = self.type_parameters(expr.type_parameters)?;
            let name = self.hir_type(*expr.name)?;
            let target = self.hir_type(*expr.target)?;
            let location = self.location(expr.location);

            let mut methods = Vec::with_capacity(expr.methods.len());
            self.with_self_as(target.name.clone(), |ctx| {
                for method in expr.methods {
                    let method = ctx.trait_implementation_method(method)?;
                    ctx.map
                        .nodes
                        .insert(method.id, lume_hir::Node::TraitMethodImpl(method.clone()));

                    methods.push(method);
                }

                ctx.ensure_unique_series(&methods, |duplicate, existing| {
                    crate::errors::DuplicateMethod {
                        duplicate_range: duplicate.name.location,
                        original_range: existing.location,
                        name: existing.name.to_string(),
                    }
                    .into()
                })
            })?;

            Ok(lume_hir::Node::TraitImpl(lume_hir::TraitImplementation {
                id,
                name: Box::new(name),
                target: Box::new(target),
                methods,
                type_parameters,
                location,
            }))
        })
    }

    #[libftrace::traced(level = Debug)]
    fn trait_implementation_method(
        &mut self,
        expr: lume_ast::TraitMethodImplementation,
    ) -> Result<lume_hir::TraitMethodImplementation> {
        let id = self.next_node_id();

        lume_hir::with_frame!(self.current_type_params, || {
            let name = self.identifier(expr.signature.name);
            let parameters = self.parameters(expr.signature.parameters, true)?;
            let type_parameters = self.type_parameters(expr.signature.type_parameters)?;
            let return_type = self.hir_type_opt(expr.signature.return_type.map(|f| *f))?;
            let location = self.location(expr.location);

            let block = expr.block.map(|block| self.isolated_block(block, &parameters));

            Ok(lume_hir::TraitMethodImplementation {
                id,
                name,
                parameters,
                type_parameters,
                return_type,
                block,
                location,
            })
        })
    }
}
