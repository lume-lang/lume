use lume_hir::{SELF_PARAM_NAME, SELF_TYPE_NAME};

use crate::*;

/// Collects the given iterator of doc comments into a single string.
pub(crate) fn documentation<I>(comments: I) -> Option<String>
where
    I: IntoIterator<Item = lume_ast::DocComment>,
{
    let str = comments
        .into_iter()
        .map(|doc| doc.as_text().trim_start_matches("/// ").to_string())
        .collect::<Vec<_>>()
        .join("\n");

    if str.is_empty() { None } else { Some(str) }
}

fn visibility(expr: Option<lume_ast::Visibility>) -> lume_hir::Visibility {
    let Some(visibility) = expr else {
        return lume_hir::Visibility::Private;
    };

    if visibility.pub_kw().is_some() && visibility.internal_kw().is_some() {
        lume_hir::Visibility::Internal
    } else if visibility.pub_kw().is_some() {
        lume_hir::Visibility::Public
    } else {
        lume_hir::Visibility::Private
    }
}

impl LoweringContext<'_> {
    #[tracing::instrument(level = "DEBUG", skip_all)]
    pub(crate) fn item(&mut self, expr: lume_ast::Item) -> Result<()> {
        let hir_ast = match expr {
            lume_ast::Item::Import(i) => {
                self.import(i)?;

                return Ok(());
            }
            lume_ast::Item::Namespace(namespace) => {
                if let Some(import_path) = namespace.import_path() {
                    self.namespace = Some(self.expand_import_path(import_path));
                }

                return Ok(());
            }
            lume_ast::Item::Struct(t) => self.struct_definition(t),
            lume_ast::Item::Trait(t) => self.trait_definition(t),
            lume_ast::Item::Enum(t) => self.enum_definition(t),
            lume_ast::Item::Fn(f) => self.function_definition(f),
            lume_ast::Item::TraitImpl(f) => self.trait_implementation(f),
            lume_ast::Item::Impl(f) => self.implementation(f),
        };

        let id = hir_ast.id();

        // Ensure that the ID doesn't overwrite an existing entry.
        debug_assert!(!self.map.nodes.contains_key(&id));

        self.map.nodes.insert(id, hir_ast);

        Ok(())
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    pub(crate) fn import(&mut self, expr: lume_ast::Import) -> Result<()> {
        let Some(list) = expr.import_list() else { return Ok(()) };
        let Some(path) = expr.import_path() else { return Ok(()) };

        let namespace = self.expand_import_path(path);

        for imported_name in list.items() {
            let import_path_name = if imported_name.is_lower() {
                lume_hir::PathSegment::callable(self.ident(imported_name))
            } else {
                lume_hir::PathSegment::ty(self.ident(imported_name))
            };

            let imported_path = lume_hir::Path::with_root(namespace.clone(), import_path_name);

            self.imports.insert(imported_path.name.to_string(), imported_path);
        }

        Ok(())
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn signature(&mut self, sig: lume_ast::Sig, allow_self: bool) -> lume_hir::FnSignature {
        // Must be defined first, so any type arguments in the method name can be
        // resolved correctly.
        let type_parameters = match sig.bound_types() {
            Some(bound_types) => self.type_parameters(bound_types.types()),
            None => Vec::new(),
        };

        let name = match self.self_type.as_ref() {
            Some(self_path) => {
                let method_name = lume_hir::PathSegment::callable(self.ident_opt(sig.name()));
                lume_hir::Path::with_root(self_path.to_owned(), method_name)
            }
            None => self.expand_callable_name(sig.name()),
        };

        let parameters = self.parameters(sig.param_list(), allow_self);
        let return_type = self.type_or_void(sig.return_type().and_then(|ret| ret.ty()));
        let location = self.location(sig.location());

        lume_hir::FnSignature {
            name,
            parameters,
            type_parameters,
            return_type,
            location,
        }
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn signature_opt(&mut self, sig: Option<lume_ast::Sig>, allow_self: bool) -> lume_hir::FnSignature {
        if let Some(sig) = sig {
            return self.signature(sig, allow_self);
        }

        lume_hir::FnSignature {
            name: lume_hir::Path::missing(),
            parameters: Vec::new(),
            type_parameters: Vec::new(),
            return_type: lume_hir::Type::void(),
            location: Location::empty(),
        }
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn function_definition(&mut self, expr: lume_ast::Fn) -> lume_hir::Node {
        let id = self.next_node_id();
        self.handle_attributes(expr.attr());

        lume_hir::with_frame!(self.current_type_params, || {
            let visibility = visibility(expr.visibility());
            let signature = self.signature_opt(expr.sig(), false);
            let doc_comment = documentation(expr.doc_comments());
            let location = self.location(expr.location());

            self.ensure_item_undefined(id, DefinedItem::Function(signature.name.clone()));

            let block = expr
                .block()
                .map(|block| self.isolated_block(block, &signature.parameters));

            lume_hir::Node::Function(lume_hir::FunctionDefinition {
                id,
                doc_comment,
                visibility,
                signature,
                block,
                location,
            })
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn parameters(
        &mut self,
        parameter_list: Option<lume_ast::ParamList>,
        allow_self: bool,
    ) -> Vec<lume_hir::Parameter> {
        let Some(parameter_list) = parameter_list else {
            return Vec::new();
        };

        let parameter_list: Vec<_> = parameter_list.param().collect();
        let param_len = parameter_list.len();
        let mut parameters = Vec::with_capacity(param_len);

        for (index, param) in parameter_list.into_iter().enumerate() {
            // Make sure that `self` is the first parameter.
            //
            // While it doesn't change must in the view of the compiler,
            // using `self` as the first parameter is a best practice, since it's
            // so much easier to see whether a method is an instance method or a static
            // method.
            if index > 0 && param.is_self() {
                self.dcx.emit_and_push(
                    crate::errors::SelfNotFirstParameter {
                        source: self.current_file().clone(),
                        range: param.location().0.clone(),
                        ty: String::from(SELF_PARAM_NAME),
                    }
                    .into(),
                );
            }

            // Using `self` outside of an object context is not allowed, such as functions.
            if !allow_self && param.ty().is_some_and(|ty| ty.is_self()) {
                self.dcx.emit_and_push(
                    crate::errors::InvalidSelfParameter {
                        source: self.current_file().clone(),
                        range: param.location().0.clone(),
                        ty: String::from(SELF_TYPE_NAME),
                    }
                    .into(),
                );
            }

            let is_vararg = param.vararg().is_some();

            // Naming a parameter `self` with an explicit type is not allowed.
            if param.ty().is_some()
                && !param.is_self_type()
                && param.name().is_some_and(|name| name.syntax().text() == SELF_PARAM_NAME)
            {
                self.dcx.emit_and_push(
                    crate::errors::SelfWithExplicitType {
                        source: self.current_file().clone(),
                        range: param.location().0.clone(),
                        ty: String::from(SELF_PARAM_NAME),
                    }
                    .into(),
                );
            }

            // Make sure that any vararg parameters exist only on the last position.
            if is_vararg && index + 1 < param_len {
                self.dcx.emit_and_push(
                    crate::errors::VarargNotLastParameter {
                        source: self.current_file().clone(),
                        range: param.location().0.clone(),
                    }
                    .into(),
                );
            }

            let id = self.next_node_id();
            let name = self.ident_opt(param.name());
            let location = self.location(param.location());

            let param_type = if name.as_str() == SELF_PARAM_NAME {
                self.alloc_self_type(self.location(param.location()))
            } else {
                self.type_or_void(param.ty())
            };

            let parameter = lume_hir::Parameter {
                id,
                index,
                name,
                param_type,
                vararg: is_vararg,
                location,
            };

            self.map.nodes.insert(id, lume_hir::Node::Parameter(parameter.clone()));

            parameters.push(parameter);
        }

        self.ensure_unique_series(&parameters, |duplicate, existing| {
            crate::errors::DuplicateParameter {
                duplicate_range: duplicate.location,
                original_range: existing.location,
                name: existing.name.to_string(),
            }
            .into()
        });

        parameters
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn struct_definition(&mut self, expr: lume_ast::Struct) -> lume_hir::Node {
        let id = self.next_node_id();
        self.handle_attributes(expr.attr());

        lume_hir::with_frame!(self.current_type_params, || {
            let type_parameters = match expr.bound_types() {
                Some(bound_types) => self.type_parameters(bound_types.types()),
                None => Vec::new(),
            };

            let name = self.expand_generic_name(expr.name(), expr.bound_types().map(|list| list.types()));

            let visibility = visibility(expr.visibility());
            let doc_comment = documentation(expr.doc_comments());
            let location = self.location(expr.location());

            self.ensure_item_undefined(id, DefinedItem::Type(name.clone()));
            let mut fields = Vec::new();

            self.with_self_as(name.clone(), |ctx| {
                for (idx, field) in expr.fields().enumerate() {
                    fields.push(ctx.struct_field_definition(idx, field));
                }

                ctx.ensure_unique_series(&fields, |duplicate, existing| {
                    crate::errors::DuplicateField {
                        duplicate_range: duplicate.location(),
                        original_range: existing.location(),
                        name: existing.name.to_string(),
                    }
                    .into()
                });
            });

            lume_hir::Node::Type(lume_hir::TypeDefinition::Struct(Box::new(lume_hir::StructDefinition {
                id,
                doc_comment,
                name,
                visibility,
                type_parameters,
                fields,
                location,
            })))
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn struct_field_definition(&mut self, index: usize, expr: lume_ast::Field) -> lume_hir::Field {
        let id = self.next_node_id();

        let doc_comment = documentation(expr.doc_comments());
        let visibility = visibility(expr.visibility());
        let name = self.ident_opt(expr.name());
        let field_type = self.type_or_void(expr.field_type());
        let default_value = expr.default_value().map(|def| self.expression(def));
        let location = self.location(expr.location());

        let field = lume_hir::Field {
            id,
            index,
            doc_comment,
            name,
            visibility,
            field_type,
            default_value,
            location,
        };

        self.map.nodes.insert(id, lume_hir::Node::Field(field.clone()));

        field
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn implementation(&mut self, expr: lume_ast::Impl) -> lume_hir::Node {
        let id = self.next_node_id();

        lume_hir::with_frame!(self.current_type_params, || {
            let type_parameters = match expr.bound_types() {
                Some(bound_types) => self.type_parameters(bound_types.types()),
                None => Vec::new(),
            };

            let target = self.type_or_void(expr.target());
            let location = self.location(expr.location());

            let mut methods = Vec::new();
            self.with_self_as(target.name.clone(), |ctx| {
                for method in expr.methods() {
                    let method = ctx.implementation_method(method);
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
                });
            });

            lume_hir::Node::Impl(lume_hir::Implementation {
                id,
                target: Box::new(target),
                methods,
                type_parameters,
                location,
            })
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn implementation_method(&mut self, expr: lume_ast::Method) -> lume_hir::MethodDefinition {
        let id = self.next_node_id();
        self.handle_attributes(expr.attr());

        lume_hir::with_frame!(self.current_type_params, || {
            let visibility = visibility(expr.visibility());
            let signature = self.signature_opt(expr.sig(), true);
            let doc_comment = documentation(expr.doc_comments());
            let location = self.location(expr.location());

            self.ensure_item_undefined(id, DefinedItem::Method(signature.name.clone()));

            let block = expr
                .block()
                .map(|block| self.isolated_block(block, &signature.parameters));

            lume_hir::MethodDefinition {
                id,
                doc_comment,
                signature,
                visibility,
                block,
                location,
            }
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn trait_definition(&mut self, expr: lume_ast::Trait) -> lume_hir::Node {
        let id = self.next_node_id();
        self.handle_attributes(expr.attr());

        lume_hir::with_frame!(self.current_type_params, || {
            // Must be defined first, so any type arguments in the trait name can be
            // resolved correctly.
            let type_parameters = match expr.bound_types() {
                Some(bound_types) => self.type_parameters(bound_types.types()),
                None => Vec::new(),
            };

            let name = self.expand_generic_name(expr.name(), expr.bound_types().map(|list| list.types()));
            self.ensure_item_undefined(id, DefinedItem::Type(name.clone()));

            let visibility = visibility(expr.visibility());
            let doc_comment = documentation(expr.doc_comments());
            let location = self.location(expr.location());

            let mut methods = Vec::new();
            self.with_self_as(name.clone(), |ctx| {
                for method in expr.methods() {
                    let method = ctx.trait_definition_method(method);
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
                });
            });

            lume_hir::Node::Type(lume_hir::TypeDefinition::Trait(Box::new(lume_hir::TraitDefinition {
                id,
                doc_comment,
                name,
                visibility,
                type_parameters,
                methods,
                location,
            })))
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn trait_definition_method(&mut self, expr: lume_ast::Method) -> lume_hir::TraitMethodDefinition {
        let id = self.next_node_id();

        lume_hir::with_frame!(self.current_type_params, || {
            let signature = self.signature_opt(expr.sig(), true);
            let location = self.location(expr.location());
            let doc_comment = documentation(expr.doc_comments());
            let block = expr.block().map(|b| self.isolated_block(b, &signature.parameters));

            lume_hir::TraitMethodDefinition {
                id,
                doc_comment,
                signature,
                block,
                location,
            }
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn enum_definition(&mut self, expr: lume_ast::Enum) -> lume_hir::Node {
        let id = self.next_node_id();

        lume_hir::with_frame!(self.current_type_params, || {
            let type_parameters = match expr.bound_types() {
                Some(bound_types) => self.type_parameters(bound_types.types()),
                None => Vec::new(),
            };

            let visibility = visibility(expr.visibility());
            let doc_comment = documentation(expr.doc_comments());
            let location = self.location(expr.location());

            let name = self.expand_generic_name(expr.name(), expr.bound_types().map(|list| list.types()));
            self.ensure_item_undefined(id, DefinedItem::Type(name.clone()));

            let mut cases = Vec::new();
            for (idx, case) in expr.cases().enumerate() {
                cases.push(self.enum_definition_case(idx, case));
            }

            self.ensure_unique_series(&cases, |duplicate, existing| {
                crate::errors::DuplicateVariant {
                    duplicate_range: duplicate.name.location,
                    original_range: existing.location,
                    name: existing.name.to_string(),
                }
                .into()
            });

            lume_hir::Node::Type(lume_hir::TypeDefinition::Enum(Box::new(lume_hir::EnumDefinition {
                id,
                doc_comment,
                name,
                type_parameters,
                visibility,
                cases,
                location,
            })))
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn enum_definition_case(&mut self, idx: usize, expr: lume_ast::Case) -> lume_hir::EnumDefinitionCase {
        let name = self.expand_type_name(expr.name());
        let doc_comment = documentation(expr.doc_comments());
        let location = self.location(expr.location());

        let mut parameters = Vec::new();

        if let Some(param_list) = expr.case_param_list() {
            for param in param_list.ty() {
                parameters.push(self.hir_type(param));
            }
        }

        lume_hir::EnumDefinitionCase {
            idx,
            doc_comment,
            name,
            parameters,
            location,
        }
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn trait_implementation(&mut self, expr: lume_ast::TraitImpl) -> lume_hir::Node {
        let id = self.next_node_id();

        lume_hir::with_frame!(self.current_type_params, || {
            let type_parameters = match expr.bound_types() {
                Some(bound_types) => self.type_parameters(bound_types.types()),
                None => Vec::new(),
            };

            let name = self.type_or_void(expr.trait_type());
            let target = self.type_or_void(expr.target_type());
            let location = self.location(expr.location());

            let mut methods = Vec::new();
            self.with_self_as(target.name.clone(), |ctx| {
                for method in expr.methods() {
                    let method = ctx.trait_implementation_method(method);
                    ctx.map
                        .nodes
                        .insert(method.id, lume_hir::Node::TraitMethodImpl(method.clone()));

                    methods.push(method);
                }

                ctx.ensure_unique_series(&methods, |duplicate, existing| {
                    crate::errors::DuplicateMethod {
                        duplicate_range: duplicate.signature.name.location,
                        original_range: existing.location,
                        name: existing.signature.name.to_string(),
                    }
                    .into()
                });
            });

            lume_hir::Node::TraitImpl(lume_hir::TraitImplementation {
                id,
                name: Box::new(name),
                target: Box::new(target),
                methods,
                type_parameters,
                location,
            })
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn trait_implementation_method(&mut self, expr: lume_ast::Method) -> lume_hir::TraitMethodImplementation {
        let id = self.next_node_id();

        lume_hir::with_frame!(self.current_type_params, || {
            let signature = self.signature_opt(expr.sig(), true);
            let doc_comment = documentation(expr.doc_comments());
            let location = self.location(expr.location());

            let block = expr
                .block()
                .map(|block| self.isolated_block(block, &signature.parameters));

            lume_hir::TraitMethodImplementation {
                id,
                doc_comment,
                signature,
                block,
                location,
            }
        })
    }
}
