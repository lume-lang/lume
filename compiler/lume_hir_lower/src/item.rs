use std::collections::HashSet;

use error_snippet::Result;
use lume_hir::{Node, SELF_TYPE_NAME};

use crate::errors::*;
use crate::{DefinedItem, LowerModule};

impl LowerModule {
    #[libftrace::traced(level = Debug)]
    pub(super) fn lower_items(&mut self, expressions: Vec<lume_ast::TopLevelExpression>) -> Result<()> {
        for expr in &expressions {
            if let Err(err) = self.define_imported_items(expr) {
                self.dcx.emit_and_push(err);
            }
        }

        self.dcx.ensure_untainted()?;

        for expr in expressions {
            if let Err(err) = self.top_level_expression(expr) {
                self.dcx.emit_and_push(err);
            }
        }

        self.dcx.ensure_untainted()
    }

    #[libftrace::traced(level = Debug)]
    fn define_imported_items(&mut self, expr: &lume_ast::TopLevelExpression) -> Result<()> {
        match expr {
            lume_ast::TopLevelExpression::Namespace(namespace) => {
                self.namespace = Some(self.import_path(namespace.path.clone())?);

                Ok(())
            }
            lume_ast::TopLevelExpression::TypeDefinition(type_def) => match &**type_def {
                lume_ast::TypeDefinition::Struct(struct_def) => {
                    let id = self.next_node_id();

                    let path_segment = lume_ast::PathSegment::ty(struct_def.name.clone());
                    let name = self.expand_name(path_segment)?;

                    self.ensure_item_undefined(id, DefinedItem::Type(name))?;
                    self.add_lang_items(id, &struct_def.attributes)?;

                    Ok(())
                }
                lume_ast::TypeDefinition::Trait(trait_def) => {
                    let id = self.next_node_id();

                    let path_segment = lume_ast::PathSegment::ty(trait_def.name.clone());
                    let name = self.expand_name(path_segment)?;

                    self.ensure_item_undefined(id, DefinedItem::Type(name))?;
                    self.add_lang_items(id, &trait_def.attributes)?;

                    Ok(())
                }
                lume_ast::TypeDefinition::Enum(enum_def) => {
                    let id = self.next_node_id();

                    let path_segment = lume_ast::PathSegment::ty(enum_def.name.clone());
                    let name = self.expand_name(path_segment)?;

                    self.ensure_item_undefined(id, DefinedItem::Type(name))?;

                    Ok(())
                }
            },
            lume_ast::TopLevelExpression::FunctionDefinition(func_def) => {
                let id = self.next_node_id();

                let path_segment = lume_ast::PathSegment::callable(func_def.name.clone());
                let name = self.expand_name(path_segment)?;

                self.ensure_item_undefined(id, DefinedItem::Function(name))?;
                self.add_lang_items(id, &func_def.attributes)?;

                Ok(())
            }
            lume_ast::TopLevelExpression::Impl(implementation) => {
                let impl_name = implementation.name.to_string();
                let type_name = self.expand_name(lume_ast::PathSegment::ty(impl_name))?;

                for method in &implementation.methods {
                    let id = self.next_node_id();
                    let method_name = self.path_segment(lume_ast::PathSegment::callable(method.name.clone()))?;

                    self.ensure_item_undefined(id, DefinedItem::Method(type_name.clone(), method_name))?;
                    self.add_lang_items(id, &method.attributes)?;
                }

                Ok(())
            }
            lume_ast::TopLevelExpression::Import(_) | lume_ast::TopLevelExpression::TraitImpl(_) => Ok(()),
        }
    }

    #[libftrace::traced(level = Debug)]
    fn top_level_expression(&mut self, expr: lume_ast::TopLevelExpression) -> Result<()> {
        let hir_ast = match expr {
            lume_ast::TopLevelExpression::Import(i) => {
                self.import(*i)?;

                return Ok(());
            }
            lume_ast::TopLevelExpression::Namespace(namespace) => {
                self.namespace = Some(self.import_path(namespace.path)?);

                return Ok(());
            }
            lume_ast::TopLevelExpression::TypeDefinition(t) => match *t {
                lume_ast::TypeDefinition::Struct(t) => self.struct_definition(*t)?,
                lume_ast::TypeDefinition::Trait(t) => self.trait_definition(*t)?,
                lume_ast::TypeDefinition::Enum(t) => self.enum_definition(*t)?,
            },
            lume_ast::TopLevelExpression::FunctionDefinition(f) => self.function_definition(*f)?,
            lume_ast::TopLevelExpression::TraitImpl(f) => self.trait_implementation(*f)?,
            lume_ast::TopLevelExpression::Impl(f) => self.implementation(*f)?,
        };

        let id = hir_ast.id();

        // Ensure that the ID doesn't overwrite an existing entry.
        debug_assert!(!self.map.nodes.contains_key(&id));

        self.map.nodes.insert(id, hir_ast);

        Ok(())
    }

    #[libftrace::traced(level = Debug)]
    pub(super) fn import(&mut self, expr: lume_ast::Import) -> Result<()> {
        for imported_name in expr.names {
            let namespace = self.import_path(expr.path.clone())?;

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
    fn function_definition(&mut self, expr: lume_ast::FunctionDefinition) -> Result<lume_hir::Node> {
        let name = self.expand_name(lume_ast::PathSegment::callable(expr.name))?;
        let id = *self.defined.get(&DefinedItem::Function(name.clone())).unwrap();

        let visibility = lower_visibility(expr.visibility.as_ref());
        let type_parameters = self.type_parameters(expr.type_parameters)?;
        let parameters = self.parameters(expr.parameters, false)?;
        let return_type = self.opt_type_ref(expr.return_type.map(|f| *f))?;
        let location = self.location(expr.location);

        let block = expr.block.map(|block| self.isolated_block(block, &parameters));

        self.type_parameters.pop().unwrap();

        Ok(lume_hir::Node::Function(lume_hir::FunctionDefinition {
            id,
            doc_comment: expr.documentation,
            visibility,
            name,
            type_parameters,
            parameters,
            return_type,
            block,
            location,
        }))
    }

    #[libftrace::traced(level = Debug)]
    fn parameters(&mut self, params: Vec<lume_ast::Parameter>, allow_self: bool) -> Result<Vec<lume_hir::Parameter>> {
        let param_len = params.len();
        let mut parameters = Vec::with_capacity(param_len);
        let mut names: HashSet<lume_ast::Identifier> = HashSet::with_capacity(param_len);

        for (index, param) in params.into_iter().enumerate() {
            // Make sure that `self` is the first parameter.
            //
            // While it doesn't change must in the view of the compiler,
            // using `self` as the first parameter is a best practice, since it's
            // so much easier to see whether a method is an instance method or a static
            // method.
            if index > 0 && param.is_self() {
                return Err(SelfNotFirstParameter {
                    source: self.file.clone(),
                    range: param.location.0.clone(),
                    ty: String::from(SELF_TYPE_NAME),
                }
                .into());
            }

            // Using `self` outside of an object context is not allowed, such as functions.
            if !allow_self && param.param_type.is_self() {
                return Err(SelfOutsideObjectContext {
                    source: self.file.clone(),
                    range: param.location.0.clone(),
                    ty: String::from(SELF_TYPE_NAME),
                }
                .into());
            }

            // Naming a parameter `self` with an explicit type is not allowed.
            if param.name.as_str() == SELF_TYPE_NAME && !param.param_type.is_self() {
                return Err(SelfWithExplicitType {
                    source: self.file.clone(),
                    range: param.location.0.clone(),
                    ty: String::from(SELF_TYPE_NAME),
                }
                .into());
            }

            // Make sure that any vararg parameters exist only on the last position.
            if param.vararg && index + 1 < param_len {
                return Err(VarargNotLastParameter {
                    source: self.file.clone(),
                    range: param.location.0.clone(),
                }
                .into());
            }

            if let Some(existing) = names.get(&param.name) {
                return Err(crate::errors::DuplicateParameter {
                    source: self.file.clone(),
                    duplicate_range: param.location.0.clone(),
                    original_range: existing.location.0.clone(),
                    name: param.name.to_string(),
                }
                .into());
            }

            names.insert(param.name.clone());

            let name = self.identifier(param.name);
            let param_type = self.type_ref(param.param_type)?;
            let location = self.location(param.location);

            parameters.push(lume_hir::Parameter {
                index,
                name,
                param_type,
                vararg: param.vararg,
                location,
            });
        }

        Ok(parameters)
    }

    #[libftrace::traced(level = Debug)]
    fn struct_definition(&mut self, expr: lume_ast::StructDefinition) -> Result<lume_hir::Node> {
        let name = self.expand_name(lume_ast::PathSegment::ty(expr.name))?;
        let id = *self.defined.get(&DefinedItem::Type(name.clone())).unwrap();

        let visibility = lower_visibility(expr.visibility.as_ref());
        let type_parameters = self.type_parameters(expr.type_parameters)?;
        let location = self.location(expr.location);

        self.self_type = Some(name.clone());

        let mut fields = Vec::with_capacity(expr.fields.len());
        for (idx, field) in expr.fields.into_iter().enumerate() {
            fields.push(self.struct_field_definition(idx, field)?);
        }

        self.type_parameters.pop().unwrap();
        self.self_type = None;

        Ok(lume_hir::Node::Type(lume_hir::TypeDefinition::Struct(Box::new(
            lume_hir::StructDefinition {
                id,
                doc_comment: expr.documentation,
                name,
                visibility,
                builtin: expr.builtin,
                type_parameters,
                fields,
                location,
            },
        ))))
    }

    #[libftrace::traced(level = Debug)]
    fn struct_field_definition(&mut self, index: usize, expr: lume_ast::Field) -> Result<lume_hir::Field> {
        let id = self.next_node_id();

        let visibility = lower_visibility(expr.visibility.as_ref());
        let name = self.identifier(expr.name);
        let field_type = self.type_ref(expr.field_type)?;
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

        self.map.nodes.insert(id, Node::Field(field.clone()));

        Ok(field)
    }

    #[libftrace::traced(level = Debug)]
    fn implementation(&mut self, expr: lume_ast::Implementation) -> Result<lume_hir::Node> {
        let id = self.next_node_id();

        let impl_name = expr.name.to_string();
        let type_name = self.expand_name(lume_ast::PathSegment::ty(impl_name))?;

        let type_parameters = self.type_parameters(expr.type_parameters)?;
        let target = self.type_ref(*expr.name)?;
        let location = self.location(expr.location);

        self.self_type = Some(target.name.clone());

        let mut methods = Vec::with_capacity(expr.methods.len());
        let mut method_names: HashSet<lume_ast::Identifier> = HashSet::with_capacity(expr.methods.len());

        for method in expr.methods {
            if let Some(existing) = method_names.get(&method.name) {
                return Err(crate::errors::DuplicateMethod {
                    source: self.file.clone(),
                    duplicate_range: method.name.location.0.clone(),
                    original_range: existing.location.0.clone(),
                    name: method.name.to_string(),
                }
                .into());
            }

            method_names.insert(method.name.clone());

            let method = self.implementation_method(type_name.clone(), method)?;
            self.map.nodes.insert(method.id, Node::Method(method.clone()));

            methods.push(method);
        }

        self.type_parameters.pop().unwrap();
        self.self_type = None;

        Ok(lume_hir::Node::Impl(lume_hir::Implementation {
            id,
            target: Box::new(target),
            methods,
            type_parameters,
            location,
        }))
    }

    #[libftrace::traced(level = Debug)]
    fn implementation_method(
        &mut self,
        type_name: lume_hir::Path,
        expr: lume_ast::MethodDefinition,
    ) -> Result<lume_hir::MethodDefinition> {
        let method_name = self.path_segment(lume_ast::PathSegment::callable(expr.name.clone()))?;
        let defined_item = DefinedItem::Method(type_name, method_name);

        let id = *self.defined.get(&defined_item).unwrap();

        let visibility = lower_visibility(expr.visibility.as_ref());
        let name = self.identifier(expr.name);
        let type_parameters = self.type_parameters(expr.type_parameters)?;
        let parameters = self.parameters(expr.parameters, true)?;
        let return_type = self.opt_type_ref(expr.return_type.map(|f| *f))?;
        let location = self.location(expr.location);

        let block = expr.block.map(|block| self.isolated_block(block, &parameters));

        self.type_parameters.pop().unwrap();

        Ok(lume_hir::MethodDefinition {
            id,
            doc_comment: expr.documentation,
            name,
            visibility,
            type_parameters,
            parameters,
            return_type,
            block,
            location,
        })
    }

    #[libftrace::traced(level = Debug)]
    fn trait_definition(&mut self, expr: lume_ast::TraitDefinition) -> Result<lume_hir::Node> {
        let self_name = self.expand_self_name(expr.name.clone(), &expr.type_parameters)?;

        let name = self.expand_name(lume_ast::PathSegment::ty(expr.name))?;
        let id = *self.defined.get(&DefinedItem::Type(name.clone())).unwrap();

        let visibility = lower_visibility(expr.visibility.as_ref());
        let type_parameters = self.type_parameters(expr.type_parameters)?;
        let location = self.location(expr.location);

        self.self_type = Some(self_name);

        let mut methods = Vec::with_capacity(expr.methods.len());
        let mut method_names: HashSet<lume_ast::Identifier> = HashSet::with_capacity(expr.methods.len());

        for method in expr.methods {
            if let Some(existing) = method_names.get(&method.name) {
                return Err(crate::errors::DuplicateMethod {
                    source: self.file.clone(),
                    duplicate_range: method.name.location.0.clone(),
                    original_range: existing.location.0.clone(),
                    name: method.name.to_string(),
                }
                .into());
            }

            method_names.insert(method.name.clone());

            let method = self.trait_definition_method(method)?;
            self.map.nodes.insert(method.id, Node::TraitMethodDef(method.clone()));

            methods.push(method);
        }

        self.type_parameters.pop().unwrap();
        self.self_type = None;

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
    }

    #[libftrace::traced(level = Debug)]
    fn trait_definition_method(
        &mut self,
        expr: lume_ast::TraitMethodDefinition,
    ) -> Result<lume_hir::TraitMethodDefinition> {
        let id = self.next_node_id();

        let name = self.identifier(expr.name);
        let type_parameters = self.type_parameters(expr.type_parameters)?;
        let parameters = self.parameters(expr.parameters, true)?;
        let return_type = self.opt_type_ref(expr.return_type.map(|f| *f))?;
        let location = self.location(expr.location);
        let block = expr.block.map(|b| self.isolated_block(b, &parameters));

        self.type_parameters.pop().unwrap();

        Ok(lume_hir::TraitMethodDefinition {
            id,
            doc_comment: expr.documentation,
            name,
            type_parameters,
            parameters,
            return_type,
            block,
            location,
        })
    }

    #[libftrace::traced(level = Debug)]
    fn enum_definition(&mut self, expr: lume_ast::EnumDefinition) -> Result<lume_hir::Node> {
        let id = self.next_node_id();

        let name = self.expand_name(lume_ast::PathSegment::ty(expr.name))?;
        let type_parameters = self.type_parameters(expr.type_parameters)?;
        let visibility = lower_visibility(expr.visibility.as_ref());
        let location = self.location(expr.location);

        let mut cases = Vec::with_capacity(expr.cases.len());
        for (idx, case) in expr.cases.into_iter().enumerate() {
            cases.push(self.enum_definition_case(idx, case)?);
        }

        self.type_parameters.pop().unwrap();

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
    }

    #[libftrace::traced(level = Debug)]
    fn enum_definition_case(
        &mut self,
        idx: usize,
        expr: lume_ast::EnumDefinitionCase,
    ) -> Result<lume_hir::EnumDefinitionCase> {
        let name = self.expand_name(lume_ast::PathSegment::ty(expr.name))?;
        let location = self.location(expr.location);

        let mut parameters = Vec::with_capacity(expr.parameters.len());
        for param in expr.parameters {
            parameters.push(self.type_ref(*param)?);
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
        let type_parameters = self.type_parameters(expr.type_parameters)?;
        let name = self.type_ref(*expr.name)?;
        let target = self.type_ref(*expr.target)?;

        self.self_type = Some(target.name.clone());

        let mut methods = Vec::with_capacity(expr.methods.len());
        let mut method_names: HashSet<lume_ast::Identifier> = HashSet::with_capacity(expr.methods.len());

        for method in expr.methods {
            if let Some(existing) = method_names.get(&method.name) {
                return Err(crate::errors::DuplicateMethod {
                    source: self.file.clone(),
                    duplicate_range: method.name.location.0.clone(),
                    original_range: existing.location.0.clone(),
                    name: method.name.to_string(),
                }
                .into());
            }

            method_names.insert(method.name.clone());

            let method = self.trait_implementation_method(method)?;
            self.map.nodes.insert(method.id, Node::TraitMethodImpl(method.clone()));

            methods.push(method);
        }

        let location = self.location(expr.location);

        self.type_parameters.pop().unwrap();
        self.self_type = None;

        Ok(lume_hir::Node::TraitImpl(lume_hir::TraitImplementation {
            id,
            name: Box::new(name),
            target: Box::new(target),
            methods,
            type_parameters,
            location,
        }))
    }

    #[libftrace::traced(level = Debug)]
    fn trait_implementation_method(
        &mut self,
        expr: lume_ast::TraitMethodImplementation,
    ) -> Result<lume_hir::TraitMethodImplementation> {
        let id = self.next_node_id();

        let name = self.identifier(expr.name);
        let parameters = self.parameters(expr.parameters, true)?;
        let type_parameters = self.type_parameters(expr.type_parameters)?;
        let return_type = self.opt_type_ref(expr.return_type.map(|f| *f))?;
        let location = self.location(expr.location);

        let block = expr.block.map(|block| self.isolated_block(block, &parameters));

        self.type_parameters.pop().unwrap();

        Ok(lume_hir::TraitMethodImplementation {
            id,
            name,
            parameters,
            type_parameters,
            return_type,
            block,
            location,
        })
    }

    fn expand_self_name(
        &mut self,
        name: lume_ast::Identifier,
        type_params: &[lume_ast::TypeParameter],
    ) -> Result<lume_hir::Path> {
        let self_type_args = type_params
            .iter()
            .map(|param| {
                lume_ast::Type::Named(Box::new(lume_ast::NamedType {
                    name: lume_ast::Path::rooted(lume_ast::PathSegment::ty(param.name.name.clone())),
                }))
            })
            .collect::<Vec<_>>();

        let self_name = lume_ast::PathSegment::Type {
            location: name.location.clone(),
            name,
            bound_types: self_type_args,
        };

        self.expand_name(self_name)
    }
}

fn lower_visibility(expr: Option<&lume_ast::Visibility>) -> lume_hir::Visibility {
    match expr {
        Some(lume_ast::Visibility::Public { .. }) => lume_hir::Visibility::Public,
        Some(lume_ast::Visibility::Internal { .. }) => lume_hir::Visibility::Internal,
        None | Some(lume_ast::Visibility::Private { .. }) => lume_hir::Visibility::Private,
    }
}
