use std::collections::HashSet;

use error_snippet::Result;
use lume_hir::Node;

use crate::DefinedItem;
use crate::LowerModule;
use crate::errors::*;

use lume_hir::SELF_TYPE_NAME;

impl LowerModule<'_> {
    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    pub(super) fn def_type(&mut self, expr: lume_ast::TypeDefinition) -> Result<lume_hir::Node> {
        match expr {
            lume_ast::TypeDefinition::Struct(t) => self.def_struct(*t),
            lume_ast::TypeDefinition::Trait(t) => self.def_trait(*t),
            lume_ast::TypeDefinition::Enum(t) => self.def_enum(*t),
        }
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn def_struct(&mut self, expr: lume_ast::StructDefinition) -> Result<lume_hir::Node> {
        let id = self.next_node_id();

        let name = self.expand_name(lume_ast::PathSegment::ty(expr.name))?;
        let visibility = lower_visibility(&expr.visibility);
        let type_parameters = self.type_parameters(expr.type_parameters)?;
        let location = self.location(expr.location);

        self.ensure_item_undefined(DefinedItem::Type(name.clone()))?;

        self.self_type = Some(name.clone());

        let mut fields = Vec::with_capacity(expr.fields.len());
        for field in expr.fields {
            fields.push(self.def_field(field)?);
        }

        self.self_type = None;

        Ok(lume_hir::Node::Type(lume_hir::TypeDefinition::Struct(Box::new(
            lume_hir::StructDefinition {
                id,
                name,
                visibility,
                builtin: expr.builtin,
                type_parameters,
                fields,
                location,
            },
        ))))
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn def_field(&mut self, expr: lume_ast::Field) -> Result<lume_hir::Field> {
        let id = self.next_node_id();

        let visibility = lower_visibility(&expr.visibility);
        let name = self.identifier(expr.name);
        let field_type = self.type_ref(expr.field_type)?;
        let location = self.location(expr.location);

        let default_value = if let Some(def) = expr.default_value {
            Some(self.expression(def)?)
        } else {
            None
        };

        Ok(lume_hir::Field {
            id,
            name,
            visibility,
            field_type,
            default_value,
            location,
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    pub(super) fn def_impl(&mut self, expr: lume_ast::Implementation) -> Result<lume_hir::Node> {
        let id = self.next_node_id();

        let target = self.type_ref(*expr.name)?;
        let type_parameters = self.type_parameters(expr.type_parameters)?;
        let location = self.location(expr.location);

        self.self_type = Some(target.name.clone());

        let mut methods = Vec::with_capacity(expr.methods.len());
        for method in expr.methods {
            let method = self.def_impl_method(method)?;
            self.map.nodes.insert(method.id, Node::Method(method.clone()));

            methods.push(method);
        }

        self.self_type = None;

        Ok(lume_hir::Node::Impl(lume_hir::Implementation {
            id,
            target: Box::new(target),
            methods,
            type_parameters,
            location,
        }))
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn def_impl_method(&mut self, expr: lume_ast::MethodDefinition) -> Result<lume_hir::MethodDefinition> {
        let id = self.next_node_id();

        let visibility = lower_visibility(&expr.visibility);
        let name = self.identifier(expr.name);
        let type_parameters = self.type_parameters(expr.type_parameters)?;
        let parameters = self.parameters(expr.parameters, true)?;
        let return_type = self.opt_type_ref(expr.return_type.map(|f| *f))?;
        let location = self.location(expr.location);

        let block = if expr.external {
            None
        } else {
            Some(self.isolated_block(expr.block, &parameters))
        };

        Ok(lume_hir::MethodDefinition {
            id,
            name,
            visibility,
            type_parameters,
            parameters,
            return_type,
            block,
            location,
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn def_trait(&mut self, expr: lume_ast::TraitDefinition) -> Result<lume_hir::Node> {
        let id = self.next_node_id();

        let name = self.expand_self_name(expr.name.clone(), &expr.type_parameters)?;
        let visibility = lower_visibility(&expr.visibility);
        let type_parameters = self.type_parameters(expr.type_parameters)?;
        let location = self.location(expr.location);

        self.ensure_item_undefined(DefinedItem::Type(name.clone()))?;

        self.self_type = Some(name.clone());

        let mut methods = Vec::with_capacity(expr.methods.len());
        for method in expr.methods {
            let method = self.def_trait_methods(method)?;
            self.map.nodes.insert(method.id, Node::TraitMethodDef(method.clone()));

            methods.push(method);
        }

        self.self_type = None;

        Ok(lume_hir::Node::Type(lume_hir::TypeDefinition::Trait(Box::new(
            lume_hir::TraitDefinition {
                id,
                name,
                visibility,
                type_parameters,
                methods,
                location,
            },
        ))))
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn def_trait_methods(&mut self, expr: lume_ast::TraitMethodDefinition) -> Result<lume_hir::TraitMethodDefinition> {
        let id = self.next_node_id();

        let visibility = lower_visibility(&expr.visibility);
        let name = self.identifier(expr.name);
        let type_parameters = self.type_parameters(expr.type_parameters)?;
        let parameters = self.parameters(expr.parameters, true)?;
        let return_type = self.opt_type_ref(expr.return_type.map(|f| *f))?;
        let location = self.location(expr.location);
        let block = expr.block.map(|b| self.isolated_block(b, &parameters));

        Ok(lume_hir::TraitMethodDefinition {
            id,
            name,
            visibility,
            type_parameters,
            parameters,
            return_type,
            block,
            location,
        })
    }

    fn def_enum(&mut self, expr: lume_ast::EnumDefinition) -> Result<lume_hir::Node> {
        let id = self.next_node_id();

        let name = self.expand_name(lume_ast::PathSegment::ty(expr.name))?;
        let type_parameters = self.type_parameters(expr.type_parameters)?;
        let visibility = lower_visibility(&expr.visibility);
        let location = self.location(expr.location);

        self.ensure_item_undefined(DefinedItem::Type(name.clone()))?;

        let mut cases = Vec::with_capacity(expr.cases.len());
        for (idx, case) in expr.cases.into_iter().enumerate() {
            cases.push(self.def_enum_case(idx, case)?);
        }

        Ok(lume_hir::Node::Type(lume_hir::TypeDefinition::Enum(Box::new(
            lume_hir::EnumDefinition {
                id,
                name,
                type_parameters,
                visibility,
                cases,
                location,
            },
        ))))
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn def_enum_case(&self, idx: usize, expr: lume_ast::EnumDefinitionCase) -> Result<lume_hir::EnumDefinitionCase> {
        let name = self.expand_name(lume_ast::PathSegment::ty(expr.name))?;
        let location = self.location(expr.location);

        let mut parameters = Vec::with_capacity(expr.parameters.len());
        for param in expr.parameters {
            parameters.push(self.type_ref(*param)?);
        }

        let symbol = lume_hir::EnumDefinitionCase {
            idx,
            name,
            parameters,
            location,
        };

        Ok(symbol)
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    pub(super) fn def_function(&mut self, expr: lume_ast::FunctionDefinition) -> Result<lume_hir::Node> {
        let id = self.next_node_id();

        let visibility = lower_visibility(&expr.visibility);
        let name = self.expand_name(lume_ast::PathSegment::ty(expr.name))?;
        let type_parameters = self.type_parameters(expr.type_parameters)?;
        let parameters = self.parameters(expr.parameters, false)?;
        let return_type = self.opt_type_ref(expr.return_type.map(|f| *f))?;
        let location = self.location(expr.location);

        self.ensure_item_undefined(DefinedItem::Function(name.clone()))?;

        let block = if expr.external {
            None
        } else {
            Some(self.isolated_block(expr.block, &parameters))
        };

        Ok(lume_hir::Node::Function(lume_hir::FunctionDefinition {
            id,
            visibility,
            name,
            type_parameters,
            parameters,
            return_type,
            block,
            location,
        }))
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn parameters(&mut self, params: Vec<lume_ast::Parameter>, allow_self: bool) -> Result<Vec<lume_hir::Parameter>> {
        let param_len = params.len();
        let mut parameters = Vec::with_capacity(param_len);
        let mut names: HashSet<lume_ast::Identifier> = HashSet::with_capacity(param_len);

        for (index, param) in params.into_iter().enumerate() {
            // Make sure that `self` is the first parameter.
            //
            // While it doesn't change must in the view of the compiler,
            // using `self` as the first parameter is a best practice, since it's
            // so much easier to see whether a method is an instance method or a static method.
            if index > 0 && param.param_type.is_self() {
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

            parameters.push(self.parameter(index, param)?);
        }

        Ok(parameters)
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn parameter(&mut self, index: usize, param: lume_ast::Parameter) -> Result<lume_hir::Parameter> {
        let name = self.identifier(param.name);
        let param_type = self.type_ref(param.param_type)?;
        let location = self.location(param.location);

        Ok(lume_hir::Parameter {
            index,
            name,
            param_type,
            vararg: param.vararg,
            location,
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    pub(super) fn def_trait_impl(&mut self, expr: lume_ast::ImplTrait) -> Result<lume_hir::Node> {
        let id = self.next_node_id();
        let type_parameters = self.type_parameters(expr.type_parameters)?;

        let visibility = lower_visibility(&expr.visibility);
        let name = self.type_ref(*expr.name)?;
        let target = self.type_ref(*expr.target)?;

        self.self_type = Some(target.name.clone());

        let mut methods = Vec::with_capacity(expr.methods.len());
        for method in expr.methods {
            let method = self.def_use_method(method)?;
            self.map.nodes.insert(method.id, Node::TraitMethodImpl(method.clone()));

            methods.push(method);
        }

        let location = self.location(expr.location);

        self.self_type = None;

        Ok(lume_hir::Node::TraitImpl(lume_hir::TraitImplementation {
            id,
            name: Box::new(name),
            target: Box::new(target),
            visibility,
            methods,
            type_parameters,
            location,
        }))
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn def_use_method(
        &mut self,
        expr: lume_ast::TraitMethodImplementation,
    ) -> Result<lume_hir::TraitMethodImplementation> {
        let id = self.next_node_id();

        let visibility = lower_visibility(&expr.visibility);
        let name = self.identifier(expr.name);
        let parameters = self.parameters(expr.parameters, true)?;
        let type_parameters = self.type_parameters(expr.type_parameters)?;
        let return_type = self.opt_type_ref(expr.return_type.map(|f| *f))?;
        let location = self.location(expr.location);

        let block = if expr.external {
            None
        } else {
            Some(self.isolated_block(expr.block, &parameters))
        };

        Ok(lume_hir::TraitMethodImplementation {
            id,
            visibility,
            name,
            parameters,
            type_parameters,
            return_type,
            block,
            location,
        })
    }

    fn expand_self_name(
        &self,
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
            type_arguments: self_type_args,
        };

        self.expand_name(self_name)
    }
}

fn lower_visibility(expr: &lume_ast::Visibility) -> lume_hir::Visibility {
    match expr {
        lume_ast::Visibility::Public { .. } => lume_hir::Visibility::Public,
        lume_ast::Visibility::Private { .. } => lume_hir::Visibility::Private,
    }
}
