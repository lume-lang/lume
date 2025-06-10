use error_snippet::Result;
use lume_span::ItemId;

use crate::LowerModule;
use crate::errors::*;

use lume_ast::{self as ast};
use lume_hir::{self as hir, SELF_TYPE_NAME};

impl LowerModule<'_> {
    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    pub(super) fn def_type(&mut self, expr: ast::TypeDefinition) -> Result<lume_hir::Item> {
        match expr {
            ast::TypeDefinition::Struct(t) => self.def_struct(*t),
            ast::TypeDefinition::Trait(t) => self.def_trait(*t),
            ast::TypeDefinition::Enum(t) => self.def_enum(*t),
            ast::TypeDefinition::Alias(t) => self.def_alias(*t),
        }
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn def_struct(&mut self, expr: ast::StructDefinition) -> Result<lume_hir::Item> {
        let name = self.symbol_name(expr.name)?;
        self.current_item = ItemId::from_name(&name);

        let type_parameters = self.type_parameters(expr.type_parameters)?;
        let location = self.location(expr.location);
        let id = self.item_id(&name);

        self.self_type = Some(name.clone());

        let properties = expr
            .properties
            .into_iter()
            .map(|m| self.def_property(m))
            .collect::<Result<Vec<hir::Property>>>()?;

        self.self_type = None;

        Ok(lume_hir::Item::Type(Box::new(hir::TypeDefinition::Struct(Box::new(
            hir::StructDefinition {
                id,
                type_id: None,
                name,
                builtin: expr.builtin,
                type_parameters,
                properties,
                methods: Vec::new(),
                location,
            },
        )))))
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn def_property(&mut self, expr: ast::Property) -> Result<hir::Property> {
        let visibility = lower_visibility(&expr.visibility);
        let name = self.identifier(expr.name);
        let property_type = self.type_ref(expr.property_type)?;
        let location = self.location(expr.location);

        let id = self.item_id(("__PROP", &name, &self.current_item));

        let default_value = if let Some(def) = expr.default_value {
            Some(self.expression(def)?)
        } else {
            None
        };

        Ok(hir::Property {
            id,
            prop_id: None,
            name,
            visibility,
            property_type,
            default_value,
            location,
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    pub(super) fn def_impl(&mut self, expr: ast::Implementation) -> Result<lume_hir::Item> {
        let target = self.type_ref(*expr.name)?;
        let type_parameters = self.type_parameters(expr.type_parameters)?;
        let location = self.location(expr.location);

        self.current_item = ItemId::from_name(&target.name);
        let id = self.impl_id(&target);

        self.self_type = Some(target.name.clone());

        let methods = expr
            .methods
            .into_iter()
            .map(|m| self.def_impl_method(m))
            .collect::<Result<Vec<hir::MethodDefinition>>>()?;

        self.self_type = None;

        Ok(lume_hir::Item::Impl(Box::new(hir::Implementation {
            id,
            impl_id: None,
            target: Box::new(target),
            methods,
            type_parameters,
            location,
        })))
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn def_impl_method(&mut self, expr: ast::MethodDefinition) -> Result<hir::MethodDefinition> {
        let visibility = lower_visibility(&expr.visibility);
        let name = self.identifier(expr.name);
        let type_parameters = self.type_parameters(expr.type_parameters)?;
        let parameters = self.parameters(expr.parameters, true)?;
        let return_type = self.opt_type_ref(expr.return_type.map(|f| *f))?;
        let location = self.location(expr.location);

        let id = self.item_id((&name, &self.current_item));

        let block = if expr.external {
            None
        } else {
            Some(self.isolated_block(expr.block, &parameters))
        };

        Ok(hir::MethodDefinition {
            id,
            method_id: None,
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
    fn def_trait(&mut self, expr: ast::TraitDefinition) -> Result<lume_hir::Item> {
        let name = self.symbol_name(expr.name)?;
        let type_parameters = self.type_parameters(expr.type_parameters)?;
        let location = self.location(expr.location);
        let id = self.item_id(&name);

        self.self_type = Some(name.clone());

        let methods = expr
            .methods
            .into_iter()
            .map(|m| self.def_trait_methods(m))
            .collect::<Result<Vec<hir::TraitMethodDefinition>>>()?;

        self.self_type = None;

        Ok(lume_hir::Item::Type(Box::new(hir::TypeDefinition::Trait(Box::new(
            hir::TraitDefinition {
                id,
                type_id: None,
                name,
                type_parameters,
                methods,
                location,
            },
        )))))
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn def_trait_methods(&mut self, expr: ast::TraitMethodDefinition) -> Result<hir::TraitMethodDefinition> {
        let visibility = lower_visibility(&expr.visibility);
        let name = self.identifier(expr.name);
        let type_parameters = self.type_parameters(expr.type_parameters)?;
        let parameters = self.parameters(expr.parameters, true)?;
        let return_type = self.opt_type_ref(expr.return_type.map(|f| *f))?;
        let location = self.location(expr.location);

        let id = self.item_id((&name, &self.current_item));
        let block = expr.block.map(|b| self.isolated_block(b, &parameters));

        Ok(hir::TraitMethodDefinition {
            id,
            method_id: None,
            name,
            visibility,
            type_parameters,
            parameters,
            return_type,
            block,
            location,
        })
    }

    fn def_enum(&self, expr: ast::EnumDefinition) -> Result<lume_hir::Item> {
        let name = self.symbol_name(expr.name)?;
        let location = self.location(expr.location);
        let id = self.item_id(&name);

        let cases = expr
            .cases
            .into_iter()
            .map(|c| self.def_enum_case(c))
            .collect::<Result<Vec<hir::EnumDefinitionCase>>>()?;

        Ok(lume_hir::Item::Type(Box::new(hir::TypeDefinition::Enum(Box::new(
            hir::EnumDefinition {
                id,
                type_id: None,
                name,
                cases,
                location,
            },
        )))))
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn def_enum_case(&self, expr: ast::EnumDefinitionCase) -> Result<hir::EnumDefinitionCase> {
        let name = self.symbol_name(expr.name)?;
        let location = self.location(expr.location);

        let parameters = expr
            .parameters
            .into_iter()
            .map(|c| self.type_ref(*c))
            .collect::<Result<Vec<hir::Type>>>()?;

        let symbol = hir::EnumDefinitionCase {
            name,
            parameters: parameters.into_iter().map(Box::new).collect(),
            location,
        };

        Ok(symbol)
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn def_alias(&self, expr: ast::AliasDefinition) -> Result<lume_hir::Item> {
        let name = self.symbol_name(expr.name)?;
        let definition = self.type_ref(*expr.definition)?;
        let location = self.location(expr.location);
        let id = self.item_id(&name);

        Ok(lume_hir::Item::Type(Box::new(hir::TypeDefinition::Alias(Box::new(
            hir::AliasDefinition {
                id,
                type_id: None,
                name,
                definition: Box::new(definition),
                location,
            },
        )))))
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    pub(super) fn def_function(&mut self, expr: ast::FunctionDefinition) -> Result<lume_hir::Item> {
        let visibility = lower_visibility(&expr.visibility);
        let name = self.symbol_name(expr.name)?;
        let type_parameters = self.type_parameters(expr.type_parameters)?;
        let parameters = self.parameters(expr.parameters, false)?;
        let return_type = self.opt_type_ref(expr.return_type.map(|f| *f))?;
        let location = self.location(expr.location);

        self.current_item = ItemId::from_name(&name);
        let id = self.item_id(&name);

        let block = if expr.external {
            None
        } else {
            Some(self.isolated_block(expr.block, &parameters))
        };

        Ok(lume_hir::Item::Function(Box::new(hir::FunctionDefinition {
            id,
            func_id: None,
            visibility,
            name,
            type_parameters,
            parameters,
            return_type,
            block,
            location,
        })))
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn parameters(&mut self, params: Vec<ast::Parameter>, allow_self: bool) -> Result<Vec<hir::Parameter>> {
        params
            .into_iter()
            .enumerate()
            .map(|(index, param)| {
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

                self.parameter(param)
            })
            .collect::<Result<Vec<_>>>()
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn parameter(&mut self, param: ast::Parameter) -> Result<hir::Parameter> {
        let name = self.identifier(param.name);
        let param_type = self.type_ref(param.param_type)?;
        let location = self.location(param.location);

        Ok(hir::Parameter {
            name,
            param_type,
            location,
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    pub(super) fn def_use(&mut self, expr: ast::UseTrait) -> Result<lume_hir::Item> {
        let type_parameters = self.type_parameters(expr.type_parameters)?;

        let name = self.type_ref(*expr.name)?;
        let target = self.type_ref(*expr.target)?;

        self.self_type = Some(name.name.clone());
        let methods = self.def_use_methods(expr.methods)?;
        let location = self.location(expr.location);

        self.self_type = None;
        self.current_item = ItemId::from_name(&[&name.name, &target.name]);
        let id = self.item_id((&target, &name));

        Ok(lume_hir::Item::Use(Box::new(hir::TraitImplementation {
            id,
            use_id: None,
            name: Box::new(name),
            target: Box::new(target),
            methods,
            type_parameters,
            location,
        })))
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn def_use_methods(
        &mut self,
        methods: Vec<ast::TraitMethodImplementation>,
    ) -> Result<Vec<hir::TraitMethodImplementation>> {
        methods
            .into_iter()
            .map(|m| self.def_use_method(m))
            .collect::<Result<Vec<_>>>()
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn def_use_method(&mut self, expr: ast::TraitMethodImplementation) -> Result<hir::TraitMethodImplementation> {
        let visibility = lower_visibility(&expr.visibility);
        let name = self.symbol_name(expr.name)?;
        let parameters = self.parameters(expr.parameters, true)?;
        let type_parameters = self.type_parameters(expr.type_parameters)?;
        let return_type = self.opt_type_ref(expr.return_type.map(|f| *f))?;
        let block = self.isolated_block(expr.block, &parameters);

        let id = self.item_id((&name, &self.current_item));
        let location = self.location(expr.location);

        Ok(hir::TraitMethodImplementation {
            id,
            method_id: None,
            visibility,
            name,
            parameters,
            type_parameters,
            return_type,
            block,
            location,
        })
    }
}

fn lower_visibility(expr: &ast::Visibility) -> lume_hir::Visibility {
    match expr {
        ast::Visibility::Public { .. } => lume_hir::Visibility::Public,
        ast::Visibility::Private { .. } => lume_hir::Visibility::Private,
    }
}
