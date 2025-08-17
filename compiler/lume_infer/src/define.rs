use std::{
    collections::{BTreeMap, HashSet},
    sync::LazyLock,
};

use error_snippet::Result;
use lume_hir::{Path, PathSegment, TypeId, TypeParameterId};
use lume_span::{DefId, PackageId};
use lume_types::{Enum, Struct, Trait, TypeKind, TypeRef, UserType, WithTypeParameters};

use crate::TyInferCtx;

static INTRINSIC_METHODS: LazyLock<HashSet<&'static str>> = LazyLock::new(|| {
    HashSet::from([
        "std::Boolean::==",
        "std::Boolean::!=",
        "std::Int8::==",
        "std::Int8::!=",
        "std::Int8::<",
        "std::Int8::<=",
        "std::Int8::>",
        "std::Int8::>=",
        "std::Int8::+",
        "std::Int8::-",
        "std::Int8::*",
        "std::Int8::/",
        "std::Int16::==",
        "std::Int16::!=",
        "std::Int16::<",
        "std::Int16::<=",
        "std::Int16::>",
        "std::Int16::>=",
        "std::Int16::+",
        "std::Int16::-",
        "std::Int16::*",
        "std::Int16::/",
        "std::Int32::==",
        "std::Int32::!=",
        "std::Int32::<",
        "std::Int32::<=",
        "std::Int32::>",
        "std::Int32::>=",
        "std::Int32::+",
        "std::Int32::-",
        "std::Int32::*",
        "std::Int32::/",
        "std::Int64::==",
        "std::Int64::!=",
        "std::Int64::<",
        "std::Int64::<=",
        "std::Int64::>",
        "std::Int64::>=",
        "std::Int64::+",
        "std::Int64::-",
        "std::Int64::*",
        "std::Int64::/",
        "std::UInt8::==",
        "std::UInt8::!=",
        "std::UInt8::<",
        "std::UInt8::<=",
        "std::UInt8::>",
        "std::UInt8::>=",
        "std::UInt8::+",
        "std::UInt8::-",
        "std::UInt8::*",
        "std::UInt8::/",
        "std::UInt16::==",
        "std::UInt16::!=",
        "std::UInt16::<",
        "std::UInt16::<=",
        "std::UInt16::>",
        "std::UInt16::>=",
        "std::UInt16::+",
        "std::UInt16::-",
        "std::UInt16::*",
        "std::UInt16::/",
        "std::UInt32::==",
        "std::UInt32::!=",
        "std::UInt32::<",
        "std::UInt32::<=",
        "std::UInt32::>",
        "std::UInt32::>=",
        "std::UInt32::+",
        "std::UInt32::-",
        "std::UInt32::*",
        "std::UInt32::/",
        "std::UInt64::==",
        "std::UInt64::!=",
        "std::UInt64::<",
        "std::UInt64::<=",
        "std::UInt64::>",
        "std::UInt64::>=",
        "std::UInt64::+",
        "std::UInt64::-",
        "std::UInt64::*",
        "std::UInt64::/",
        "std::Float::==",
        "std::Float::!=",
        "std::Float::<",
        "std::Float::<=",
        "std::Float::>",
        "std::Float::>=",
        "std::Float::+",
        "std::Float::-",
        "std::Float::*",
        "std::Float::/",
        "std::Double::==",
        "std::Double::!=",
        "std::Double::<",
        "std::Double::<=",
        "std::Double::>",
        "std::Double::>=",
        "std::Double::+",
        "std::Double::-",
        "std::Double::*",
        "std::Double::/",
    ])
});

impl TyInferCtx {
    #[tracing::instrument(level = "DEBUG", skip_all)]
    pub(crate) fn define_types(&mut self) {
        let mut hir = std::mem::take(&mut self.hir);

        for (_, symbol) in &mut hir.items {
            if let lume_hir::Item::Type(ty) = symbol {
                self.define_type(ty);
            }
        }

        self.hir = hir;
    }

    fn define_type(&mut self, ty: &mut lume_hir::TypeDefinition) {
        match ty {
            lume_hir::TypeDefinition::Struct(struct_def) => {
                let name = struct_def.name.clone();
                let kind = TypeKind::User(UserType::Struct(Box::new(Struct::new(struct_def.as_ref()))));

                let type_id = if let Some(first_root) = name.root.first()
                    && first_root.name().as_str() == "std"
                    && let Some(std_id) = std_type_id(&name)
                {
                    std_id
                } else {
                    self.tdb_mut().type_alloc(struct_def.id.package, name, kind)
                };

                struct_def.type_id = Some(type_id);
            }
            lume_hir::TypeDefinition::Trait(trait_def) => {
                let name = trait_def.name.clone();
                let kind = TypeKind::User(UserType::Trait(Box::new(Trait::new(trait_def.as_ref()))));
                let type_id = self.tdb_mut().type_alloc(trait_def.id.package, name, kind);

                trait_def.type_id = Some(type_id);
            }
            lume_hir::TypeDefinition::Enum(enum_def) => {
                let name = enum_def.name.clone();
                let kind = TypeKind::User(UserType::Enum(Box::new(Enum::new(enum_def.as_ref()))));
                let type_id = self.tdb_mut().type_alloc(enum_def.id.package, name, kind);

                enum_def.type_id = Some(type_id);
            }
        }
    }
}

fn std_type_id(name: &Path) -> Option<TypeId> {
    match name {
        n if n.is_name_match(&Path::void()) => Some(lume_types::TYPEREF_VOID_ID),
        n if n.is_name_match(&Path::boolean()) => Some(lume_types::TYPEREF_BOOL_ID),
        n if n.is_name_match(&Path::i8()) => Some(lume_types::TYPEREF_INT8_ID),
        n if n.is_name_match(&Path::i16()) => Some(lume_types::TYPEREF_INT16_ID),
        n if n.is_name_match(&Path::i32()) => Some(lume_types::TYPEREF_INT32_ID),
        n if n.is_name_match(&Path::i64()) => Some(lume_types::TYPEREF_INT64_ID),
        n if n.is_name_match(&Path::u8()) => Some(lume_types::TYPEREF_UINT8_ID),
        n if n.is_name_match(&Path::u16()) => Some(lume_types::TYPEREF_UINT16_ID),
        n if n.is_name_match(&Path::u32()) => Some(lume_types::TYPEREF_UINT32_ID),
        n if n.is_name_match(&Path::u64()) => Some(lume_types::TYPEREF_UINT64_ID),
        n if n.is_name_match(&Path::f32()) => Some(lume_types::TYPEREF_FLOAT32_ID),
        n if n.is_name_match(&Path::f64()) => Some(lume_types::TYPEREF_FLOAT64_ID),
        n if n.is_name_match(&Path::string()) => Some(lume_types::TYPEREF_STRING_ID),
        n if n.is_name_match(&Path::pointer()) => Some(lume_types::TYPEREF_POINTER_ID),
        n if n.is_name_match(&Path::array()) => Some(lume_types::TYPEREF_ARRAY_ID),
        _ => None,
    }
}

impl TyInferCtx {
    #[tracing::instrument(level = "DEBUG", skip_all)]
    pub(crate) fn define_functions(&mut self) {
        let mut hir = std::mem::take(&mut self.hir);

        for (_, item) in &mut hir.items {
            if let lume_hir::Item::Function(func) = item {
                let name = func.name.clone();
                let visibility = func.visibility;
                let func_id = self.tdb_mut().func_alloc(func.id, name, visibility);

                func.func_id = Some(func_id);
            }
        }

        self.hir = hir;
    }
}

impl TyInferCtx {
    #[tracing::instrument(level = "DEBUG", skip_all)]
    pub(crate) fn define_implementations(&mut self) {
        let mut hir = std::mem::take(&mut self.hir);

        for (_, item) in &mut hir.items {
            if let lume_hir::Item::Impl(implementation) = item {
                let target = implementation.target.name.clone();
                let impl_id = self.tdb_mut().impl_alloc(target);

                implementation.impl_id = Some(impl_id);
            }
        }

        self.hir = hir;
    }
}

impl TyInferCtx {
    #[tracing::instrument(level = "DEBUG", skip_all)]
    pub(crate) fn define_trait_implementations(&mut self) {
        let mut hir = std::mem::take(&mut self.hir);

        for (_, item) in &mut hir.items {
            if let lume_hir::Item::TraitImpl(trait_impl) = item {
                trait_impl.use_id = Some(self.tdb_mut().use_alloc());
            }
        }

        self.hir = hir;
    }
}

impl TyInferCtx {
    #[tracing::instrument(level = "DEBUG", skip_all)]
    pub(crate) fn define_fields(&mut self) -> Result<()> {
        let mut hir = std::mem::take(&mut self.hir);

        for (_, item) in &mut hir.items {
            if let lume_hir::Item::Type(ty) = item {
                self.define_fields_type(ty)?;
            }
        }

        self.hir = hir;

        Ok(())
    }

    fn define_fields_type(&mut self, ty: &mut lume_hir::TypeDefinition) -> Result<()> {
        if let lume_hir::TypeDefinition::Struct(struct_def) = ty {
            let type_id = struct_def.type_id.unwrap();

            for (index, field) in struct_def.fields_mut().enumerate() {
                let field_name = field.name.name.clone();
                let visibility = field.visibility;

                let field_id = self
                    .tdb_mut()
                    .field_alloc(index, type_id, field_name.clone(), visibility)?;

                field.field_id = Some(field_id);
            }
        }

        Ok(())
    }
}

impl TyInferCtx {
    #[tracing::instrument(level = "DEBUG", skip_all)]
    pub(crate) fn define_trait_methods(&mut self) -> Result<()> {
        let mut hir = std::mem::take(&mut self.hir);

        for (_, item) in &mut hir.items {
            match item {
                lume_hir::Item::Type(ty) => {
                    if let lume_hir::TypeDefinition::Trait(tr) = &mut **ty {
                        self.define_trait_def_methods(tr)?;
                    }
                }
                lume_hir::Item::TraitImpl(u) => self.define_trait_impl_methods(u)?,
                _ => (),
            }
        }

        self.hir = hir;

        Ok(())
    }

    fn define_trait_def_methods(&mut self, trait_def: &mut lume_hir::TraitDefinition) -> Result<()> {
        let type_id = trait_def.type_id.unwrap();
        let type_ref = TypeRef::new(type_id, trait_def.location);

        for method in &mut trait_def.methods {
            let method_name = method.name.clone();
            let mut qualified_name =
                Path::with_root(trait_def.name.clone(), PathSegment::callable(method_name.clone()));

            qualified_name.location = method_name.location;

            let method_id = self.tdb_mut().method_alloc(
                method.id,
                type_ref.clone(),
                qualified_name,
                method.visibility,
                lume_types::MethodKind::TraitDefinition,
            )?;

            method.method_id = Some(method_id);
        }

        Ok(())
    }

    fn define_trait_impl_methods(&mut self, trait_impl: &mut lume_hir::TraitImplementation) -> Result<()> {
        let type_ref = self.mk_type_ref(trait_impl.target.as_ref())?;

        for method in &mut trait_impl.methods {
            let method_name = method.name.clone();
            let mut qualified_name = Path::with_root(
                trait_impl.target.name.clone(),
                PathSegment::callable(method_name.clone()),
            );

            qualified_name.location = method_name.location;

            let method_id = self.tdb_mut().method_alloc(
                method.id,
                type_ref.clone(),
                qualified_name,
                method.visibility,
                lume_types::MethodKind::TraitImplementation,
            )?;

            method.method_id = Some(method_id);
        }

        Ok(())
    }
}

impl TyInferCtx {
    #[tracing::instrument(level = "DEBUG", skip_all)]
    pub(crate) fn define_type_parameters(&mut self) -> Result<()> {
        let mut hir = std::mem::take(&mut self.hir);

        for (_, item) in &mut hir.items {
            match item {
                lume_hir::Item::Type(t) => self.define_type_type_params(t)?,
                lume_hir::Item::Impl(i) => self.define_impl_type_params(i)?,
                lume_hir::Item::TraitImpl(u) => self.define_trait_impl_type_params(u)?,
                lume_hir::Item::Function(f) => self.define_func_type_params(f)?,
            }
        }

        self.hir = hir;

        Ok(())
    }

    fn define_type_type_params(&mut self, ty: &mut lume_hir::TypeDefinition) -> Result<()> {
        let package_id = ty.id().package;

        match ty {
            lume_hir::TypeDefinition::Struct(struct_def) => {
                let type_id = struct_def.type_id.unwrap();

                for type_param in &mut struct_def.type_parameters.iter_mut() {
                    let type_param_id = self.tdb_mut().type_param_alloc(type_param.name.name.clone());

                    type_param.type_param_id = Some(type_param_id);
                    type_param.type_id = Some(self.wrap_type_param(package_id, type_param_id));

                    self.tdb_mut().push_type_param(type_id, type_param_id)?;
                }
            }
            lume_hir::TypeDefinition::Trait(trait_def) => {
                let type_id = trait_def.type_id.unwrap();

                for type_param in &mut trait_def.type_parameters.iter_mut() {
                    let type_param_id = self.tdb_mut().type_param_alloc(type_param.name.name.clone());

                    type_param.type_param_id = Some(type_param_id);
                    type_param.type_id = Some(self.wrap_type_param(package_id, type_param_id));

                    self.tdb_mut().push_type_param(type_id, type_param_id)?;
                }

                for method in &mut trait_def.methods {
                    let method_id = method.method_id.unwrap();

                    for type_param in &mut method.type_parameters.iter_mut() {
                        let type_param_id = self.tdb_mut().type_param_alloc(type_param.name.name.clone());

                        type_param.type_param_id = Some(type_param_id);
                        type_param.type_id = Some(self.wrap_type_param(package_id, type_param_id));

                        self.tdb_mut().push_type_param(method_id, type_param_id)?;
                    }
                }
            }
            lume_hir::TypeDefinition::Enum(enum_def) => {
                let type_id = enum_def.type_id.unwrap();

                for type_param in &mut enum_def.type_parameters.iter_mut() {
                    let type_param_id = self.tdb_mut().type_param_alloc(type_param.name.name.clone());

                    type_param.type_param_id = Some(type_param_id);
                    type_param.type_id = Some(self.wrap_type_param(package_id, type_param_id));

                    self.tdb_mut().push_type_param(type_id, type_param_id)?;
                }
            }
        }

        Ok(())
    }

    fn define_impl_type_params(&mut self, implementation: &mut lume_hir::Implementation) -> Result<()> {
        let impl_id = implementation.impl_id.unwrap();
        let package_id = implementation.id.package;

        for type_param in &mut implementation.type_parameters.iter_mut() {
            let type_param_id = self.tdb_mut().type_param_alloc(type_param.name.name.clone());

            type_param.type_param_id = Some(type_param_id);
            type_param.type_id = Some(self.wrap_type_param(package_id, type_param_id));

            self.tdb_mut().push_type_param(impl_id, type_param_id)?;
        }

        let type_ref = self.mk_type_ref_generic(
            implementation.target.as_ref(),
            &implementation.type_parameters.as_refs(),
        )?;

        let is_type_intrinsic = type_ref.is_bool() || type_ref.is_integer() || type_ref.is_float();

        for method in &mut implementation.methods {
            let method_name = method.name.clone();

            let mut qualified_name = Path::with_root(
                implementation.target.name.clone(),
                PathSegment::callable(method_name.clone()),
            );

            let method_kind = if is_type_intrinsic && INTRINSIC_METHODS.contains(format!("{qualified_name:+}").as_str())
            {
                lume_types::MethodKind::Intrinsic
            } else {
                lume_types::MethodKind::Implementation
            };

            qualified_name.location = method_name.location;

            let method_id = self.tdb_mut().method_alloc(
                method.id,
                type_ref.clone(),
                qualified_name,
                method.visibility,
                method_kind,
            )?;

            method.method_id = Some(method_id);

            for type_param in &mut method.type_parameters.iter_mut() {
                let type_param_id = self.tdb_mut().type_param_alloc(type_param.name.name.clone());

                type_param.type_param_id = Some(type_param_id);
                type_param.type_id = Some(self.wrap_type_param(package_id, type_param_id));

                self.tdb_mut().push_type_param(method_id, type_param_id)?;
            }
        }

        Ok(())
    }

    fn define_trait_impl_type_params(&mut self, trait_impl: &mut lume_hir::TraitImplementation) -> Result<()> {
        let use_id = trait_impl.use_id.unwrap();
        let package_id = trait_impl.id.package;

        for type_param in &mut trait_impl.type_parameters.iter_mut() {
            let type_param_id = self.tdb_mut().type_param_alloc(type_param.name.name.clone());

            type_param.type_param_id = Some(type_param_id);
            type_param.type_id = Some(self.wrap_type_param(package_id, type_param_id));

            self.tdb_mut().push_type_param(use_id, type_param_id)?;
        }

        let trait_ref = self.mk_type_ref_generic(trait_impl.name.as_ref(), &trait_impl.type_parameters.as_refs())?;

        let target_ref = self.mk_type_ref_generic(trait_impl.target.as_ref(), &trait_impl.type_parameters.as_refs())?;

        let trait_impl_ref = self.tdb_mut().use_mut(use_id).unwrap();
        trait_impl_ref.trait_ = trait_ref;
        trait_impl_ref.target = target_ref;

        for method in &mut trait_impl.methods {
            let method_id = method.method_id.unwrap();

            for type_param in &mut method.type_parameters.iter_mut() {
                let type_param_id = self.tdb_mut().type_param_alloc(type_param.name.name.clone());

                type_param.type_param_id = Some(type_param_id);
                type_param.type_id = Some(self.wrap_type_param(package_id, type_param_id));

                self.tdb_mut().push_type_param(method_id, type_param_id)?;
            }
        }

        Ok(())
    }

    fn define_func_type_params(&mut self, func: &mut lume_hir::FunctionDefinition) -> Result<()> {
        let func_id = func.func_id.unwrap();

        for type_param in &mut func.type_parameters.iter_mut() {
            let type_param_id = self.tdb_mut().type_param_alloc(type_param.name.name.clone());

            type_param.type_param_id = Some(type_param_id);
            type_param.type_id = Some(self.wrap_type_param(func.id.package, type_param_id));

            self.tdb_mut().push_type_param(func_id, type_param_id)?;
        }

        Ok(())
    }

    fn wrap_type_param(&mut self, pkg: PackageId, type_param_id: TypeParameterId) -> TypeId {
        let name = self.tdb().type_parameter(type_param_id).unwrap().name.clone();

        let symbol_name = Path {
            name: PathSegment::ty(name),
            root: Vec::new(),
            location: lume_span::Location::empty(),
        };

        self.tdb_mut()
            .type_alloc(pkg, symbol_name, TypeKind::TypeParameter(type_param_id))
    }
}

impl TyInferCtx {
    #[tracing::instrument(level = "DEBUG", skip_all)]
    pub(crate) fn define_type_constraints(&mut self) -> Result<()> {
        let mut hir = std::mem::take(&mut self.hir);

        for (_, item) in &mut hir.items {
            match item {
                lume_hir::Item::Type(t) => self.define_type_type_constraints(t)?,
                lume_hir::Item::Impl(i) => self.define_impl_type_constraints(i)?,
                lume_hir::Item::TraitImpl(u) => self.define_trait_impl_type_constraints(u)?,
                lume_hir::Item::Function(f) => self.define_func_type_constraints(f)?,
            }
        }

        self.hir = hir;

        Ok(())
    }

    fn define_type_type_constraints(&mut self, ty: &mut lume_hir::TypeDefinition) -> Result<()> {
        match ty {
            lume_hir::TypeDefinition::Struct(struct_def) => {
                let type_id = struct_def.type_id.unwrap();
                let type_params = self.tdb().type_params_of(type_id)?.to_owned();

                self.lower_type_constraints(&struct_def.type_parameters.inner, &type_params)?;
            }
            lume_hir::TypeDefinition::Trait(trait_def) => {
                let type_id = trait_def.type_id.unwrap();
                let type_params = type_id.type_params(self.tdb())?.to_owned();

                self.lower_type_constraints(&trait_def.type_parameters.inner, &type_params)?;

                for method in &trait_def.methods {
                    let method_id = method.method_id.unwrap();
                    let type_params = self.tdb().type_params_of(method_id)?.to_owned();

                    self.lower_type_constraints(&method.type_parameters.inner, &type_params)?;
                }
            }
            lume_hir::TypeDefinition::Enum(enum_def) => {
                let type_id = enum_def.type_id.unwrap();
                let type_params = self.tdb().type_params_of(type_id)?.to_owned();

                self.lower_type_constraints(&enum_def.type_parameters.inner, &type_params)?;
            }
        }

        Ok(())
    }

    fn define_impl_type_constraints(&mut self, implementation: &mut lume_hir::Implementation) -> Result<()> {
        let impl_id = implementation.impl_id.unwrap();
        let type_params = self.tdb().type_params_of(impl_id)?.to_owned();

        self.lower_type_constraints(&implementation.type_parameters.inner, &type_params)?;

        for method in &implementation.methods {
            let method_id = method.method_id.unwrap();
            let type_params = self.tdb().type_params_of(method_id)?.to_owned();

            self.lower_type_constraints(&method.type_parameters.inner, &type_params)?;
        }

        Ok(())
    }

    fn define_trait_impl_type_constraints(&mut self, trait_impl: &mut lume_hir::TraitImplementation) -> Result<()> {
        let use_id = trait_impl.use_id.unwrap();
        let type_params = self.tdb().type_params_of(use_id)?.to_owned();

        self.lower_type_constraints(&trait_impl.type_parameters.inner, &type_params)?;

        for method in &trait_impl.methods {
            let method_id = method.method_id.unwrap();
            let type_params = self.tdb().type_params_of(method_id)?.to_owned();

            self.lower_type_constraints(&method.type_parameters.inner, &type_params)?;
        }

        Ok(())
    }

    fn define_func_type_constraints(&mut self, func: &mut lume_hir::FunctionDefinition) -> Result<()> {
        let func_id = func.func_id.unwrap();
        let type_params = self.tdb().function(func_id).unwrap().type_parameters.clone();

        self.lower_type_constraints(&func.type_parameters.inner, &type_params)?;

        Ok(())
    }

    fn lower_type_constraints(&mut self, hir: &[lume_hir::TypeParameter], ids: &[TypeParameterId]) -> Result<()> {
        for (param_id, hir_param) in ids.iter().zip(hir.iter()) {
            for type_constraint in &hir_param.constraints {
                let lowered_constraint = self.mk_type_ref(type_constraint)?;

                self.tdb_mut()
                    .type_parameter_mut(*param_id)
                    .unwrap()
                    .constraints
                    .push(lowered_constraint);
            }
        }

        Ok(())
    }
}

impl TyInferCtx {
    #[tracing::instrument(level = "DEBUG", skip_all)]
    pub(crate) fn define_field_types(&mut self) -> Result<()> {
        let hir = std::mem::take(&mut self.hir);

        for (_, item) in &hir.items {
            if let lume_hir::Item::Type(ty) = item {
                self.define_field_type_on_type(ty)?;
            }
        }

        self.hir = hir;

        Ok(())
    }

    fn define_field_type_on_type(&mut self, ty: &lume_hir::TypeDefinition) -> Result<()> {
        if let lume_hir::TypeDefinition::Struct(struct_def) = ty {
            for field in struct_def.fields() {
                let field_id = field.field_id.unwrap();
                let type_ref = self.mk_type_ref_generic(&field.field_type, &struct_def.type_parameters.as_refs())?;

                self.tdb_mut().field_mut(field_id).unwrap().field_type = type_ref;
            }
        }

        Ok(())
    }
}

impl TyInferCtx {
    #[tracing::instrument(level = "DEBUG", skip_all)]
    pub(crate) fn define_method_bodies(&mut self) -> Result<()> {
        // TODO: this is not a very good way of handling mutability issues.
        for (_, item) in &self.hir.clone().items {
            match item {
                lume_hir::Item::Type(t) => self.define_method_bodies_type(t)?,
                lume_hir::Item::Function(f) => self.define_func_body(f)?,
                lume_hir::Item::TraitImpl(u) => self.define_method_bodies_trait_impl(u)?,
                lume_hir::Item::Impl(i) => self.define_method_bodies_impl(i)?,
            }
        }

        Ok(())
    }

    fn define_method_bodies_type(&mut self, ty: &lume_hir::TypeDefinition) -> Result<()> {
        if let lume_hir::TypeDefinition::Trait(trait_def) = &ty {
            for method in &trait_def.methods {
                let method_id = method.method_id.unwrap();

                for param in &method.parameters {
                    let name = param.name.name.clone();
                    let type_ref = self.type_of_parameter_pre(
                        param,
                        &[
                            &trait_def.type_parameters.as_refs()[..],
                            &method.type_parameters.as_refs()[..],
                        ]
                        .concat(),
                    )?;

                    self.tdb_mut()
                        .method_mut(method_id)
                        .unwrap()
                        .parameters
                        .push(name, type_ref, param.vararg);
                }

                self.tdb_mut().method_mut(method_id).unwrap().return_type = self.mk_type_ref_generic(
                    &method.return_type,
                    &[
                        &trait_def.type_parameters.as_refs()[..],
                        &method.type_parameters.as_refs()[..],
                    ]
                    .concat(),
                )?;
            }
        }

        Ok(())
    }

    fn define_func_body(&mut self, func: &lume_hir::FunctionDefinition) -> Result<()> {
        let func_id = func.func_id.unwrap();

        for param in &func.parameters {
            let name = param.name.name.clone();
            let type_ref = self.type_of_parameter_pre(param, &func.type_parameters.as_refs())?;

            self.tdb_mut()
                .function_mut(func_id)
                .unwrap()
                .parameters
                .push(name, type_ref, param.vararg);
        }

        self.tdb_mut().function_mut(func_id).unwrap().return_type =
            self.mk_type_ref_generic(&func.return_type, &func.type_parameters.as_refs())?;

        Ok(())
    }

    fn define_method_bodies_trait_impl(&mut self, trait_impl: &lume_hir::TraitImplementation) -> Result<()> {
        for method in &trait_impl.methods {
            let method_id = method.method_id.unwrap();

            for param in &method.parameters {
                let name = param.name.name.clone();
                let type_ref = self.type_of_parameter_pre(
                    param,
                    &[
                        &trait_impl.type_parameters.as_refs()[..],
                        &method.type_parameters.as_refs()[..],
                    ]
                    .concat(),
                )?;

                self.tdb_mut()
                    .method_mut(method_id)
                    .unwrap()
                    .parameters
                    .push(name, type_ref, param.vararg);
            }

            self.tdb_mut().method_mut(method_id).unwrap().return_type = self.mk_type_ref_generic(
                &method.return_type,
                &[
                    &trait_impl.type_parameters.as_refs()[..],
                    &method.type_parameters.as_refs()[..],
                ]
                .concat(),
            )?;
        }

        Ok(())
    }

    fn define_method_bodies_impl(&mut self, implementation: &lume_hir::Implementation) -> Result<()> {
        for method in &implementation.methods {
            let method_id = method.method_id.unwrap();

            for param in &method.parameters {
                let name = param.name.name.clone();
                let type_ref = self.type_of_parameter_pre(
                    param,
                    &[
                        &implementation.type_parameters.as_refs()[..],
                        &method.type_parameters.as_refs()[..],
                    ]
                    .concat(),
                )?;

                self.tdb_mut()
                    .method_mut(method_id)
                    .unwrap()
                    .parameters
                    .push(name, type_ref, param.vararg);
            }

            self.tdb_mut().method_mut(method_id).unwrap().return_type = self.mk_type_ref_generic(
                &method.return_type,
                &[
                    &implementation.type_parameters.as_refs()[..],
                    &method.type_parameters.as_refs()[..],
                ]
                .concat(),
            )?;
        }

        Ok(())
    }
}

impl TyInferCtx {
    #[tracing::instrument(level = "DEBUG", skip_all)]
    pub(crate) fn define_scopes(&mut self) -> Result<()> {
        let mut tree = BTreeMap::new();

        for (_, item) in &self.hir.items {
            match item {
                lume_hir::Item::Type(ty) => match ty.as_ref() {
                    lume_hir::TypeDefinition::Struct(f) => self.define_struct_scope(&mut tree, f)?,
                    lume_hir::TypeDefinition::Trait(f) => self.define_trait_scope(&mut tree, f)?,
                    lume_hir::TypeDefinition::Enum(_) => {}
                },
                lume_hir::Item::Impl(f) => self.define_impl_scope(&mut tree, f)?,
                lume_hir::Item::TraitImpl(f) => self.define_use_scope(&mut tree, f)?,
                lume_hir::Item::Function(f) => self.define_function_scope(&mut tree, f)?,
            }
        }

        self.ancestry = tree;

        Ok(())
    }

    fn define_struct_scope(
        &self,
        tree: &mut BTreeMap<DefId, DefId>,
        struct_def: &lume_hir::StructDefinition,
    ) -> Result<()> {
        let parent = DefId::Item(struct_def.id);

        for field in &struct_def.fields {
            let _ = tree.try_insert(field.id, parent);

            if let Some(default) = field.default_value {
                self.define_expr_scope(tree, default, field.id)?;
            }
        }

        Ok(())
    }

    fn define_trait_scope(
        &self,
        tree: &mut BTreeMap<DefId, DefId>,
        trait_def: &lume_hir::TraitDefinition,
    ) -> Result<()> {
        let parent = DefId::Item(trait_def.id);

        for method in &trait_def.methods {
            let _ = tree.try_insert(method.id, parent);

            if let Some(block) = &method.block {
                self.define_block_scope(tree, block, method.id)?;
            }
        }

        Ok(())
    }

    fn define_impl_scope(
        &self,
        tree: &mut BTreeMap<DefId, DefId>,
        implementation: &lume_hir::Implementation,
    ) -> Result<()> {
        let parent = DefId::Item(implementation.id);

        for method in &implementation.methods {
            let _ = tree.try_insert(method.id, parent);

            if let Some(block) = &method.block {
                self.define_block_scope(tree, block, method.id)?;
            }
        }

        Ok(())
    }

    fn define_use_scope(
        &self,
        tree: &mut BTreeMap<DefId, DefId>,
        trait_impl: &lume_hir::TraitImplementation,
    ) -> Result<()> {
        let parent = DefId::Item(trait_impl.id);

        for method in &trait_impl.methods {
            let _ = tree.try_insert(method.id, parent);

            self.define_block_scope(tree, &method.block, method.id)?;
        }

        Ok(())
    }

    fn define_function_scope(
        &self,
        tree: &mut BTreeMap<DefId, DefId>,
        func: &lume_hir::FunctionDefinition,
    ) -> Result<()> {
        if let Some(block) = &func.block {
            let parent = DefId::Item(func.id);

            self.define_block_scope(tree, block, parent)?;
        }

        Ok(())
    }

    fn define_block_scope(
        &self,
        tree: &mut BTreeMap<DefId, DefId>,
        block: &lume_hir::Block,
        parent: DefId,
    ) -> Result<()> {
        for stmt in &block.statements {
            self.define_stmt_scope(tree, *stmt, parent)?;
        }

        Ok(())
    }

    fn define_stmt_scope(
        &self,
        tree: &mut BTreeMap<DefId, DefId>,
        stmt: lume_span::StatementId,
        parent: DefId,
    ) -> Result<()> {
        let stmt_id = DefId::Statement(stmt);
        let _ = tree.try_insert(stmt_id, parent);

        let stmt = self.hir.statement(stmt).unwrap();

        match &stmt.kind {
            lume_hir::StatementKind::Variable(s) => self.define_expr_scope(tree, s.value, stmt_id),
            lume_hir::StatementKind::Break(_) | lume_hir::StatementKind::Continue(_) => Ok(()),
            lume_hir::StatementKind::Final(s) => {
                self.define_expr_scope(tree, s.value, stmt_id)?;

                Ok(())
            }
            lume_hir::StatementKind::Return(s) => {
                if let Some(value) = s.value {
                    self.define_expr_scope(tree, value, stmt_id)
                } else {
                    Ok(())
                }
            }
            lume_hir::StatementKind::InfiniteLoop(s) => self.define_block_scope(tree, &s.block, stmt_id),
            lume_hir::StatementKind::IteratorLoop(s) => {
                self.define_block_scope(tree, &s.block, stmt_id)?;
                self.define_expr_scope(tree, s.collection, stmt_id)?;

                Ok(())
            }
            lume_hir::StatementKind::Expression(s) => self.define_expr_scope(tree, *s, stmt_id),
        }
    }

    fn define_condition_scope(
        &self,
        tree: &mut BTreeMap<DefId, DefId>,
        cond: &lume_hir::Condition,
        parent: DefId,
    ) -> Result<()> {
        if let Some(condition) = cond.condition {
            self.define_expr_scope(tree, condition, parent)?;
        }

        self.define_block_scope(tree, &cond.block, parent)?;

        Ok(())
    }

    fn define_expr_scope(
        &self,
        tree: &mut BTreeMap<DefId, DefId>,
        expr: lume_span::ExpressionId,
        parent: DefId,
    ) -> Result<()> {
        let expr_id = DefId::Expression(expr);
        let _ = tree.try_insert(expr_id, parent);

        let expr = self.hir.expression(expr).unwrap();

        match &expr.kind {
            lume_hir::ExpressionKind::Assignment(s) => {
                self.define_expr_scope(tree, s.target, expr_id)?;
                self.define_expr_scope(tree, s.value, expr_id)?;

                Ok(())
            }
            lume_hir::ExpressionKind::Cast(s) => self.define_expr_scope(tree, s.source, expr_id),
            lume_hir::ExpressionKind::Construct(s) => {
                for field in &s.fields {
                    self.define_expr_scope(tree, field.value, expr_id)?;
                }

                Ok(())
            }
            lume_hir::ExpressionKind::Binary(s) => {
                self.define_expr_scope(tree, s.lhs, expr_id)?;
                self.define_expr_scope(tree, s.rhs, expr_id)?;

                Ok(())
            }
            lume_hir::ExpressionKind::InstanceCall(s) => {
                self.define_expr_scope(tree, s.callee, expr_id)?;

                for arg in &s.arguments {
                    self.define_expr_scope(tree, *arg, expr_id)?;
                }

                Ok(())
            }
            lume_hir::ExpressionKind::IntrinsicCall(s) => {
                self.define_expr_scope(tree, s.callee(), expr_id)?;

                for arg in &s.arguments {
                    self.define_expr_scope(tree, *arg, expr_id)?;
                }

                Ok(())
            }
            lume_hir::ExpressionKind::If(s) => {
                for case in &s.cases {
                    self.define_condition_scope(tree, case, expr_id)?;
                }

                Ok(())
            }
            lume_hir::ExpressionKind::Logical(s) => {
                self.define_expr_scope(tree, s.lhs, expr_id)?;
                self.define_expr_scope(tree, s.rhs, expr_id)?;

                Ok(())
            }
            lume_hir::ExpressionKind::Member(s) => self.define_expr_scope(tree, s.callee, expr_id),
            lume_hir::ExpressionKind::StaticCall(s) => {
                for arg in &s.arguments {
                    self.define_expr_scope(tree, *arg, expr_id)?;
                }

                Ok(())
            }
            lume_hir::ExpressionKind::Scope(s) => {
                for stmt in &s.body {
                    self.define_stmt_scope(tree, *stmt, expr_id)?;
                }

                Ok(())
            }
            lume_hir::ExpressionKind::Switch(s) => {
                for case in &s.cases {
                    self.define_pat_scope(tree, &case.pattern, expr_id)?;
                    self.define_expr_scope(tree, case.branch, expr_id)?;
                }

                Ok(())
            }
            lume_hir::ExpressionKind::Literal(_)
            | lume_hir::ExpressionKind::Variable(_)
            | lume_hir::ExpressionKind::Field(_)
            | lume_hir::ExpressionKind::Variant(_) => Ok(()),
        }
    }

    fn define_pat_scope(
        &self,
        tree: &mut BTreeMap<DefId, DefId>,
        pat: &lume_hir::Pattern,
        parent: DefId,
    ) -> Result<()> {
        let def_id = pat.id;
        let _ = tree.try_insert(def_id, parent);

        match &pat.kind {
            lume_hir::PatternKind::Literal(_)
            | lume_hir::PatternKind::Identifier(_)
            | lume_hir::PatternKind::Wildcard(_) => Ok(()),
            lume_hir::PatternKind::Variant(var) => {
                for field in &var.fields {
                    self.define_pat_scope(tree, field, def_id)?;
                }

                Ok(())
            }
        }
    }
}
