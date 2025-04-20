use diag::Result;
use indexmap::IndexMap;

use crate::{
    hir::{self, ExpressionId, Identifier, Location, SymbolName, Visibility},
    thir::ThirBuildCtx,
};

const UNKNOWN_TYPE_ID: TypeId = TypeId(u32::MAX);

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Parameter {
    pub idx: usize,
    pub name: String,
    pub ty: TypeRef,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Parameters {
    pub params: Vec<Parameter>,
}

impl Parameters {
    pub fn new() -> Self {
        Self { params: Vec::new() }
    }

    pub fn push(&mut self, name: String, ty: TypeRef) {
        self.params.push(Parameter {
            idx: self.params.len(),
            name,
            ty,
        });
    }
}

#[derive(serde::Serialize, Hash, Debug, Clone, PartialEq, Eq)]
pub struct FunctionId(pub u32);

impl FunctionId {
    pub fn get<'a>(&'a self, ctx: &'a TypeDatabaseContext) -> &'a Function {
        &ctx.functions[self.0 as usize]
    }

    pub fn get_mut<'a>(&'a self, ctx: &'a mut TypeDatabaseContext) -> &'a mut Function {
        &mut ctx.functions[self.0 as usize]
    }

    pub fn is_private(self, ctx: &TypeDatabaseContext) -> bool {
        self.get(ctx).visibility == Visibility::Private
    }

    pub fn is_public(self, ctx: &TypeDatabaseContext) -> bool {
        self.get(ctx).visibility == Visibility::Public
    }
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Function {
    pub name: SymbolName,
    pub location: Location,
    pub visibility: Visibility,
    pub arguments: Parameters,
    pub return_type: TypeRef,
}

impl Function {
    pub fn alloc(
        ctx: &mut TypeDatabaseContext,
        name: SymbolName,
        visibility: Visibility,
        location: Location,
    ) -> FunctionId {
        let id = ctx.functions.len();
        let function = Function {
            name,
            location,
            visibility,
            arguments: Parameters::new(),
            return_type: TypeRef::unknown(),
        };

        ctx.functions.push(function);
        FunctionId(id as u32)
    }

    pub fn find(ctx: &TypeDatabaseContext, name: SymbolName) -> Option<&Function> {
        ctx.functions.iter().find(|f| f.name == name)
    }
}

#[derive(serde::Serialize, Hash, Debug, Clone, Copy, PartialEq, Eq)]
pub struct PropertyId(pub u32);

impl PropertyId {
    pub fn get<'a>(&'a self, ctx: &'a TypeDatabaseContext) -> &'a Property {
        &ctx.properties[self.0 as usize]
    }

    pub fn get_mut<'a>(&'a self, ctx: &'a mut TypeDatabaseContext) -> &'a mut Property {
        &mut ctx.properties[self.0 as usize]
    }

    pub fn is_private(self, ctx: &TypeDatabaseContext) -> bool {
        self.get(ctx).visibility == Visibility::Private
    }

    pub fn is_public(self, ctx: &TypeDatabaseContext) -> bool {
        self.get(ctx).visibility == Visibility::Public
    }

    pub fn property_type<'a>(&'a self, ctx: &'a TypeDatabaseContext) -> &'a TypeRef {
        &self.get(ctx).property_type
    }

    pub fn set_property_type(&self, ctx: &mut TypeDatabaseContext, ty: TypeRef) {
        self.get_mut(ctx).property_type = ty;
    }
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Property {
    pub visibility: Visibility,
    pub owner: TypeId,
    pub name: Identifier,
    pub property_type: TypeRef,
    pub default_value: Option<ExpressionId>,
    pub location: Location,
}

impl Property {
    pub fn alloc(
        ctx: &mut TypeDatabaseContext,
        owner: TypeId,
        name: Identifier,
        visibility: Visibility,
        location: Location,
    ) -> PropertyId {
        let id = ctx.properties.len();
        let property = Property {
            visibility,
            owner,
            name,
            property_type: TypeRef::unknown(),
            default_value: None,
            location,
        };

        ctx.properties.push(property);
        PropertyId(id as u32)
    }
}

#[derive(serde::Serialize, Hash, Debug, Clone, PartialEq, Eq)]
pub struct MethodId(pub u32);

impl MethodId {
    pub fn get<'a>(&'a self, ctx: &'a TypeDatabaseContext) -> &'a Method {
        &ctx.methods[self.0 as usize]
    }

    pub fn get_mut<'a>(&'a self, ctx: &'a mut TypeDatabaseContext) -> &'a mut Method {
        &mut ctx.methods[self.0 as usize]
    }

    pub fn is_private(&self, ctx: &TypeDatabaseContext) -> bool {
        self.get(ctx).visibility == Visibility::Private
    }

    pub fn is_public(&self, ctx: &TypeDatabaseContext) -> bool {
        self.get(ctx).visibility == Visibility::Public
    }

    pub fn return_type<'a>(&'a self, ctx: &'a TypeDatabaseContext) -> &'a TypeRef {
        &self.get(ctx).return_type
    }

    pub fn set_return_type(&self, ctx: &mut TypeDatabaseContext, ty: TypeRef) {
        self.get_mut(ctx).return_type = ty;
    }

    pub fn add_parameter(&self, ctx: &mut TypeDatabaseContext, name: String, ty: TypeRef) {
        self.get_mut(ctx).parameters.push(name, ty)
    }
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Method {
    pub visibility: Visibility,
    pub callee: TypeRef,
    pub name: Identifier,
    pub parameters: Parameters,
    pub return_type: TypeRef,
    pub location: Location,
}

impl Method {
    pub fn alloc(
        ctx: &mut TypeDatabaseContext,
        class: TypeId,
        name: Identifier,
        visibility: Visibility,
        location: Location,
    ) -> MethodId {
        let id = ctx.methods.len();
        let method = Method {
            visibility,
            callee: TypeRef::new(class),
            name,
            parameters: Parameters::new(),
            return_type: TypeRef::unknown(),
            location,
        };

        ctx.methods.push(method);
        MethodId(id as u32)
    }
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Class {
    pub name: SymbolName,
    pub location: Location,
}

impl Class {
    pub fn new(name: SymbolName, location: Location) -> Self {
        Self { name, location }
    }
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Trait {
    pub name: SymbolName,
    pub location: Location,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Enum {
    pub name: SymbolName,
    pub location: Location,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub enum TypeKind {
    /// The type is a regular user-defined class.
    Class(Box<Class>),

    /// The type is a regular user-defined trait.
    Trait(Box<Trait>),

    /// The type is a regular user-defined enumeration.
    Enum(Box<Enum>),

    /// Represents a non-value.
    Void,
}

#[derive(serde::Serialize, Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeTransport {
    /// The type is fully copied when passed as an argument or returned from a function.
    Copy,

    /// The type uses the same memory location and is passed by reference.
    Reference,
}

#[derive(serde::Serialize, Hash, Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeId(pub u32);

impl TypeId {
    pub fn get<'a>(&'a self, ctx: &'a TypeDatabaseContext) -> &'a Type {
        &ctx.types[self.0 as usize]
    }

    pub fn get_mut<'a>(&'a self, ctx: &'a mut TypeDatabaseContext) -> &'a mut Type {
        &mut ctx.types[self.0 as usize]
    }

    pub fn find(ctx: &TypeDatabaseContext, name: SymbolName) -> TypeId {
        match ctx.types.iter().position(|t| t.name == name) {
            Some(index) => TypeId(index as u32),
            None => panic!("no type of name {:?} was found", name),
        }
    }

    pub fn transport(&self, ctx: &TypeDatabaseContext) -> TypeTransport {
        self.get(ctx).transport
    }

    pub fn is_copied(&self, ctx: &TypeDatabaseContext) -> bool {
        self.get(ctx).transport == TypeTransport::Copy
    }

    pub fn is_referenced(&self, ctx: &TypeDatabaseContext) -> bool {
        self.get(ctx).transport == TypeTransport::Reference
    }

    pub fn set_transport(&self, ctx: &mut TypeDatabaseContext, transport: TypeTransport) {
        self.get_mut(ctx).transport = transport;
    }

    pub fn set_copied(&self, ctx: &mut TypeDatabaseContext) {
        self.set_transport(ctx, TypeTransport::Copy);
    }

    pub fn set_referenced(&self, ctx: &mut TypeDatabaseContext) {
        self.set_transport(ctx, TypeTransport::Reference);
    }

    pub fn kind<'a>(&'a self, ctx: &'a TypeDatabaseContext) -> &'a TypeKind {
        &self.get(ctx).kind
    }

    pub fn is_class(&self, ctx: &TypeDatabaseContext) -> bool {
        matches!(self.kind(ctx), TypeKind::Class(_))
    }

    pub fn is_trait(&self, ctx: &TypeDatabaseContext) -> bool {
        matches!(self.kind(ctx), TypeKind::Trait(_))
    }

    pub fn is_enum(&self, ctx: &TypeDatabaseContext) -> bool {
        matches!(self.kind(ctx), TypeKind::Enum(_))
    }

    pub fn is_void(&self, ctx: &TypeDatabaseContext) -> bool {
        matches!(self.kind(ctx), TypeKind::Void)
    }
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Type {
    pub kind: TypeKind,
    pub transport: TypeTransport,
    pub name: SymbolName,
    pub location: Location,
}

impl Type {
    pub fn alloc(ctx: &mut TypeDatabaseContext, name: SymbolName, kind: TypeKind, location: Location) -> TypeId {
        let id = TypeId(ctx.types.len() as u32);
        let method = Type {
            kind,
            transport: TypeTransport::Reference,
            name,
            location,
        };

        ctx.types.push(method);
        id
    }

    pub fn reference_type(name: SymbolName) -> Self {
        let location = Location::empty();

        Type {
            kind: TypeKind::Class(Box::new(Class::new(name.clone(), location.clone()))),
            transport: TypeTransport::Reference,
            name,
            location,
        }
    }

    pub fn value_type(name: SymbolName) -> Self {
        let location = Location::empty();

        Type {
            kind: TypeKind::Class(Box::new(Class::new(name.clone(), location.clone()))),
            transport: TypeTransport::Copy,
            name,
            location,
        }
    }
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct TypeRef {
    instance_of: TypeId,
    type_arguments: Vec<TypeId>,
}

impl TypeRef {
    pub fn new(instance_of: TypeId) -> Self {
        Self {
            instance_of,
            type_arguments: Vec::new(),
        }
    }

    pub fn unknown() -> Self {
        Self::new(UNKNOWN_TYPE_ID)
    }

    pub fn lower_from(ctx: &TypeDatabaseContext, ty: &hir::Type) -> Self {
        match ty {
            hir::Type::Scalar(t) => {
                let found_type = TypeId::find(ctx, t.name.clone());

                Self::new(found_type)
            }
            _ => todo!(),
        }
    }
}

#[derive(serde::Serialize, Debug)]
pub struct TypeDatabaseContext {
    pub types: Vec<Type>,
    pub properties: Vec<Property>,
    pub methods: Vec<Method>,
    pub functions: Vec<Function>,

    /// Defines a mapping between expressions and their resolved types.
    pub resolved_exprs: IndexMap<ExpressionId, TypeRef>,
}

impl TypeDatabaseContext {
    pub fn new() -> Self {
        Self {
            types: Vec::new(),
            properties: Vec::new(),
            methods: Vec::new(),
            functions: Vec::new(),
            resolved_exprs: IndexMap::new(),
        }
    }

    pub fn type_of_expr(&self, id: ExpressionId) -> &TypeRef {
        self.resolved_exprs.get(&id).unwrap()
    }
}

impl ThirBuildCtx {
    /// Attempt to infer the types of all expressions in the current module.
    ///
    /// The resolved types are stored in the `resolved_exprs` field of the `TypeDatabaseContext`.
    pub(super) fn infer_exprs(&mut self) -> Result<()> {
        for (id, expr) in &self.hir.expressions {
            let type_ref = self.type_of(expr.id)?;

            self.tcx.resolved_exprs.insert(*id, type_ref);
        }

        Ok(())
    }

    /// Returns the *type* of the expression with the given [`ExpressionId`].
    ///
    /// ### Panics
    ///
    /// This method will panic if no definition with the given ID exists
    /// within it's declared module. This also applies to any recursive calls this
    /// method makes, in the case of some expressions, such as assignments.
    pub(crate) fn type_of(&self, def: ExpressionId) -> Result<TypeRef> {
        let expr = self.hir_expr(def);

        match &expr.kind {
            hir::ExpressionKind::Assignment(e) => self.type_of(e.value.id),
            hir::ExpressionKind::Literal(e) => self.type_of_lit(&*e),
            hir::ExpressionKind::Variable(var) => {
                let decl = self.hir_expect_var_stmt(var.reference);

                Ok(self.type_of(decl.value.id)?)
            }
            k => todo!("unhandled node type in type_of(): {:?}", k),
        }
    }

    fn type_of_lit(&self, lit: &hir::Literal) -> Result<TypeRef> {
        let type_id = match &lit.kind {
            hir::LiteralKind::Int(k) => match &k.kind {
                hir::IntKind::I8 => TypeId::find(&self.tcx, SymbolName::i8()),
                hir::IntKind::U8 => TypeId::find(&self.tcx, SymbolName::u8()),
                hir::IntKind::I16 => TypeId::find(&self.tcx, SymbolName::i16()),
                hir::IntKind::U16 => TypeId::find(&self.tcx, SymbolName::u16()),
                hir::IntKind::I32 => TypeId::find(&self.tcx, SymbolName::i32()),
                hir::IntKind::U32 => TypeId::find(&self.tcx, SymbolName::u32()),
                hir::IntKind::I64 => TypeId::find(&self.tcx, SymbolName::i64()),
                hir::IntKind::U64 => TypeId::find(&self.tcx, SymbolName::u64()),
                hir::IntKind::IPtr => TypeId::find(&self.tcx, SymbolName::iptr()),
                hir::IntKind::UPtr => TypeId::find(&self.tcx, SymbolName::uptr()),
            },
            hir::LiteralKind::Float(k) => match &k.kind {
                hir::FloatKind::F32 => TypeId::find(&self.tcx, SymbolName::float()),
                hir::FloatKind::F64 => TypeId::find(&self.tcx, SymbolName::double()),
            },
            hir::LiteralKind::String(_) => TypeId::find(&self.tcx, SymbolName::string()),
            hir::LiteralKind::Boolean(_) => TypeId::find(&self.tcx, SymbolName::boolean()),
        };

        Ok(TypeRef::new(type_id))
    }
}
