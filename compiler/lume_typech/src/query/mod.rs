use crate::{ThirBuildCtx, *};
use error_snippet::Result;
use lume_hir::{self, FunctionId, Identifier, MethodId};
use lume_types::{Function, Method};

mod diagnostics;
pub(crate) mod lookup;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum CallReference {
    /// The call refers to a function.
    Function(FunctionId),

    /// The call refers to a method.
    Method(MethodId),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Callable<'a> {
    /// The call refers to a function.
    Function(&'a Function),

    /// The call refers to a method.
    Method(&'a Method),
}

impl Callable<'_> {
    pub fn name(&self) -> &SymbolName {
        match self {
            Self::Method(method) => &method.name,
            Self::Function(function) => &function.name,
        }
    }

    pub fn return_type(&self) -> &lume_types::TypeRef {
        match self {
            Self::Method(method) => &method.return_type,
            Self::Function(function) => &function.return_type,
        }
    }
}

impl From<Callable<'_>> for CallReference {
    fn from(value: Callable<'_>) -> Self {
        match value {
            Callable::Method(method) => Self::Method(method.id),
            Callable::Function(function) => Self::Function(function.id),
        }
    }
}

impl ThirBuildCtx {
    /// Returns the *type* of the expression with the given [`ExpressionId`].
    ///
    /// # Panics
    ///
    /// This method will panic if no definition with the given ID exists
    /// within it's declared module. This also applies to any recursive calls this
    /// method makes, in the case of some expressions, such as assignments.
    pub(crate) fn type_of(&self, hir: &lume_hir::map::Map, def: ExpressionId) -> Result<TypeRef> {
        // If the expression has been memorized, return it instead.
        if let Some(existing) = self.resolved_exprs.get(&def) {
            return Ok(existing.clone());
        }

        let expr = self.hir_expr(hir, def);

        match &expr.kind {
            lume_hir::ExpressionKind::Assignment(e) => self.type_of(hir, e.value.id),
            lume_hir::ExpressionKind::StaticCall(call) => {
                Ok(self.lookup_callable_static(hir, call)?.return_type().clone())
            }
            lume_hir::ExpressionKind::InstanceCall(call) => {
                let callable = self.lookup_callable_instance(hir, call)?;

                Ok(callable.return_type().clone())
            }
            lume_hir::ExpressionKind::Literal(e) => Ok(self.type_of_lit(e)),
            lume_hir::ExpressionKind::Member(expr) => {
                let callee_type = self.type_of(hir, expr.callee.id)?;

                let Some(property) = self.tcx().find_property(callee_type.instance_of, &expr.name) else {
                    let ty = self.tcx().type_(callee_type.instance_of).unwrap();

                    return Err(errors::MissingProperty {
                        source: expr.location.file.clone(),
                        range: expr.location.index.clone(),
                        type_name: ty.name.clone(),
                        property_name: Identifier {
                            name: expr.name.clone(),
                            location: expr.location.clone(),
                        },
                    }
                    .into());
                };

                Ok(property.property_type.clone())
            }
            lume_hir::ExpressionKind::Variable(var) => {
                let decl = self.hir_expect_var_stmt(hir, var.reference);

                Ok(self.type_of(hir, decl.value.id)?)
            }
        }
    }

    /// Attempts to get the type of a literal expression.
    fn type_of_lit(&self, lit: &lume_hir::Literal) -> TypeRef {
        let ty = match &lit.kind {
            lume_hir::LiteralKind::Int(k) => match &k.kind {
                lume_hir::IntKind::I8 => self.tcx().find_type(&SymbolName::i8()).unwrap(),
                lume_hir::IntKind::U8 => self.tcx().find_type(&SymbolName::u8()).unwrap(),
                lume_hir::IntKind::I16 => self.tcx().find_type(&SymbolName::i16()).unwrap(),
                lume_hir::IntKind::U16 => self.tcx().find_type(&SymbolName::u16()).unwrap(),
                lume_hir::IntKind::I32 => self.tcx().find_type(&SymbolName::i32()).unwrap(),
                lume_hir::IntKind::U32 => self.tcx().find_type(&SymbolName::u32()).unwrap(),
                lume_hir::IntKind::I64 => self.tcx().find_type(&SymbolName::i64()).unwrap(),
                lume_hir::IntKind::U64 => self.tcx().find_type(&SymbolName::u64()).unwrap(),
                lume_hir::IntKind::IPtr => self.tcx().find_type(&SymbolName::iptr()).unwrap(),
                lume_hir::IntKind::UPtr => self.tcx().find_type(&SymbolName::uptr()).unwrap(),
            },
            lume_hir::LiteralKind::Float(k) => match &k.kind {
                lume_hir::FloatKind::F32 => self.tcx().find_type(&SymbolName::float()).unwrap(),
                lume_hir::FloatKind::F64 => self.tcx().find_type(&SymbolName::double()).unwrap(),
            },
            lume_hir::LiteralKind::String(_) => self.tcx().find_type(&SymbolName::string()).unwrap(),
            lume_hir::LiteralKind::Boolean(_) => self.tcx().find_type(&SymbolName::boolean()).unwrap(),
        };

        TypeRef::new(ty.id)
    }

    /// Returns the fully-qualified [`SymbolName`] of the given [`TypeRef`].
    pub(crate) fn type_ref_name(&self, type_ref: &TypeRef) -> Result<&SymbolName> {
        Ok(&self.tcx.ty_expect(type_ref.instance_of)?.name)
    }
}
