use crate::{TyInferCtx, *};
use error_snippet::Result;
use lume_hir::{self, FunctionId, Identifier, MethodId, Path, UseId};
use lume_query::cached_query;
use lume_span::ExpressionId;
use lume_types::{Function, Method, Trait, TypeRef};

mod diagnostics;
pub mod hir;
pub mod lookup;

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
    pub fn name(&self) -> &Path {
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

impl TyInferCtx {
    /// Returns the *type* of the expression with the given [`ExpressionId`].
    ///
    /// # Panics
    ///
    /// This method will panic if no definition with the given ID exists
    /// within it's declared module. This also applies to any recursive calls this
    /// method makes, in the case of some expressions, such as assignments.
    #[cached_query(result)]
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub fn type_of(&self, def: ExpressionId) -> Result<TypeRef> {
        self.type_of_expr(self.hir_expect_expr(def))
    }

    /// Returns the *type* of the given [`Expression`].
    ///
    /// # Panics
    ///
    /// This method will panic if no definition with the given ID exists
    /// within it's declared module. This also applies to any recursive calls this
    /// method makes, in the case of some expressions, such as assignments.
    #[cached_query(result)]
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub fn type_of_expr(&self, expr: &lume_hir::Expression) -> Result<TypeRef> {
        match &expr.kind {
            lume_hir::ExpressionKind::Assignment(e) => self.type_of(e.value.id),
            lume_hir::ExpressionKind::Cast(e) => self.mk_type_ref(&e.target),
            lume_hir::ExpressionKind::Construct(e) => {
                let ty_opt = self.find_type_ref(&e.path)?;

                ty_opt.ok_or_else(|| {
                    self.missing_type_err(&lume_hir::Type {
                        id: lume_span::ItemId::empty(),
                        name: e.path.clone(),
                        location: e.path.location,
                    })
                })
            }
            lume_hir::ExpressionKind::Binary(expr) => self.type_of_expr(&expr.lhs),
            lume_hir::ExpressionKind::StaticCall(call) => Ok(self.probe_callable_static(call)?.return_type().clone()),
            lume_hir::ExpressionKind::InstanceCall(call) => {
                let callable = self.probe_callable_instance(call)?;

                Ok(callable.return_type().clone())
            }
            lume_hir::ExpressionKind::Literal(e) => Ok(self.type_of_lit(e)),
            lume_hir::ExpressionKind::Logical(expr) => self.type_of_expr(&expr.lhs),
            lume_hir::ExpressionKind::Member(expr) => {
                let callee_type = self.type_of(expr.callee.id)?;

                let Some(property) = self.tdb().find_property(callee_type.instance_of, &expr.name) else {
                    let ty = self.tdb().type_(callee_type.instance_of).unwrap();

                    return Err(crate::errors::MissingProperty {
                        source: expr.location.file.clone(),
                        range: expr.location.index.clone(),
                        type_name: ty.name.clone(),
                        property_name: Identifier {
                            name: expr.name.clone(),
                            location: expr.location,
                        },
                    }
                    .into());
                };

                Ok(property.property_type.clone())
            }
            lume_hir::ExpressionKind::Variable(var) => match &var.reference {
                lume_hir::VariableSource::Parameter(param) => self.mk_type_ref(&param.param_type),
                lume_hir::VariableSource::Variable(var) => self.type_of_vardecl(var),
            },
            lume_hir::ExpressionKind::Void => Ok(TypeRef::void()),
        }
    }

    /// Attempts to get the type of a literal expression.
    #[cached_query]
    fn type_of_lit(&self, lit: &lume_hir::Literal) -> TypeRef {
        let ty = match &lit.kind {
            lume_hir::LiteralKind::Int(k) => match &k.kind {
                lume_hir::IntKind::I8 => self.tdb().find_type(&Path::i8()).unwrap(),
                lume_hir::IntKind::U8 => self.tdb().find_type(&Path::u8()).unwrap(),
                lume_hir::IntKind::I16 => self.tdb().find_type(&Path::i16()).unwrap(),
                lume_hir::IntKind::U16 => self.tdb().find_type(&Path::u16()).unwrap(),
                lume_hir::IntKind::I32 => self.tdb().find_type(&Path::i32()).unwrap(),
                lume_hir::IntKind::U32 => self.tdb().find_type(&Path::u32()).unwrap(),
                lume_hir::IntKind::I64 => self.tdb().find_type(&Path::i64()).unwrap(),
                lume_hir::IntKind::U64 => self.tdb().find_type(&Path::u64()).unwrap(),
            },
            lume_hir::LiteralKind::Float(k) => match &k.kind {
                lume_hir::FloatKind::F32 => self.tdb().find_type(&Path::float()).unwrap(),
                lume_hir::FloatKind::F64 => self.tdb().find_type(&Path::double()).unwrap(),
            },
            lume_hir::LiteralKind::String(_) => self.tdb().find_type(&Path::string()).unwrap(),
            lume_hir::LiteralKind::Boolean(_) => self.tdb().find_type(&Path::boolean()).unwrap(),
        };

        TypeRef::new(ty.id, lit.location)
    }

    /// Returns the *type* of the given [`lume_hir::Statement`].
    ///
    /// # Panics
    ///
    /// This method will panic if no definition with the given ID exists
    /// within it's declared module. This also applies to any recursive calls this
    /// method makes, in the case of some expressions, such as assignments.
    #[cached_query(result)]
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub fn type_of_stmt(&self, stmt: &lume_hir::Statement) -> Result<TypeRef> {
        match &stmt.kind {
            lume_hir::StatementKind::IteratorLoop(l) => self.type_of_block(&l.block),
            lume_hir::StatementKind::InfiniteLoop(l) => self.type_of_block(&l.block),
            lume_hir::StatementKind::PredicateLoop(l) => self.type_of_block(&l.block),
            lume_hir::StatementKind::If(cond) => self.type_of_if_conditional(cond),
            lume_hir::StatementKind::Unless(cond) => self.type_of_unless_conditional(cond),
            lume_hir::StatementKind::Return(ret) => self.type_of_return(ret),
            _ => Ok(TypeRef::void()),
        }
    }

    /// Returns the *type* of the given [`lume_hir::If`] statement.
    #[cached_query(result)]
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub fn type_of_if_conditional(&self, cond: &lume_hir::If) -> Result<TypeRef> {
        let primary_case = cond.cases.first().unwrap();

        self.type_of_condition(primary_case)
    }

    /// Returns the *type* of the given [`lume_hir::Unless`] statement.
    #[cached_query(result)]
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub fn type_of_unless_conditional(&self, cond: &lume_hir::Unless) -> Result<TypeRef> {
        let primary_case = cond.cases.first().unwrap();

        self.type_of_condition(primary_case)
    }

    /// Returns the *type* of the given [`lume_hir::Condition`].
    #[cached_query(result)]
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub fn type_of_condition(&self, cond: &lume_hir::Condition) -> Result<TypeRef> {
        self.type_of_block(&cond.block)
    }

    /// Returns the *type* of the given [`lume_hir::VariableDeclaration`].
    #[cached_query(result)]
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub fn type_of_vardecl(&self, stmt: &lume_hir::VariableDeclaration) -> Result<TypeRef> {
        let type_params = self.hir_avail_type_params(DefId::Statement(stmt.id));

        if let Some(declared_type) = &stmt.declared_type {
            self.mk_type_ref_generic(declared_type, &type_params)
        } else {
            self.type_of_expr(&stmt.value)
        }
    }

    /// Returns the *type* of the given [`lume_hir::Return`].
    #[cached_query(result)]
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub fn type_of_return(&self, stmt: &lume_hir::Return) -> Result<TypeRef> {
        if let Some(expr) = &stmt.value {
            self.type_of_expr(expr)
        } else {
            Ok(TypeRef::void())
        }
    }

    /// Returns the *type* of the given [`lume_hir::Block`]. The type of the block
    /// is determined by the inferred type of the last expression in the block, or [`TypeRef::void()`]
    /// if no return statement is present.
    ///
    /// # Panics
    ///
    /// This method will panic if no definition with the given ID exists
    /// within it's declared module. This also applies to any recursive calls this
    /// method makes, in the case of some expressions, such as assignments.
    #[cached_query(result)]
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub fn type_of_block(&self, block: &lume_hir::Block) -> Result<TypeRef> {
        let Some(last_statement) = block.statements.last() else {
            return Ok(TypeRef::void());
        };

        self.type_of_stmt(last_statement)
    }

    /// Returns the fully-qualified [`Path`] of the given [`TypeRef`].
    #[tracing::instrument(level = "TRACE", skip(self), err, ret(Display))]
    pub fn type_ref_name(&self, type_ref: &TypeRef) -> Result<&Path> {
        Ok(&self.tdb().ty_expect(type_ref.instance_of)?.name)
    }

    /// Returns the [`Trait`] definition, which matches the [`Use`] declaration with the given ID.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub fn trait_def_of(&self, use_id: UseId) -> Result<&Trait> {
        let Some(use_ref) = self.tdb().use_(use_id) else {
            return Err(lume_types::errors::UseNotFound { id: use_id }.into());
        };

        self.tdb().ty_expect_trait(use_ref.trait_.instance_of)
    }
}
