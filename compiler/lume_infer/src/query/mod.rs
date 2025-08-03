use crate::{TyInferCtx, *};
use error_snippet::Result;
use lume_hir::{self, FunctionId, Identifier, MethodId, Node, Path, UseId};
use lume_query::cached_query;
use lume_span::ExpressionId;
use lume_types::{Function, Method, Trait, TypeRef};

mod diagnostics;
pub mod hir;
pub mod lookup;

#[derive(Debug, Hash, Copy, Clone, PartialEq, Eq)]
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

    pub fn signature(&'_ self) -> lume_types::FunctionSig<'_> {
        match self {
            Self::Method(method) => method.sig(),
            Self::Function(function) => function.sig(),
        }
    }

    pub fn return_type(&self) -> &lume_types::TypeRef {
        match self {
            Self::Method(method) => &method.return_type,
            Self::Function(function) => &function.return_type,
        }
    }

    pub fn to_call_reference(self) -> CallReference {
        self.into()
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
        let ty = match &expr.kind {
            lume_hir::ExpressionKind::Assignment(e) => self.type_of(e.value.id)?,
            lume_hir::ExpressionKind::Cast(e) => self.mk_type_ref(&e.target)?,
            lume_hir::ExpressionKind::Construct(e) => {
                let Some(ty_opt) = self.find_type_ref(&e.path)? else {
                    return Err(self.missing_type_err(&lume_hir::Type {
                        id: lume_span::ItemId::empty(),
                        name: e.path.clone(),
                        location: e.path.location,
                    }));
                };

                let type_parameters = self.hir_avail_type_params_expr(e.id);
                let type_args = self.mk_type_refs_from(e.path.type_arguments(), DefId::Expression(e.id))?;

                let type_parameters_id: Vec<lume_hir::TypeParameterId> =
                    type_parameters.iter().map(|p| p.type_param_id.unwrap()).collect();

                let instantiated = self.instantiate_type_from(&ty_opt, &type_parameters_id, &type_args);

                instantiated.clone()
            }
            lume_hir::ExpressionKind::Binary(expr) => self.type_of_expr(&expr.lhs)?,
            lume_hir::ExpressionKind::StaticCall(call) => self.type_of_call(lume_hir::CallExpression::Static(call))?,
            lume_hir::ExpressionKind::InstanceCall(call) => {
                self.type_of_call(lume_hir::CallExpression::Instanced(call))?
            }
            lume_hir::ExpressionKind::IntrinsicCall(call) => {
                self.type_of_call(lume_hir::CallExpression::Intrinsic(call))?
            }
            lume_hir::ExpressionKind::Literal(e) => self.type_of_lit(e),
            lume_hir::ExpressionKind::Logical(expr) => self.type_of_expr(&expr.lhs)?,
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

                property.property_type.clone()
            }
            lume_hir::ExpressionKind::Switch(switch) => match switch.cases.first() {
                Some(case) => self.type_of_expr(&case.branch)?,
                None => TypeRef::void(),
            },
            lume_hir::ExpressionKind::Variable(var) => match &var.reference {
                lume_hir::VariableSource::Parameter(param) => {
                    self.mk_type_ref_from(&param.param_type, DefId::Expression(var.id))?
                }
                lume_hir::VariableSource::Variable(var) => self.type_of_vardecl(var)?,
            },
            lume_hir::ExpressionKind::Variant(var) => {
                let enum_segment = var.name.clone().parent().unwrap();
                let enum_ty = self.find_type_ref(&enum_segment)?;

                enum_ty.ok_or_else(|| {
                    self.missing_type_err(&lume_hir::Type {
                        id: lume_span::ItemId::empty(),
                        name: enum_segment.clone(),
                        location: enum_segment.location,
                    })
                })?
            }
        };

        Result::Ok(ty.with_location(expr.location))
    }

    /// Attempts to get the type of a literal expression.
    #[cached_query]
    fn type_of_lit(&self, lit: &lume_hir::Literal) -> TypeRef {
        let ty = match &lit.kind {
            lume_hir::LiteralKind::Int(k) => match &k.kind {
                lume_hir::IntKind::I8 => TypeRef::i8(),
                lume_hir::IntKind::U8 => TypeRef::u8(),
                lume_hir::IntKind::I16 => TypeRef::i16(),
                lume_hir::IntKind::U16 => TypeRef::u16(),
                lume_hir::IntKind::I32 => TypeRef::i32(),
                lume_hir::IntKind::U32 => TypeRef::u32(),
                lume_hir::IntKind::I64 => TypeRef::i64(),
                lume_hir::IntKind::U64 => TypeRef::u64(),
            },
            lume_hir::LiteralKind::Float(k) => match &k.kind {
                lume_hir::FloatKind::F32 => TypeRef::f32(),
                lume_hir::FloatKind::F64 => TypeRef::f64(),
            },
            lume_hir::LiteralKind::String(_) => TypeRef::string(),
            lume_hir::LiteralKind::Boolean(_) => TypeRef::bool(),
        };

        ty.with_location(lit.location)
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
            lume_hir::StatementKind::If(cond) => self.type_of_if_conditional(cond),
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
        if let Some(declared_type) = &stmt.declared_type {
            self.mk_type_ref_from(declared_type, DefId::Statement(stmt.id))
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

    /// Returns the return type of the given [`lume_hir::CallExpression`].
    #[cached_query(result)]
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub fn type_of_call(&self, expr: lume_hir::CallExpression) -> Result<TypeRef> {
        let callable = self.probe_callable(expr)?;
        let signature = self.signature_of_instantiated(callable, expr)?;

        Result::Ok(signature.ret_ty)
    }

    /// Returns the *type* of the given [`Pattern`].
    ///
    /// # Panics
    ///
    /// This method will panic if no definition with the given ID exists
    /// within it's declared module. This also applies to any recursive calls this
    /// method makes, in the case of some expressions, such as assignments.
    #[cached_query(result)]
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub fn type_of_pattern(&self, pat: &lume_hir::Pattern) -> Result<Option<TypeRef>> {
        let ty = match pat {
            lume_hir::Pattern::Literal(lit) => self.type_of_lit(lit),
            lume_hir::Pattern::Variant(var) => {
                let enum_name = var.name.clone().parent().unwrap();

                match self.tdb().find_type(&enum_name) {
                    Some(ty) => TypeRef::new(ty.id, pat.location()),
                    None => {
                        return Err(self.missing_type_err(&lume_hir::Type {
                            id: lume_span::ItemId::empty(),
                            name: var.name.clone(),
                            location: var.name.location,
                        }));
                    }
                }
            }
            lume_hir::Pattern::Identifier(_) | lume_hir::Pattern::Wildcard(_) => return Ok(None),
        };

        Result::Ok(Some(ty.with_location(pat.location())))
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

    /// Returns the enum definition with the given type ID.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub fn enum_def_type(&self, ty: TypeId) -> Result<&lume_hir::EnumDefinition> {
        let Some(parent_ty) = self.tdb().type_(ty) else {
            return Err(lume_types::errors::TypeNotFound { id: ty }.into());
        };

        let lume_types::TypeKind::User(lume_types::UserType::Enum(enum_ty)) = &parent_ty.kind else {
            return Err(lume_types::errors::UnexpectedTypeKind {
                found: parent_ty.kind.as_kind_ref(),
                expected: lume_types::TypeKindRef::Trait,
            }
            .into());
        };

        Ok(self.hir_expect_enum(enum_ty.id))
    }

    /// Returns the enum definition with the given name.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub fn enum_def_of_name(&self, name: &Path) -> Result<&lume_hir::EnumDefinition> {
        let Some(parent_ty) = self.tdb().find_type(name) else {
            return Err(lume_types::errors::TypeNameNotFound { name: name.clone() }.into());
        };

        let lume_types::TypeKind::User(lume_types::UserType::Enum(enum_ty)) = &parent_ty.kind else {
            return Err(lume_types::errors::UnexpectedTypeKind {
                found: parent_ty.kind.as_kind_ref(),
                expected: lume_types::TypeKindRef::Trait,
            }
            .into());
        };

        Ok(self.hir_expect_enum(enum_ty.id))
    }

    /// Returns the enum case definitions on the enum type with the given ID.
    #[cached_query(result)]
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub fn enum_cases_of(&self, ty: TypeId) -> Result<&[lume_hir::EnumDefinitionCase]> {
        Result::Ok(&self.enum_def_type(ty)?.cases)
    }

    /// Returns the enum case definitions, which is being referred to by the given expression.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub fn enum_cases_expr(&self, id: ExpressionId) -> Result<&[lume_hir::EnumDefinitionCase]> {
        self.enum_cases_of(self.type_of(id)?.instance_of)
    }

    /// Returns the enum case definitions on the enum type with the given name.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub fn enum_cases_of_name(&self, name: &Path) -> Result<&[lume_hir::EnumDefinitionCase]> {
        let Some(parent_ty_ref) = self.find_type_ref(name)? else {
            return Err(lume_types::errors::TypeNameNotFound { name: name.clone() }.into());
        };

        self.enum_cases_of(parent_ty_ref.instance_of)
    }

    /// Returns the enum case definition, which is being referred to by the given expression.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub fn enum_case_expr(&self, id: ExpressionId) -> Result<&lume_hir::EnumDefinitionCase> {
        let expr = self.hir_expect_expr(id);

        if let lume_hir::ExpressionKind::Variant(var) = &expr.kind {
            self.enum_case_with_name(&var.name)
        } else {
            panic!("bug!: attempted to find enum variant of non-variant expression")
        }
    }

    /// Returns the enum case definition, which uses the given variant path.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub fn enum_case_with_name(&self, variant: &Path) -> Result<&lume_hir::EnumDefinitionCase> {
        let Some(parent_ty_path) = variant.clone().parent() else {
            return Err(lume_types::errors::PathWithoutParent { path: variant.clone() }.into());
        };

        for case in self.enum_cases_of_name(&parent_ty_path)? {
            if case.name.name() == variant.name() {
                return Ok(case);
            }
        }

        Err(diagnostics::MissingVariant {
            source: variant.location,
            type_name: parent_ty_path,
            name: variant.clone(),
        }
        .into())
    }

    /// Returns the field of the constructor expression, matching the given property.
    #[cached_query]
    #[tracing::instrument(level = "TRACE", skip(self), ret)]
    pub fn constructer_field_of(&self, expr: &lume_hir::Construct, prop_name: &String) -> Option<lume_hir::Field> {
        if let Some(field) = expr.fields.iter().find(|field| field.name.as_str() == prop_name) {
            return Some(field.clone());
        }

        self.constructer_default_field_of(expr, prop_name)
    }

    /// Returns the field of the constructor expression, matching the given property.
    #[cached_query]
    #[tracing::instrument(level = "TRACE", skip(self), ret)]
    pub fn constructer_default_field_of(
        &self,
        expr: &lume_hir::Construct,
        prop_name: &String,
    ) -> Option<lume_hir::Field> {
        let construct_type = self.find_type_ref(&expr.path).ok()??;
        let struct_type = self.tdb().ty_expect_struct(construct_type.instance_of).ok()?;
        let struct_hir_type = self.hir_expect_struct(struct_type.id);

        let matching_property = struct_hir_type
            .properties()
            .find(|prop| prop.name.as_str() == prop_name)?;

        if let Some(default_value) = &matching_property.default_value {
            return Some(lume_hir::Field {
                name: lume_hir::Identifier::from(prop_name),
                value: default_value.clone(),
                location: lume_span::Location::empty(),
            });
        }

        None
    }
}
