use error_snippet::Result;
use lume_architect::cached_query;
use lume_hir::{CallExpression, Node, Path, Visibility};
use lume_span::NodeId;
use lume_types::{Function, Method, TypeRef};

use crate::{TyInferCtx, *};

pub mod callable;
mod diagnostics;
pub mod hir;
pub mod lookup;
pub mod traits;
pub mod ty;

/// Opaque reference to a callable node, such as a function, method definition
/// or method implementation.
///
/// This structure only contains the *ID* of the referenced callable - for a
/// data type which references the callable with lifetime references, see
/// [`Callable`].
#[derive(Debug, Hash, Copy, Clone, PartialEq, Eq)]
pub enum CallReference {
    /// The call refers to a function.
    Function(NodeId),

    /// The call refers to a method.
    Method(NodeId),
}

/// Opaque reference to a callable node, such as a function, method definition
/// or method implementation.
///
/// This structure contains a lifetime reference to the referenced callable -
/// for a data type which only references the callable with it's [`NodeId`], see
/// [`CallReference`].
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Callable<'a> {
    /// The call refers to a function.
    Function(&'a Function),

    /// The call refers to a method.
    Method(&'a Method),
}

impl Callable<'_> {
    #[inline]
    pub fn id(&self) -> NodeId {
        match self {
            Self::Method(method) => method.id,
            Self::Function(function) => function.id,
        }
    }

    #[inline]
    pub fn name(&self) -> &Path {
        match self {
            Self::Method(method) => &method.name,
            Self::Function(function) => &function.name,
        }
    }

    #[inline]
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
    /// Returns the *type* of the expression with the given [`NodeId`].
    ///
    /// # Panics
    ///
    /// This method will panic if no definition with the given ID exists
    /// within it's declared module. This also applies to any recursive calls
    /// this method makes, in the case of some expressions, such as
    /// assignments.
    #[cached_query(result)]
    #[libftrace::traced(level = Trace, err)]
    pub fn type_of(&self, def: NodeId) -> Result<TypeRef> {
        self.type_of_expr(self.hir_expect_expr(def))
    }

    /// Returns the *type* of the given [`lume_hir::Expression`].
    ///
    /// # Panics
    ///
    /// This method will panic if no definition with the given ID exists
    /// within it's declared module. This also applies to any recursive calls
    /// this method makes, in the case of some expressions, such as
    /// assignments.
    #[cached_query(result)]
    #[libftrace::traced(level = Trace, err)]
    pub fn type_of_expr(&self, expr: &lume_hir::Expression) -> Result<TypeRef> {
        let ty = match &expr.kind {
            lume_hir::ExpressionKind::Assignment(e) => {
                if !self.can_assign_value(e.target) {
                    return Err(diagnostics::CannotAssignExpression {
                        source: self.hir_span_of_node(e.target),
                    }
                    .into());
                }

                TypeRef::void().with_location(e.location)
            }
            lume_hir::ExpressionKind::Cast(e) => self.mk_type_ref(&e.target)?,
            lume_hir::ExpressionKind::Construct(e) => {
                let Some(ty_opt) = self.find_type_ref_from(&e.path, e.id)? else {
                    return Err(self.missing_type_err(&lume_hir::Type {
                        id: lume_hir::TypeId::from(lume_span::NodeId::empty(expr.id.package)),
                        name: e.path.clone(),
                        location: e.path.location,
                    }));
                };

                let type_parameters = self.available_type_params_at(e.id);

                let type_args = self.mk_type_refs_from(e.path.bound_types(), e.id)?;
                let instantiated = self.instantiate_type_from(&ty_opt, &type_parameters, &type_args);

                instantiated.clone()
            }
            lume_hir::ExpressionKind::StaticCall(call) => self.type_of_call(lume_hir::CallExpression::Static(call))?,
            lume_hir::ExpressionKind::InstanceCall(call) => {
                self.type_of_call(lume_hir::CallExpression::Instanced(call))?
            }
            lume_hir::ExpressionKind::IntrinsicCall(call) => {
                self.type_of_call(lume_hir::CallExpression::Intrinsic(call))?
            }
            lume_hir::ExpressionKind::If(cond) => self.type_of_if_conditional(cond)?,
            lume_hir::ExpressionKind::Is(_) => TypeRef::bool().with_location(expr.location),
            lume_hir::ExpressionKind::Literal(e) => self.type_of_lit(e)?,
            lume_hir::ExpressionKind::Member(expr) => {
                let callee_type = self.type_of(expr.callee)?;

                let Some(field) = self.field_on(callee_type.instance_of, &expr.name.name)? else {
                    let ty = self.tdb().type_(callee_type.instance_of).unwrap();

                    return Err(crate::errors::MissingField {
                        source: expr.location.file.clone(),
                        range: expr.location.index.clone(),
                        type_name: ty.name.clone(),
                        field_name: expr.name.clone(),
                    }
                    .into());
                };

                let type_params = self.available_type_params_at(callee_type.instance_of);
                let field_type = self.mk_type_ref_from(&field.field_type, callee_type.instance_of)?;

                self.instantiate_type_from(&field_type, &type_params, &callee_type.bound_types)
            }
            lume_hir::ExpressionKind::Scope(scope) => self.type_of_scope(scope)?,
            lume_hir::ExpressionKind::Switch(switch) => match switch.cases.first() {
                Some(case) => self.type_of(case.branch)?,
                None => TypeRef::void().with_location(switch.location),
            },
            lume_hir::ExpressionKind::Variable(var) => match &var.reference {
                lume_hir::VariableSource::Parameter(param) => self.mk_type_ref_from(&param.param_type, var.id)?,
                lume_hir::VariableSource::Variable(var) => self.type_of_vardecl(var)?,
                lume_hir::VariableSource::Pattern(pat) => self.type_of_pattern(pat)?,
            },
            lume_hir::ExpressionKind::Variant(var) => {
                let enum_segment = var.name.clone().parent().unwrap();
                let enum_ty = self.find_type_ref_from(&enum_segment, expr.id)?;

                enum_ty.ok_or_else(|| {
                    self.missing_type_err(&lume_hir::Type {
                        id: lume_hir::TypeId::from(lume_span::NodeId::empty(expr.id.package)),
                        name: enum_segment.clone(),
                        location: enum_segment.location,
                    })
                })?
            }
        };

        Result::Ok(ty.with_location(expr.location))
    }

    /// Attempts to get the type of a literal expression.
    #[cached_query(result)]
    fn type_of_lit(&self, lit: &lume_hir::Literal) -> Result<TypeRef> {
        let ty = match &lit.kind {
            lume_hir::LiteralKind::Int(k) => {
                let kind = if let Some(k) = k.kind { k } else { self.kind_of_int(k)? };

                match kind {
                    lume_hir::IntKind::I8 => TypeRef::i8(),
                    lume_hir::IntKind::U8 => TypeRef::u8(),
                    lume_hir::IntKind::I16 => TypeRef::i16(),
                    lume_hir::IntKind::U16 => TypeRef::u16(),
                    lume_hir::IntKind::I32 => TypeRef::i32(),
                    lume_hir::IntKind::U32 => TypeRef::u32(),
                    lume_hir::IntKind::I64 => TypeRef::i64(),
                    lume_hir::IntKind::U64 => TypeRef::u64(),
                }
            }
            lume_hir::LiteralKind::Float(k) => {
                let kind = if let Some(k) = k.kind {
                    k
                } else {
                    self.kind_of_float(k)?
                };

                match kind {
                    lume_hir::FloatKind::F32 => TypeRef::f32(),
                    lume_hir::FloatKind::F64 => TypeRef::f64(),
                }
            }
            lume_hir::LiteralKind::String(_) => self.std_ref_string(),
            lume_hir::LiteralKind::Boolean(_) => TypeRef::bool(),
        };

        Ok(ty.with_location(lit.location))
    }

    /// Attempts to get the kind of an integer literal expression.
    #[cached_query(result)]
    pub fn kind_of_int(&self, lit: &lume_hir::IntLiteral) -> Result<lume_hir::IntKind> {
        // Prevents stack-overflows when attempting to infer the literal kind in some
        // situations. If we've already attempted to lock, we're in a loop and
        // should return the default.
        //
        // TODO: This is a hacky workaround, until we get around to adding
        //       an effective cycle-handling implementation in `architect.`
        let Ok(_guard) = self.nested_inference_lock.try_write() else {
            return Ok(lume_hir::IntKind::I32);
        };

        let Some(guessed_type) = self.expected_type_of(lit.id)? else {
            return Ok(lume_hir::IntKind::I32);
        };

        match guessed_type {
            t if t.is_i8() => Ok(lume_hir::IntKind::I8),
            t if t.is_i16() => Ok(lume_hir::IntKind::I16),
            t if t.is_i32() => Ok(lume_hir::IntKind::I32),
            t if t.is_i64() => Ok(lume_hir::IntKind::I64),
            t if t.is_u8() => Ok(lume_hir::IntKind::U8),
            t if t.is_u16() => Ok(lume_hir::IntKind::U16),
            t if t.is_u32() => Ok(lume_hir::IntKind::U32),
            t if t.is_u64() => Ok(lume_hir::IntKind::U64),
            _ => Ok(lume_hir::IntKind::I32),
        }
    }

    /// Attempts to get the kind of an floating-point literal expression.
    #[cached_query(result)]
    pub fn kind_of_float(&self, lit: &lume_hir::FloatLiteral) -> Result<lume_hir::FloatKind> {
        // Prevents stack-overflows when attempting to infer the literal kind in some
        // situations. If we've already attempted to lock, we're in a loop and
        // should return the default.
        //
        // TODO: This is a hacky workaround, until we get around to adding
        //       an effective cycle-handling implementation in `architect.`
        let Ok(_guard) = self.nested_inference_lock.try_write() else {
            return Ok(lume_hir::FloatKind::F64);
        };

        let Some(guessed_type) = self.expected_type_of(lit.id)? else {
            return Ok(lume_hir::FloatKind::F64);
        };

        match guessed_type {
            t if t.is_f32() => Ok(lume_hir::FloatKind::F32),
            t if t.is_f64() => Ok(lume_hir::FloatKind::F64),
            _ => Ok(lume_hir::FloatKind::F64),
        }
    }

    /// Returns the return type of the given [`lume_hir::Scope`].
    #[cached_query(result)]
    #[libftrace::traced(level = Trace, err)]
    pub fn type_of_scope(&self, scope: &lume_hir::Scope) -> Result<TypeRef> {
        self.type_of_body(&scope.body, scope.location)
    }

    /// Returns the return type of the given body of nodes.
    #[cached_query(result)]
    #[libftrace::traced(level = Trace, err)]
    pub fn type_of_body(&self, body: &[NodeId], loc: Location) -> Result<TypeRef> {
        if let Some(stmt) = body.last() {
            let stmt = self.hir_expect_stmt(*stmt);

            match &stmt.kind {
                lume_hir::StatementKind::Final(fin) => return self.type_of(fin.value),
                lume_hir::StatementKind::Break(_) | lume_hir::StatementKind::Continue(_) => {
                    return Ok(self
                        .never_type()
                        .expect("expected `Never` type to exist")
                        .with_location(stmt.location));
                }
                _ => {}
            }
        }

        Ok(TypeRef::void().with_location(loc))
    }

    /// Returns the *type* of the given [`lume_hir::Statement`].
    ///
    /// # Panics
    ///
    /// This method will panic if no definition with the given ID exists
    /// within it's declared module. This also applies to any recursive calls
    /// this method makes, in the case of some expressions, such as
    /// assignments.
    #[cached_query(result)]
    #[libftrace::traced(level = Trace, err)]
    pub fn type_of_stmt(&self, stmt: &lume_hir::Statement) -> Result<TypeRef> {
        match &stmt.kind {
            lume_hir::StatementKind::Final(fin) => self.type_of(fin.value),
            lume_hir::StatementKind::IteratorLoop(l) => self.type_of_block(&l.block),
            lume_hir::StatementKind::InfiniteLoop(l) => self.type_of_block(&l.block),
            lume_hir::StatementKind::Return(ret) => self.type_of_return(ret),
            _ => Ok(TypeRef::void().with_location(stmt.location)),
        }
    }

    /// Returns the *type* of the given [`lume_hir::If`] statement.
    #[cached_query(result)]
    #[libftrace::traced(level = Trace, err)]
    pub fn type_of_if_conditional(&self, cond: &lume_hir::If) -> Result<TypeRef> {
        let primary_case = cond.cases.first().unwrap();

        self.type_of_condition(primary_case)
    }

    /// Returns the *type* of the given [`lume_hir::Condition`].
    #[cached_query(result)]
    #[libftrace::traced(level = Trace, err)]
    pub fn type_of_condition(&self, cond: &lume_hir::Condition) -> Result<TypeRef> {
        self.type_of_block(&cond.block)
    }

    /// Returns the returned type of the given [`lume_hir::Condition`].
    #[cached_query(result)]
    #[libftrace::traced(level = Trace, err)]
    pub fn type_of_condition_scope(&self, cond: &lume_hir::Condition) -> Result<TypeRef> {
        self.type_of_body(&cond.block.statements, cond.block.location)
    }

    /// Returns the *type* of the given [`lume_hir::VariableDeclaration`].
    #[cached_query(result)]
    #[libftrace::traced(level = Trace, err)]
    pub fn type_of_vardecl(&self, stmt: &lume_hir::VariableDeclaration) -> Result<TypeRef> {
        if let Some(declared_type) = &stmt.declared_type {
            self.mk_type_ref_from(declared_type, stmt.id)
        } else {
            self.type_of(stmt.value)
        }
    }

    /// Returns the *type* of the given [`lume_hir::Return`].
    #[cached_query(result)]
    #[libftrace::traced(level = Trace, err)]
    pub fn type_of_return(&self, stmt: &lume_hir::Return) -> Result<TypeRef> {
        if let Some(expr) = stmt.value {
            self.type_of(expr)
        } else {
            Ok(TypeRef::void().with_location(stmt.location))
        }
    }

    /// Returns the *type* of the given [`lume_hir::Block`]. The type of the
    /// block is determined by the inferred type of the last expression in
    /// the block, or [`TypeRef::void()`] if no return statement is present.
    ///
    /// # Panics
    ///
    /// This method will panic if no definition with the given ID exists
    /// within it's declared module. This also applies to any recursive calls
    /// this method makes, in the case of some expressions, such as
    /// assignments.
    #[cached_query(result)]
    #[libftrace::traced(level = Trace, err)]
    pub fn type_of_block(&self, block: &lume_hir::Block) -> Result<TypeRef> {
        let Some(last_statement) = block.statements.last() else {
            return Ok(TypeRef::void().with_location(block.location));
        };

        let stmt = self.hir_expect_stmt(*last_statement);
        self.type_of_stmt(stmt)
    }

    /// Returns the return type of the given [`lume_hir::CallExpression`].
    #[cached_query(result)]
    #[libftrace::traced(level = Trace, fields(name = expr.name()), err, ret)]
    pub fn type_of_call(&self, expr: lume_hir::CallExpression) -> Result<TypeRef> {
        let callable = self.probe_callable(expr)?;
        let signature = self.instantiated_signature_of(callable, expr)?;

        Ok(signature.ret_ty)
    }

    /// Returns the *type* of the given [`lume_hir::Pattern`].
    ///
    /// # Panics
    ///
    /// This method will panic if no definition with the given ID exists
    /// within it's declared module. This also applies to any recursive calls
    /// this method makes, in the case of some expressions, such as
    /// assignments.
    #[cached_query(result)]
    #[libftrace::traced(level = Trace, err)]
    pub fn type_of_pattern(&self, pat: &lume_hir::Pattern) -> Result<TypeRef> {
        let ty = match &pat.kind {
            lume_hir::PatternKind::Literal(lit) => self.type_of_lit(&lit.literal)?,
            lume_hir::PatternKind::Variant(var) => {
                let enum_name = var.name.clone().parent().unwrap();
                let type_args = self.mk_type_refs_from(enum_name.bound_types(), pat.id)?;

                match self.tdb().find_type(&enum_name) {
                    Some(ty) => TypeRef {
                        instance_of: ty.id,
                        bound_types: type_args,
                        location: pat.location,
                    },
                    None => {
                        return Err(self.missing_type_err(&lume_hir::Type {
                            id: lume_hir::TypeId::from(lume_span::NodeId::empty(pat.id.package)),
                            name: var.name.clone(),
                            location: var.name.location,
                        }));
                    }
                }
            }
            lume_hir::PatternKind::Identifier(_) | lume_hir::PatternKind::Wildcard(_) => {
                let def_id = pat.id;

                for parent in self.hir_parent_iter(def_id) {
                    match parent {
                        // If the pattern is not a sub-pattern, we return the type of the operand
                        // which was passed to the parent `switch` expression.
                        lume_hir::Node::Expression(expr) => match &expr.kind {
                            lume_hir::ExpressionKind::Is(is) => return self.type_of(is.target),
                            lume_hir::ExpressionKind::Switch(switch) => return self.type_of(switch.operand),
                            _ => {}
                        },

                        // If the pattern is a sub-pattern, we get the variant pattern it was nested within.
                        // From the variant pattern, we can deduce the type of the subpattern, by the type of
                        // the corresponding field on the enum case definition.
                        lume_hir::Node::Pattern(parent_pat) if parent_pat.id != def_id => {
                            let lume_hir::PatternKind::Variant(variant_pat) = &parent_pat.kind else {
                                panic!("bug!: found sub-pattern inside non-variant pattern");
                            };

                            let field_idx = variant_pat.fields.iter().position(|f| f.id == def_id).unwrap();
                            let field_type = self.type_of_variant_field(&variant_pat.name, field_idx)?;

                            return Ok(field_type);
                        }
                        _ => {}
                    }
                }

                panic!("bug!: pattern outside switch expression");
            }
        };

        Ok(ty.with_location(pat.location))
    }

    /// Returns the *type* of the field within the enum definition with the
    /// given name.
    #[cached_query(result)]
    #[libftrace::traced(level = Trace, err)]
    pub fn type_of_variant_field(&self, variant_name: &Path, field: usize) -> Result<TypeRef> {
        let enum_def = self.enum_def_with_name(&variant_name.clone().parent().unwrap())?;
        let enum_case_def = self.enum_case_with_name(variant_name)?;

        let Some(enum_field) = enum_case_def.parameters.get(field) else {
            return Err(diagnostics::ArgumentCountMismatch {
                source: variant_name.location,
                expected: enum_case_def.parameters.len(),
                actual: field,
            }
            .into());
        };

        let field_type = self.mk_type_ref_from(enum_field, enum_def.id)?;

        let type_args = self.mk_type_refs_from(&variant_name.all_bound_types(), enum_def.id)?;
        let type_params = &enum_def.type_parameters;
        let instantiated_field_type = self.instantiate_type_from(&field_type, type_params, &type_args);

        Ok(instantiated_field_type)
    }

    /// Returns the fully-qualified [`Path`] of the given [`TypeRef`].
    #[libftrace::traced(level = Trace, err, ret(Display))]
    pub fn type_ref_name(&self, type_ref: &TypeRef) -> Result<&Path> {
        Ok(&self.tdb().expect_type(type_ref.instance_of)?.name)
    }

    /// Returns the [`lume_hir::EnumDefinition`] with the given ID.
    ///
    /// # Errors
    ///
    /// Returns [`Err`] if the no type could be find with the given ID, or if
    /// a type was found, but not an enum type definition.
    #[libftrace::traced(level = Trace, err)]
    pub fn enum_definition(&self, id: NodeId) -> Result<&lume_hir::EnumDefinition> {
        let Some(type_def) = self.tdb().type_(id) else {
            return Err(crate::query::diagnostics::NodeNotFound { id }.into());
        };

        let lume_hir::Node::Type(lume_hir::TypeDefinition::Enum(enum_def)) = self.hir_expect_node(type_def.id) else {
            return Err(crate::query::diagnostics::UnexpectedTypeKind {
                found: type_def.kind,
                expected: lume_types::TypeKind::Trait,
            }
            .into());
        };

        Ok(enum_def.as_ref())
    }

    /// Returns the [`lume_hir::EnumDefinition`] with the given path.
    ///
    /// # Errors
    ///
    /// Returns [`Err`] if the no type could be find with the given name, or if
    /// a type was found, but not an enum type definition.
    #[libftrace::traced(level = Trace, err)]
    pub fn enum_def_with_name(&self, name: &Path) -> Result<&lume_hir::EnumDefinition> {
        let Some(type_def) = self.tdb().find_type(name) else {
            return Err(crate::query::diagnostics::TypeNameNotFound {
                name: name.clone(),
                location: name.location,
            }
            .into());
        };

        let lume_hir::Node::Type(lume_hir::TypeDefinition::Enum(enum_def)) = self.hir_expect_node(type_def.id) else {
            return Err(crate::query::diagnostics::UnexpectedTypeKind {
                found: type_def.kind,
                expected: lume_types::TypeKind::Enum,
            }
            .into());
        };

        Ok(enum_def.as_ref())
    }

    /// Returns the enum case definitions on the enum type with the given ID.
    ///
    /// # Errors
    ///
    /// Returns [`Err`] if the no type could be find with the given ID, or if
    /// a type was found, but not an enum type definition.
    #[libftrace::traced(level = Trace, err)]
    pub fn enum_cases_of(&self, ty: NodeId) -> Result<&[lume_hir::EnumDefinitionCase]> {
        Ok(&self.enum_definition(ty)?.cases)
    }

    /// Returns the enum case definitions on the enum type with the given name.
    ///
    /// # Errors
    ///
    /// Returns [`Err`] if the no type could be find with the given name, or if
    /// a type was found, but not an enum type definition.
    #[libftrace::traced(level = Trace, err)]
    pub fn cases_of_enum_definition(&self, name: &Path) -> Result<&[lume_hir::EnumDefinitionCase]> {
        Ok(&self.enum_def_with_name(name)?.cases)
    }

    /// Returns the enum case definition, which uses the given variant path.
    ///
    /// # Errors
    ///
    /// Returns [`Err`] if:
    /// - the given path doesn't have any parent path segments,
    /// - the no type could be find with the given name
    /// - or a type was found, but not an enum type definition
    #[libftrace::traced(level = Trace, err)]
    pub fn enum_case_with_name(&self, variant: &Path) -> Result<&lume_hir::EnumDefinitionCase> {
        let Some(parent_ty_path) = variant.clone().parent() else {
            return Err(crate::query::diagnostics::PathWithoutParent { path: variant.clone() }.into());
        };

        for case in self.cases_of_enum_definition(&parent_ty_path)? {
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

    /// Returns the enum case definitions, which is being referred to by the
    /// given expression.
    ///
    /// # Errors
    ///
    /// Returns [`Err`] if the no expression could be find with the given ID, or
    /// if the expression type wasn't a reference to an enum definition.
    #[libftrace::traced(level = Trace, err)]
    pub fn cases_of_enum_expr(&self, id: NodeId) -> Result<&[lume_hir::EnumDefinitionCase]> {
        self.enum_cases_of(self.type_of(id)?.instance_of)
    }

    /// Returns the enum case definition, which is being referred to by the
    /// given `Variant` expression.
    ///
    /// # Panics
    ///
    /// This method will panic if the given expression is not a `Variant`
    /// expression.
    #[libftrace::traced(level = Trace, err)]
    pub fn case_of_enum_expr(&self, id: NodeId) -> Result<&lume_hir::EnumDefinitionCase> {
        let expr = self.hir_expect_expr(id);

        if let lume_hir::ExpressionKind::Variant(var) = &expr.kind {
            self.enum_case_with_name(&var.name)
        } else {
            panic!("bug!: attempted to find enum variant of non-variant expression")
        }
    }

    /// Gets the case zero-indexed index of the variant within the given enum
    /// type definition.
    ///
    /// # Errors
    ///
    /// Returns [`Err`] if the no type could be find with the given ID, or if
    /// a type was found, but not an enum type definition.
    #[cached_query(result)]
    #[libftrace::traced(level = Trace, err)]
    pub fn discriminant_of_variant(&self, type_id: NodeId, name: &str) -> Result<usize> {
        for case in self.enum_cases_of(type_id)? {
            if case.name.name().as_str() == name {
                return Ok(case.idx);
            }
        }

        Err(diagnostics::VariantNameNotFound {
            type_id,
            name: name.to_string(),
        }
        .into())
    }

    /// Returns the field of the constructor expression with the given name of
    /// `field_name`, if any.
    ///
    /// Otherwise, returns [`None`].
    #[cached_query]
    #[libftrace::traced(level = Trace, ret)]
    pub fn constructer_field_of(
        &self,
        expr: &lume_hir::Construct,
        field_name: &str,
    ) -> Option<lume_hir::ConstructorField> {
        if let Some(field) = expr.fields.iter().find(|field| field.name.as_str() == field_name) {
            return Some(field.clone());
        }

        self.constructer_default_field_of(expr, field_name)
    }

    /// Constructs a new [`lume_hir::ConstructorField`] from the given
    /// constructor field which uses the default value of the field.
    ///
    /// If the field doesn't have any default value, returns [`None`].
    #[cached_query]
    #[libftrace::traced(level = Trace, ret)]
    pub fn constructer_default_field_of(
        &self,
        expr: &lume_hir::Construct,
        field_name: &str,
    ) -> Option<lume_hir::ConstructorField> {
        let construct_type = self.find_type_ref(&expr.path).ok()??;
        let struct_hir_type = self.hir_expect_struct(construct_type.instance_of);

        let matching_field = struct_hir_type.fields().find(|prop| prop.name.as_str() == field_name)?;

        if let Some(default_value) = matching_field.default_value {
            return Some(lume_hir::ConstructorField {
                name: lume_hir::Identifier::from(field_name),
                value: default_value,
                is_default: true,
                location: self.hir_span_of_node(default_value),
            });
        }

        None
    }

    /// Determines whether the given can actually be assigned a value.
    ///
    /// For example, the given value would never be able to be assigned:
    /// ```lm
    /// let a = 0_i32;
    /// a + 1 = 3;
    /// ```
    #[cached_query]
    #[libftrace::traced(level = Trace, ret)]
    pub fn can_assign_value(&self, id: NodeId) -> bool {
        match &self.hir_expect_expr(id).kind {
            lume_hir::ExpressionKind::Assignment(_)
            | lume_hir::ExpressionKind::Scope(_)
            | lume_hir::ExpressionKind::Cast(_)
            | lume_hir::ExpressionKind::Construct(_)
            | lume_hir::ExpressionKind::InstanceCall(_)
            | lume_hir::ExpressionKind::IntrinsicCall(_)
            | lume_hir::ExpressionKind::StaticCall(_)
            | lume_hir::ExpressionKind::If(_)
            | lume_hir::ExpressionKind::Is(_)
            | lume_hir::ExpressionKind::Literal(_)
            | lume_hir::ExpressionKind::Switch(_)
            | lume_hir::ExpressionKind::Variant(_) => false,
            lume_hir::ExpressionKind::Member(_) | lume_hir::ExpressionKind::Variable(_) => true,
        }
    }

    /// Determines whether a value is expected from the given expression.
    ///
    /// For example, given an expression like this:
    /// ```lm
    /// if a { b } else { c }
    /// ```
    ///
    /// Even though the `if` statement returns a value of `Boolean`, the
    /// resulting value is never assigned to anything, so no value is
    /// expected.
    #[cached_query(result)]
    #[libftrace::traced(level = Trace, err, ret)]
    pub fn is_value_expected(&self, id: NodeId) -> Result<bool> {
        let Some(parent_id) = self.hir_parent_of(id) else {
            return Ok(false);
        };

        match self.hir_node(parent_id).unwrap() {
            lume_hir::Node::Expression(expr) => match &expr.kind {
                lume_hir::ExpressionKind::Assignment(e) => {
                    if e.target == id {
                        self.is_value_expected(e.id)
                    } else {
                        Ok(false)
                    }
                }
                _ => Ok(true),
            },
            lume_hir::Node::Statement(stmt) => match &stmt.kind {
                lume_hir::StatementKind::Variable(_)
                | lume_hir::StatementKind::Final(_)
                | lume_hir::StatementKind::Return(_)
                | lume_hir::StatementKind::IteratorLoop(_) => Ok(true),
                lume_hir::StatementKind::Break(_) => unreachable!("break statements cannot have sub expressions"),
                lume_hir::StatementKind::Continue(_) => unreachable!("continue statements cannot have sub expressions"),
                lume_hir::StatementKind::InfiniteLoop(_) => unreachable!("infinite loops cannot have sub expressions"),
                lume_hir::StatementKind::Expression(_) => Ok(false),
            },
            lume_hir::Node::Field(_) => Ok(true),
            _ => panic!("bug!: expression not contained within statement, expression or field"),
        }
    }

    /// Attempts to get the expected type of the [`lume_hir::Expression`] with
    /// the given ID, in the context in which it is defined. For example,
    /// given an expression like this:
    /// ```lm
    /// let _: std::Array<Int32> = std::Array::new();
    /// ```
    ///
    /// We can infer the expected type of the expression `std::Array::new()`
    /// since it is explicitly declared on the variable declaration. In
    /// another instances it might not be as explicit, which may cause the
    /// method to return [`None`]. For example, an expression like:
    /// ```lm
    /// let _ = std::Array::new();
    /// ```
    ///
    /// would be impossible to solve, since no explicit type is declared.
    ///
    /// If a type could not be determined, [`Self::guess_type_of_ctx`] is
    /// invoked to attempt to infer the type from other expressions and
    /// statements which reference the target expression.
    #[cached_query(result)]
    #[libftrace::traced(level = Trace, err)]
    pub fn expected_type_of(&self, id: NodeId) -> Result<Option<TypeRef>> {
        if let Some(expected_type) = self.try_expected_type_of(id)? {
            Ok(Some(expected_type))
        } else {
            self.guess_type_of_ctx(id)
        }
    }

    /// Attempts to get the expected type of the [`lume_hir::Expression`] with
    /// the given ID, in the context in which it is defined. For example,
    /// given an expression like this:
    /// ```lm
    /// let _: std::Array<Int32> = std::Array::new();
    /// ```
    ///
    /// We can infer the expected type of the expression `std::Array::new()`
    /// since it is explicitly declared on the variable declaration. In
    /// another instances it might not be as explicit, which may cause the
    /// method to return [`None`]. For example, an expression like:
    /// ```lm
    /// let _ = std::Array::new();
    /// ```
    ///
    /// would be impossible to solve, since no explicit type is declared.
    #[cached_query(result)]
    #[libftrace::traced(level = Trace, err)]
    pub fn try_expected_type_of(&self, id: NodeId) -> Result<Option<TypeRef>> {
        let Some(parent_id) = self.hir_parent_of(id) else {
            return Ok(None);
        };

        let Some(parent) = self.hir_node(parent_id) else {
            return Ok(None);
        };

        match parent {
            lume_hir::Node::Expression(expr) => match &expr.kind {
                lume_hir::ExpressionKind::Assignment(_) | lume_hir::ExpressionKind::Scope(_) => {
                    self.try_expected_type_of(parent_id)
                }
                lume_hir::ExpressionKind::Cast(_) | lume_hir::ExpressionKind::Member(_) => Ok(None),
                lume_hir::ExpressionKind::Construct(construct) => self.expected_type_of_construct(id, construct),
                lume_hir::ExpressionKind::InstanceCall(call) => {
                    self.expected_type_of_call(id, CallExpression::Instanced(call))
                }
                lume_hir::ExpressionKind::IntrinsicCall(call) => {
                    self.expected_type_of_call(id, CallExpression::Intrinsic(call))
                }
                lume_hir::ExpressionKind::StaticCall(call) => {
                    self.expected_type_of_call(id, CallExpression::Static(call))
                }
                lume_hir::ExpressionKind::If(_) => Ok(Some(TypeRef::bool().with_location(expr.location))),
                lume_hir::ExpressionKind::Is(expr) => {
                    if id == expr.pattern.id
                        && let Some(lume_hir::Node::Pattern(_)) = self.hir_node(id)
                    {
                        return self.type_of(expr.target).map(Some);
                    }

                    Ok(Some(TypeRef::bool().with_location(expr.location)))
                }
                lume_hir::ExpressionKind::Literal(_) => unreachable!("literals cannot have sub expressions"),
                lume_hir::ExpressionKind::Switch(switch) => {
                    if switch.operand == id {
                        return Ok(None);
                    }

                    if let Some(lume_hir::Node::Pattern(_)) = self.hir_node(id) {
                        return self.type_of(switch.operand).map(Some);
                    }

                    return self.expected_type_of(switch.id);
                }
                lume_hir::ExpressionKind::Variable(_) => {
                    unreachable!("variable references cannot have sub expressions")
                }
                lume_hir::ExpressionKind::Variant(variant) => {
                    let idx = variant
                        .arguments
                        .iter()
                        .enumerate()
                        .find_map(|(idx, arg)| if *arg == id { Some(idx) } else { None })
                        .expect("bug!: expression not contained in variant arg list");

                    let enum_name = variant.name.clone().parent().unwrap();

                    let enum_def = self.enum_def_with_name(&enum_name)?;
                    let enum_case_def = self.enum_case_with_name(&variant.name)?;
                    let Some(enum_field_type) = enum_case_def.parameters.get(idx) else {
                        return Ok(None);
                    };

                    self.mk_type_ref_from(enum_field_type, enum_def.id).map(Some)
                }
            },
            lume_hir::Node::Statement(stmt) => match &stmt.kind {
                lume_hir::StatementKind::Variable(decl) => {
                    if let Some(declared_type) = &decl.declared_type {
                        let type_ref = self.mk_type_ref_from(declared_type, decl.id)?;

                        Ok(Some(type_ref))
                    } else {
                        Ok(None)
                    }
                }
                lume_hir::StatementKind::Break(_) => unreachable!("break statements cannot have sub expressions"),
                lume_hir::StatementKind::Continue(_) => unreachable!("continue statements cannot have sub expressions"),
                lume_hir::StatementKind::Final(fin) => match self.hir_parent_node_of(fin.id) {
                    Some(Node::Expression(parent)) => self.try_expected_type_of(parent.id),
                    Some(Node::Statement(_)) => Ok(Some(TypeRef::void().with_location(fin.location))),
                    _ => self.return_type_within(fin.id).map(Some),
                },
                lume_hir::StatementKind::Return(ret) => self.return_type_within(ret.id).map(Some),
                lume_hir::StatementKind::InfiniteLoop(_) => unreachable!("infinite loops cannot have sub expressions"),
                lume_hir::StatementKind::IteratorLoop(_) => todo!("expected_type_of IteratorLoop statement"),
                lume_hir::StatementKind::Expression(expr) => {
                    let location = self.hir_span_of_node(*expr);

                    Ok(Some(TypeRef::void().with_location(location)))
                }
            },
            lume_hir::Node::Field(field) => {
                let type_ref = self.mk_type_ref_from(&field.field_type, field.id)?;

                Ok(Some(type_ref))
            }
            _ => panic!("bug!: expression not contained within statement, expression or field"),
        }
    }

    #[cached_query(result)]
    #[libftrace::traced(level = Trace, err)]
    fn expected_type_of_construct(&self, expr: NodeId, construct: &lume_hir::Construct) -> Result<Option<TypeRef>> {
        let Some(constructed_type) = self.find_type_ref_from(&construct.path, construct.id)? else {
            return Ok(None);
        };

        let field_name = construct
            .fields
            .iter()
            .find_map(|field| {
                if field.value == expr {
                    Some(field.name.as_str())
                } else {
                    None
                }
            })
            .expect("could not find expression in constructer fields");

        let Some(field) = self.constructer_field_of(construct, field_name) else {
            return Ok(None);
        };

        if let Some(type_field) = self.field_on(constructed_type.instance_of, &field.name.name)? {
            let field_type = self.mk_type_ref_from(&type_field.field_type, constructed_type.instance_of)?;

            return Ok(Some(field_type));
        }

        Ok(None)
    }

    #[cached_query(result)]
    #[libftrace::traced(level = Trace, err)]
    fn expected_type_of_call(&self, expr: NodeId, call: CallExpression<'_>) -> Result<Option<TypeRef>> {
        let callable = self.probe_callable(call)?;

        let is_instance_call = if let CallExpression::Instanced(_) = call
            && let Callable::Method(_) = callable
        {
            self.is_instanced_method(callable.id())
        } else {
            false
        };

        // If we're attempting to determine the type of a literal without any explicit
        // numeric kind, make sure it's not as a generic argument. If it is, we can
        // end up creating an infinite loop when attempting to instantiate the
        // signature.
        if self.should_infer_literal_kind(expr)
            && let Some(mut argument_idx) = call.find_arg_idx(expr)
        {
            if is_instance_call {
                argument_idx += 1;
            }

            let signature = self.signature_of(callable)?;
            let Some(parameter) = signature.params.get(argument_idx) else {
                return Ok(None);
            };

            if self.is_type_generic(&parameter.ty)? {
                return Ok(None);
            }

            return Ok(Some(parameter.ty.clone()));
        }

        let signature = self.instantiate_signature_from_args(callable, call)?;

        // If the ID refers to the callee of an instance method, return the type
        // expected from the instance method we found.
        if let CallExpression::Instanced(instance_call) = call
            && expr == instance_call.callee
        {
            let callee_type = &signature.params.first().unwrap().ty;

            return Ok(Some(callee_type.clone()));
        }

        let mut argument_idx = call.find_arg_idx(expr).expect("could not find expression in arg list");
        if is_instance_call {
            argument_idx += 1;
        }

        let parameter = &signature.params[argument_idx];

        Ok(Some(parameter.ty.clone()))
    }

    /// Attempts to guess the expected type of the given expression, based on
    /// other statements and expressions which refer to the target
    /// expression.
    #[cached_query(result)]
    #[libftrace::traced(level = Trace, err)]
    pub fn guess_type_of_ctx(&self, id: NodeId) -> Result<Option<TypeRef>> {
        for expr_node_ref in self.indirect_expression_refs(id)? {
            let Some(expr_node) = self.hir_node(expr_node_ref) else {
                continue;
            };

            match expr_node {
                Node::Statement(stmt) => {
                    if let lume_hir::StatementKind::Variable(decl) = &stmt.kind
                        && let Some(declared_type) = &decl.declared_type
                    {
                        return Ok(Some(self.mk_type_ref_from(declared_type, decl.id)?));
                    }
                }
                Node::Expression(expr) => {
                    if let Some(expected_type) = self.try_expected_type_of(expr.id)? {
                        return Ok(Some(expected_type));
                    }
                }
                _ => {}
            }
        }

        Ok(None)
    }

    #[libftrace::traced(level = Trace, err)]
    pub fn indirect_expression_refs(&self, id: NodeId) -> Result<Vec<NodeId>> {
        let Some(parent) = self.hir_parent_node_of(id) else {
            return Ok(Vec::new());
        };

        let mut refs = vec![parent.id()];

        if let Node::Statement(stmt) = parent
            && let lume_hir::StatementKind::Variable(decl) = &stmt.kind
        {
            for expr in self.hir.expressions() {
                if let lume_hir::ExpressionKind::Variable(var_ref) = &expr.kind
                    && let lume_hir::VariableSource::Variable(var_source) = &var_ref.reference
                    && var_source.id == decl.id
                {
                    refs.push(expr.id);
                }
            }
        }

        Ok(refs)
    }

    /// Determines whether all the arms in the given `switch` expression have a
    /// pattern which refer to constant literals. Fallback patterns are also
    /// allowed in constant contexts.
    #[cached_query]
    #[libftrace::traced(level = Trace, ret)]
    pub fn is_switch_constant(&self, expr: &lume_hir::Switch) -> bool {
        for case in &expr.cases {
            match &case.pattern.kind {
                lume_hir::PatternKind::Variant(_) => return false,
                lume_hir::PatternKind::Literal(lit) => {
                    if !matches!(
                        lit.literal.kind,
                        lume_hir::LiteralKind::Int(_)
                            | lume_hir::LiteralKind::Float(_)
                            | lume_hir::LiteralKind::Boolean(_)
                    ) {
                        return false;
                    }
                }
                lume_hir::PatternKind::Identifier(_) | lume_hir::PatternKind::Wildcard(_) => {}
            }
        }

        true
    }

    /// Attempts to determine whether the kind of the given literal expression
    /// should be inferred.
    ///
    /// Literal kinds are explicit suffixes appended after a numeric literal,
    /// such as `1_u32` or `1.0_f64`. This method determines whether the literal
    /// kind is missing, therefore requiring it to be inferred.
    #[cached_query]
    #[libftrace::traced(level = Trace)]
    fn should_infer_literal_kind(&self, id: NodeId) -> bool {
        let Some(expr) = self.hir_expr(id) else {
            return false;
        };

        let lume_hir::ExpressionKind::Literal(lit) = &expr.kind else {
            return false;
        };

        match &lit.kind {
            lume_hir::LiteralKind::Int(lit) => lit.kind.is_none(),
            lume_hir::LiteralKind::Float(lit) => lit.kind.is_none(),
            _ => false,
        }
    }

    /// Returns a slice of all [`lume_hir::Field`]s on the `struct` definition
    /// with the given ID.
    ///
    /// If the given ID does not refer to a struct definition, returns an empty
    /// slice.
    pub fn fields_on(&self, id: NodeId) -> Result<&[lume_hir::Field]> {
        let Some(Node::Type(lume_hir::TypeDefinition::Struct(struct_def))) = self.hir_node(id) else {
            return Ok(&[]);
        };

        Ok(&struct_def.fields)
    }

    /// Returns the [`lume_hir::Field`] on the `struct` definition
    /// with the given ID, which has the name `name`.
    ///
    /// If the given ID does not refer to a struct definition, returns an empty
    /// slice. If no such field was found on the `struct` definition, returns
    /// [`None`].
    #[libftrace::traced(level = Trace)]
    pub fn field_on(&self, id: NodeId, name: &str) -> Result<Option<&lume_hir::Field>> {
        Ok(self.fields_on(id)?.iter().find(|field| field.name.as_str() == name))
    }

    /// Gets the visibility of the given node, if one can be applied to it.
    ///
    /// If the type of node cannot have a visibility modifier, returns [`None`].
    #[cached_query]
    #[libftrace::traced(level = Trace)]
    pub fn visibility_of(&self, id: NodeId) -> Option<Visibility> {
        match self.hir_node(id)? {
            Node::Function(n) => Some(n.visibility),
            Node::Type(def) => match def {
                lume_hir::TypeDefinition::Enum(n) => Some(n.visibility),
                lume_hir::TypeDefinition::Struct(n) => Some(n.visibility),
                lume_hir::TypeDefinition::Trait(n) => Some(n.visibility),
                lume_hir::TypeDefinition::TypeParameter(_) => None,
            },
            Node::TraitImpl(_)
            | Node::TraitMethodDef(_)
            | Node::TraitMethodImpl(_)
            | Node::Impl(_)
            | Node::Pattern(_)
            | Node::Statement(_)
            | Node::Expression(_) => None,
            Node::Field(n) => Some(n.visibility),
            Node::Method(n) => Some(n.visibility),
        }
    }
}
