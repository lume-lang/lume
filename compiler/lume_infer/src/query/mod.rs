use crate::{TyInferCtx, *};
use error_snippet::Result;
use lume_hir::{CallExpression, Identifier, Node, Path};
use lume_query::cached_query;
use lume_span::NodeId;
use lume_types::{Function, Method, Trait, TypeRef};

mod diagnostics;
pub mod hir;
pub mod lookup;

#[derive(Debug, Hash, Copy, Clone, PartialEq, Eq)]
pub enum CallReference {
    /// The call refers to a function.
    Function(NodeId),

    /// The call refers to a method.
    Method(NodeId),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Callable<'a> {
    /// The call refers to a function.
    Function(&'a Function),

    /// The call refers to a method.
    Method(&'a Method),
}

impl Callable<'_> {
    pub fn id(&self) -> NodeId {
        match self {
            Self::Method(method) => method.id,
            Self::Function(function) => function.id,
        }
    }

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

    pub fn is_instance(&self) -> bool {
        if let Self::Method(method) = self {
            method.is_instanced()
        } else {
            false
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
    pub fn type_of(&self, def: NodeId) -> Result<TypeRef> {
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
            lume_hir::ExpressionKind::Assignment(e) => self.type_of(e.value)?,
            lume_hir::ExpressionKind::Cast(e) => self.mk_type_ref(&e.target)?,
            lume_hir::ExpressionKind::Construct(e) => {
                let Some(ty_opt) = self.find_type_ref_from(&e.path, e.id)? else {
                    return Err(self.missing_type_err(&lume_hir::Type {
                        id: lume_span::NodeId::empty(expr.id.package),
                        name: e.path.clone(),
                        location: e.path.location,
                    }));
                };

                let type_parameters = self.hir_avail_type_params(e.id);
                let type_parameters_id: Vec<NodeId> = type_parameters.iter().map(|p| p.id).collect();

                let type_args = self.mk_type_refs_from(e.path.type_arguments(), e.id)?;
                let instantiated = self.instantiate_type_from(&ty_opt, &type_parameters_id, &type_args);

                instantiated.clone()
            }
            lume_hir::ExpressionKind::Binary(expr) => self.type_of(expr.lhs)?,
            lume_hir::ExpressionKind::StaticCall(call) => self.type_of_call(lume_hir::CallExpression::Static(call))?,
            lume_hir::ExpressionKind::InstanceCall(call) => {
                self.type_of_call(lume_hir::CallExpression::Instanced(call))?
            }
            lume_hir::ExpressionKind::IntrinsicCall(call) => {
                self.type_of_call(lume_hir::CallExpression::Intrinsic(call))?
            }
            lume_hir::ExpressionKind::If(cond) => self.type_of_if_conditional(cond)?,
            lume_hir::ExpressionKind::Is(_) => TypeRef::bool(),
            lume_hir::ExpressionKind::Literal(e) => self.type_of_lit(e),
            lume_hir::ExpressionKind::Logical(expr) => self.type_of(expr.lhs)?,
            lume_hir::ExpressionKind::Member(expr) => {
                let callee_type = self.type_of(expr.callee)?;

                let Some(field) = self.tdb().find_field(callee_type.instance_of, &expr.name) else {
                    let ty = self.tdb().type_(callee_type.instance_of).unwrap();

                    return Err(crate::errors::MissingField {
                        source: expr.location.file.clone(),
                        range: expr.location.index.clone(),
                        type_name: ty.name.clone(),
                        field_name: Identifier {
                            name: expr.name.clone(),
                            location: expr.location,
                        },
                    }
                    .into());
                };

                field.field_type.clone()
            }
            lume_hir::ExpressionKind::Field(field) => {
                let pattern = self.hir_expect_pattern(field.pattern);
                let lume_hir::PatternKind::Variant(variant_pat) = &pattern.kind else {
                    panic!("bug!: found field expression referencing non-variant pattern");
                };

                self.type_of_variant_field(&variant_pat.name, field.field)?
            }
            lume_hir::ExpressionKind::Scope(scope) => self.type_of_scope(scope)?,
            lume_hir::ExpressionKind::Switch(switch) => match switch.cases.first() {
                Some(case) => self.type_of(case.branch)?,
                None => TypeRef::void(),
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
                        id: lume_span::NodeId::empty(expr.id.package),
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
            lume_hir::LiteralKind::String(_) => self.std_ref_string(),
            lume_hir::LiteralKind::Boolean(_) => TypeRef::bool(),
        };

        ty.with_location(lit.location)
    }

    /// Returns the *type* of the given [`lume_hir::Parameter`], before the
    /// ancestry tree has been initialized.
    #[cached_query(result)]
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub fn type_of_parameter_pre(
        &self,
        param: &lume_hir::Parameter,
        type_params: &[&TypeParameter],
    ) -> Result<TypeRef> {
        let elemental_type = self.mk_type_ref_generic(&param.param_type, type_params)?;

        if param.vararg {
            Result::Ok(self.std_ref_array(elemental_type))
        } else {
            Result::Ok(elemental_type)
        }
    }

    /// Returns the *type* of the given [`lume_hir::Parameter`].
    #[cached_query(result)]
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub fn type_of_parameter(&self, param: &lume_hir::Parameter, owner: NodeId) -> Result<TypeRef> {
        let elemental_type = self.mk_type_ref_from(&param.param_type, owner)?;

        if param.vararg {
            Result::Ok(self.std_ref_array(elemental_type))
        } else {
            Result::Ok(elemental_type)
        }
    }

    /// Returns the return type of the given [`lume_hir::Scope`].
    #[cached_query(result)]
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub fn type_of_scope(&self, scope: &lume_hir::Scope) -> Result<TypeRef> {
        if let Some(stmt) = scope.body.last() {
            let stmt = self.hir.expect_statement(*stmt)?;

            if let lume_hir::StatementKind::Final(fin) = &stmt.kind {
                return self.type_of(fin.value);
            }
        }

        Result::Ok(TypeRef::void())
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
            lume_hir::StatementKind::Final(fin) => self.type_of(fin.value),
            lume_hir::StatementKind::IteratorLoop(l) => self.type_of_block(&l.block),
            lume_hir::StatementKind::InfiniteLoop(l) => self.type_of_block(&l.block),
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

    /// Returns the returned type of the given [`lume_hir::Condition`].
    #[cached_query(result)]
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub fn type_of_condition_scope(&self, cond: &lume_hir::Condition) -> Result<TypeRef> {
        let Some(last_statement) = cond.block.statements.last() else {
            return Ok(TypeRef::void());
        };

        let stmt = self.hir.expect_statement(*last_statement)?;

        if let lume_hir::StatementKind::Final(fin) = &stmt.kind {
            self.type_of(fin.value)
        } else {
            Ok(TypeRef::void())
        }
    }

    /// Returns the *type* of the given [`lume_hir::VariableDeclaration`].
    #[cached_query(result)]
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub fn type_of_vardecl(&self, stmt: &lume_hir::VariableDeclaration) -> Result<TypeRef> {
        if let Some(declared_type) = &stmt.declared_type {
            self.mk_type_ref_from(declared_type, stmt.id)
        } else {
            self.type_of(stmt.value)
        }
    }

    /// Returns the *type* of the given [`lume_hir::Return`].
    #[cached_query(result)]
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub fn type_of_return(&self, stmt: &lume_hir::Return) -> Result<TypeRef> {
        if let Some(expr) = stmt.value {
            self.type_of(expr)
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

        let stmt = self.hir.expect_statement(*last_statement)?;
        self.type_of_stmt(stmt)
    }

    /// Returns the return type of the given [`lume_hir::CallExpression`].
    #[cached_query(result)]
    #[tracing::instrument(level = "TRACE", skip(self), fields(name = %expr.name()), err, ret)]
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
    pub fn type_of_pattern(&self, pat: &lume_hir::Pattern) -> Result<TypeRef> {
        let ty = match &pat.kind {
            lume_hir::PatternKind::Literal(lit) => self.type_of_lit(&lit.literal),
            lume_hir::PatternKind::Variant(var) => {
                let enum_name = var.name.clone().parent().unwrap();
                let type_args = self.mk_type_refs_from(enum_name.type_arguments(), pat.id)?;

                match self.tdb().find_type(&enum_name) {
                    Some(ty) => TypeRef {
                        instance_of: ty.id,
                        type_arguments: type_args,
                        location: pat.location,
                    },
                    None => {
                        return Err(self.missing_type_err(&lume_hir::Type {
                            id: lume_span::NodeId::empty(pat.id.package),
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
                            _ => continue,
                        },

                        // If the pattern is a sub-pattern, we get the variant pattern it was nested within.
                        // From the variant pattern, we can deduce the type of the subpattern, by the type of
                        // the corresponding field on the enum case definition.
                        lume_hir::Node::Pattern(parent_pat) if parent_pat.id != def_id => {
                            let lume_hir::PatternKind::Variant(variant_pat) = &parent_pat.kind else {
                                panic!("bug!: found sub-pattern inside non-variant pattern");
                            };

                            let field_idx = variant_pat.fields.iter().position(|f| f.id == def_id).unwrap();

                            return self.type_of_variant_field(&variant_pat.name, field_idx);
                        }
                        _ => {}
                    }
                }

                panic!("bug!: pattern outside switch expression");
            }
        };

        Result::Ok(ty.with_location(pat.location))
    }

    /// Returns the *type* of the field within the enum definition with the given name.
    #[cached_query(result)]
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub fn type_of_variant_field(&self, variant_name: &Path, field: usize) -> Result<TypeRef> {
        let enum_def = self.enum_def_of_name(&variant_name.clone().parent().unwrap())?;
        let enum_case_def = self.enum_case_with_name(variant_name)?;

        let Some(enum_field) = enum_case_def.parameters.get(field) else {
            return Err(diagnostics::ArgumentCountMismatch {
                source: variant_name.location,
                expected: enum_case_def.parameters.len(),
                actual: field,
            }
            .into());
        };

        self.mk_type_ref_from(enum_field, enum_def.id)
    }

    /// Returns the fully-qualified [`Path`] of the given [`TypeRef`].
    #[tracing::instrument(level = "TRACE", skip(self), err, ret(Display))]
    pub fn type_ref_name(&self, type_ref: &TypeRef) -> Result<&Path> {
        Ok(&self.tdb().ty_expect(type_ref.instance_of)?.name)
    }

    /// Returns the [`Trait`] definition, which matches the [`Use`] declaration with the given ID.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub fn trait_def_of(&self, id: NodeId) -> Result<&Trait> {
        let Some(use_ref) = self.tdb().use_(id) else {
            return Err(lume_types::errors::NodeNotFound { id }.into());
        };

        self.tdb().ty_expect_trait(use_ref.trait_.instance_of)
    }

    /// Returns the enum definition with the given type ID.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub fn enum_def_type(&self, id: NodeId) -> Result<&lume_hir::EnumDefinition> {
        let Some(parent_ty) = self.tdb().type_(id) else {
            return Err(lume_types::errors::NodeNotFound { id }.into());
        };

        let lume_types::TypeKind::User(lume_types::UserType::Enum(enum_ty)) = &parent_ty.kind else {
            return Err(lume_types::errors::UnexpectedTypeKind {
                found: parent_ty.kind.as_kind_ref(),
                expected: lume_types::TypeKindRef::Enum,
            }
            .into());
        };

        Ok(self.hir_expect_enum(enum_ty.id))
    }

    /// Returns the enum definition with the given name.
    #[cached_query(result)]
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub fn enum_def_of_name(&self, name: &Path) -> Result<&lume_hir::EnumDefinition> {
        let Some(parent_ty) = self.tdb().find_type(name) else {
            return Err(lume_types::errors::TypeNameNotFound {
                name: name.clone(),
                location: name.location,
            }
            .into());
        };

        let lume_types::TypeKind::User(lume_types::UserType::Enum(enum_ty)) = &parent_ty.kind else {
            return Err(lume_types::errors::UnexpectedTypeKind {
                found: parent_ty.kind.as_kind_ref(),
                expected: lume_types::TypeKindRef::Trait,
            }
            .into());
        };

        Result::Ok(self.hir_expect_enum(enum_ty.id))
    }

    /// Returns the enum case definitions on the enum type with the given ID.
    #[cached_query(result)]
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub fn enum_cases_of(&self, ty: NodeId) -> Result<&[lume_hir::EnumDefinitionCase]> {
        Result::Ok(&self.enum_def_type(ty)?.cases)
    }

    /// Returns the enum case definitions on the enum type with the given name.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub fn enum_cases_of_name(&self, name: &Path) -> Result<&[lume_hir::EnumDefinitionCase]> {
        let Some(parent_ty) = self.tdb().find_type(name) else {
            return Err(lume_types::errors::TypeNameNotFound {
                name: name.clone(),
                location: name.location,
            }
            .into());
        };

        self.enum_cases_of(parent_ty.id)
    }

    /// Returns the enum case definition, which is being referred to by the given expression.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub fn enum_case_expr(&self, id: NodeId) -> Result<&lume_hir::EnumDefinitionCase> {
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

    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub fn discriminant_of_variant_ty(&self, type_id: NodeId, name: &str) -> Result<usize> {
        for case in self.enum_cases_of(type_id)? {
            if case.name.name().as_str() == name {
                return Ok(case.idx);
            }
        }

        panic!("bug!: could not find variant in {type_id:?} with name {name:?}")
    }

    /// Returns the field of the constructor expression, matching the given field.
    #[cached_query]
    #[tracing::instrument(level = "TRACE", skip(self), ret)]
    pub fn constructer_field_of(
        &self,
        expr: &lume_hir::Construct,
        prop_name: &str,
    ) -> Option<lume_hir::ConstructorField> {
        if let Some(field) = expr.fields.iter().find(|field| field.name.as_str() == prop_name) {
            return Some(field.clone());
        }

        self.constructer_default_field_of(expr, prop_name)
    }

    /// Returns the field of the constructor expression, matching the given field.
    #[cached_query]
    #[tracing::instrument(level = "TRACE", skip(self), ret)]
    pub fn constructer_default_field_of(
        &self,
        expr: &lume_hir::Construct,
        prop_name: &str,
    ) -> Option<lume_hir::ConstructorField> {
        let construct_type = self.find_type_ref(&expr.path).ok()??;
        let struct_type = self.tdb().ty_expect_struct(construct_type.instance_of).ok()?;
        let struct_hir_type = self.hir_expect_struct(struct_type.id);

        let matching_field = struct_hir_type.fields().find(|prop| prop.name.as_str() == prop_name)?;

        if let Some(default_value) = matching_field.default_value {
            return Some(lume_hir::ConstructorField {
                name: lume_hir::Identifier::from(prop_name),
                value: default_value,
                location: self.hir_span_of_node(default_value),
            });
        }

        None
    }

    /// Determines whether a value is expected from the given expression.
    ///
    /// For example, given an expression like this:
    /// ```lm
    /// if a { b } else { c }
    /// ```
    ///
    /// Even though the `if` statement returns a value of `Boolean`, the resulting value is never assigned
    /// to anything, so no value is expected.
    #[cached_query(result)]
    #[tracing::instrument(level = "TRACE", skip(self), err, ret)]
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
                lume_hir::StatementKind::Variable(_) => Ok(true),
                lume_hir::StatementKind::Break(_) => unreachable!("break statements cannot have sub expressions"),
                lume_hir::StatementKind::Continue(_) => unreachable!("continue statements cannot have sub expressions"),
                lume_hir::StatementKind::Final(_) => Ok(true),
                lume_hir::StatementKind::Return(_) => Ok(true),
                lume_hir::StatementKind::InfiniteLoop(_) => unreachable!("infinite loops cannot have sub expressions"),
                lume_hir::StatementKind::IteratorLoop(_) => Ok(true),
                lume_hir::StatementKind::Expression(_) => Ok(false),
            },
            lume_hir::Node::Field(_) => Ok(true),
            _ => panic!("bug!: expression not contained within statement, expression or field"),
        }
    }

    /// Attempts to get the expected type of the [`Expression`] with the given ID, in the
    /// context in which it is defined. For example, given an expression like this:
    /// ```lm
    /// let _: std::Array<Int32> = std::Array::new();
    /// ```
    ///
    /// We can infer the expected type of the expression `std::Array::new()` since it
    /// is explicitly declared on the variable declaration. In another instances it might
    /// not be as explicit, which may cause the method to return [`None`]. For example, an
    /// expression like:
    /// ```lm
    /// let _ = std::Array::new();
    /// ```
    ///
    /// would be impossible to solve, since no explicit type is declared.
    ///
    /// If a type could not be determined, [`guess_type_of_ctx`] is invoked to attempt
    /// to infer the type from other expressions and statements which reference the target expression.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub fn expected_type_of(&self, id: NodeId) -> Result<Option<TypeRef>> {
        if let Some(expected_type) = self.try_expected_type_of(id)? {
            Ok(Some(expected_type))
        } else {
            self.guess_type_of_ctx(id)
        }
    }

    /// Attempts to get the expected type of the [`Expression`] with the given ID, in the
    /// context in which it is defined. For example, given an expression like this:
    /// ```lm
    /// let _: std::Array<Int32> = std::Array::new();
    /// ```
    ///
    /// We can infer the expected type of the expression `std::Array::new()` since it
    /// is explicitly declared on the variable declaration. In another instances it might
    /// not be as explicit, which may cause the method to return [`None`]. For example, an
    /// expression like:
    /// ```lm
    /// let _ = std::Array::new();
    /// ```
    ///
    /// would be impossible to solve, since no explicit type is declared.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub fn try_expected_type_of(&self, id: NodeId) -> Result<Option<TypeRef>> {
        let parent_id = self.hir_parent_of(id).expect("bug!: expression exists without parent");

        match self.hir_expect_node(parent_id) {
            lume_hir::Node::Expression(expr) => match &expr.kind {
                lume_hir::ExpressionKind::Assignment(_) => self.try_expected_type_of(parent_id),
                lume_hir::ExpressionKind::Binary(_) => Ok(None),
                lume_hir::ExpressionKind::Cast(_) => Ok(None),
                lume_hir::ExpressionKind::Construct(construct) => self.expected_type_of_construct(id, construct),
                lume_hir::ExpressionKind::InstanceCall(call) => self
                    .expected_type_of_call(id, CallExpression::Instanced(call))
                    .map(|ty| Some(ty)),
                lume_hir::ExpressionKind::IntrinsicCall(call) => self
                    .expected_type_of_call(id, CallExpression::Intrinsic(call))
                    .map(|ty| Some(ty)),
                lume_hir::ExpressionKind::StaticCall(call) => self
                    .expected_type_of_call(id, CallExpression::Static(call))
                    .map(|ty| Some(ty)),
                lume_hir::ExpressionKind::If(_) | lume_hir::ExpressionKind::Is(_) => Ok(Some(TypeRef::bool())),
                lume_hir::ExpressionKind::Literal(_) => unreachable!("literals cannot have sub expressions"),
                lume_hir::ExpressionKind::Logical(_) => Ok(None),
                lume_hir::ExpressionKind::Member(_) => Ok(None),
                lume_hir::ExpressionKind::Field(_) => todo!("expected_type_of field expression"),
                lume_hir::ExpressionKind::Scope(_) => self.try_expected_type_of(parent_id),
                lume_hir::ExpressionKind::Switch(_) => todo!("expected_type_of switch expression"),
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

                    let enum_def = self.enum_def_of_name(&enum_name)?;
                    let enum_case_def = self.enum_case_with_name(&variant.name)?;
                    let enum_field_type = enum_case_def.parameters.get(idx).unwrap();

                    self.mk_type_ref_from(enum_field_type, enum_def.id).map(|ty| Some(ty))
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
                lume_hir::StatementKind::Final(fin) => {
                    if let Some(Node::Expression(parent)) = self.hir_parent_node_of(fin.id) {
                        return self.try_expected_type_of(parent.id);
                    };

                    self.hir_ctx_return_type(fin.id).map(|ty| Some(ty))
                }
                lume_hir::StatementKind::Return(ret) => self.hir_ctx_return_type(ret.id).map(|ty| Some(ty)),
                lume_hir::StatementKind::InfiniteLoop(_) => unreachable!("infinite loops cannot have sub expressions"),
                lume_hir::StatementKind::IteratorLoop(_) => todo!("expected_type_of IteratorLoop statement"),
                lume_hir::StatementKind::Expression(_) => Ok(Some(TypeRef::void())),
            },
            lume_hir::Node::Field(field) => {
                let type_ref = self.mk_type_ref_from(&field.field_type, field.id)?;

                Ok(Some(type_ref))
            }
            _ => panic!("bug!: expression not contained within statement, expression or field"),
        }
    }

    #[cached_query(result)]
    #[tracing::instrument(level = "TRACE", skip(self), err)]
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

        for type_field in self.tdb().find_fields(constructed_type.instance_of) {
            if type_field.name != field.name.name {
                continue;
            }

            return Ok(Some(type_field.field_type.clone()));
        }

        Result::Ok(None)
    }

    #[cached_query(result)]
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    fn expected_type_of_call(&self, expr: NodeId, call: CallExpression<'_>) -> Result<TypeRef> {
        let callable = self.probe_callable(call)?;
        let signature = self.instantiate_signature_from_args(callable, call)?;

        // If the ID refers to the callee of an instance method, return the type
        // expected from the instance method we found.
        if let CallExpression::Instanced(instance_call) = call
            && expr == instance_call.callee
        {
            let callee_type = &signature.params.params.first().unwrap().ty;

            return Ok(callee_type.clone());
        }

        let mut argument_idx = call.find_arg_idx(expr).expect("could not find expression in arg list");

        if let CallExpression::Instanced(_) = call
            && let Callable::Method(method) = callable
            && method.is_instanced()
        {
            argument_idx += 1;
        }

        let parameter = &signature.params.inner()[argument_idx];

        Result::Ok(parameter.ty.clone())
    }

    /// Attempts to guess the expected type of the given expression, based on other statements
    /// and expressions which refer to the target expression.
    #[tracing::instrument(level = "TRACE", skip(self), err)]
    pub fn guess_type_of_ctx(&self, id: NodeId) -> Result<Option<TypeRef>> {
        for expr_node_ref in self.indirect_expression_refs(id)? {
            let Some(expr_node) = self.hir_node(expr_node_ref) else {
                continue;
            };

            match expr_node {
                Node::Statement(stmt) => match &stmt.kind {
                    lume_hir::StatementKind::Variable(decl) => {
                        if let Some(declared_type) = &decl.declared_type {
                            return Ok(Some(self.mk_type_ref_from(declared_type, decl.id)?));
                        } else {
                            continue;
                        }
                    }
                    _ => continue,
                },
                Node::Expression(expr) => {
                    if let Some(expected_type) = self.try_expected_type_of(expr.id)? {
                        return Ok(Some(expected_type));
                    }
                }
                _ => continue,
            }
        }

        Ok(None)
    }

    #[tracing::instrument(level = "TRACE", skip(self), err)]
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

    /// Determines whether all the arms in the given switch expression have a
    /// pattern which refer to constant literals.
    #[cached_query]
    #[tracing::instrument(level = "TRACE", skip(self), ret)]
    pub fn switch_table_const_literal(&self, expr: &lume_hir::Switch) -> bool {
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
}
