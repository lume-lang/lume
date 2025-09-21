use error_snippet::Result;
use indexmap::IndexSet;
use lume_hir::Path;
use lume_span::NodeId;
use lume_types::TypeRef;

use crate::TyCheckCtx;
use crate::check::errors::*;

impl TyCheckCtx {
    /// Type checker pass to check whether expressions yield
    /// their expected type, depending on the surrounding context.
    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    pub(crate) fn typech_expressions(&mut self) -> Result<()> {
        for (_, item) in &self.hir().nodes {
            if let Err(err) = self.typech_expr_item(item) {
                self.dcx().emit(err);
            }
        }

        Ok(())
    }

    fn typech_expr_item(&self, symbol: &lume_hir::Node) -> Result<()> {
        match symbol {
            lume_hir::Node::Type(ty) => match ty {
                lume_hir::TypeDefinition::Struct(struct_def) => self.define_struct_type(struct_def),
                lume_hir::TypeDefinition::Trait(trait_def) => self.define_trait_type(trait_def),
                lume_hir::TypeDefinition::Enum(_) => Ok(()),
            },
            lume_hir::Node::Impl(impl_def) => self.define_impl_type(impl_def),
            lume_hir::Node::Function(func) => self.define_function_scope(func),
            _ => Ok(()),
        }
    }

    fn define_struct_type(&self, struct_def: &lume_hir::StructDefinition) -> Result<()> {
        for field in &struct_def.fields {
            if let Some(default_value) = &field.default_value {
                let field_type = self.mk_type_ref_from(&field.field_type, struct_def.id)?;
                let default_value_type = self.type_of(*default_value)?;

                if let Err(err) = self.ensure_type_compatibility(&default_value_type, &field_type) {
                    self.dcx().emit(err);
                }
            }
        }

        Ok(())
    }

    fn define_trait_type(&self, trait_def: &lume_hir::TraitDefinition) -> Result<()> {
        for method in &trait_def.methods {
            if let Some(block) = &method.block {
                self.define_block_scope(block)?;

                let type_parameters_hir = self.hir_avail_type_params(method.id);
                let type_parameters = type_parameters_hir.iter().map(AsRef::as_ref).collect::<Vec<_>>();

                let return_type = self.mk_type_ref_generic(&method.return_type, &type_parameters)?;

                self.ensure_block_ty_match(block, &return_type)?;
            }
        }

        Ok(())
    }

    fn define_impl_type(&self, impl_def: &lume_hir::Implementation) -> Result<()> {
        for method in &impl_def.methods {
            if let Some(block) = &method.block {
                self.define_block_scope(block)?;

                let type_params_hir = self.hir_avail_type_params(method.id);
                let type_params = type_params_hir.iter().map(AsRef::as_ref).collect::<Vec<_>>();

                let return_type = self.mk_type_ref_generic(&method.return_type, &type_params)?;

                self.ensure_block_ty_match(block, &return_type)?;
            }
        }

        Ok(())
    }

    fn define_function_scope(&self, func: &lume_hir::FunctionDefinition) -> Result<()> {
        if let Some(block) = &func.block {
            self.define_block_scope(block)?;

            let return_type = self.mk_type_ref_from(&func.return_type, func.id)?;

            self.ensure_block_ty_match(block, &return_type)?;
        }

        Ok(())
    }

    fn define_block_scope(&self, block: &lume_hir::Block) -> Result<()> {
        for stmt in &block.statements {
            self.statement(*stmt)?;
        }

        Ok(())
    }

    /// Type checks the given HIR statement.
    fn statement(&self, stmt: NodeId) -> Result<()> {
        let stmt = self.hir().expect_statement(stmt)?;

        match &stmt.kind {
            lume_hir::StatementKind::Variable(var) => {
                self.expression(var.value)?;

                self.variable_declaration(var)
            }
            lume_hir::StatementKind::Final(stmt) => {
                self.expression(stmt.value)?;

                Ok(())
            }
            lume_hir::StatementKind::Return(ret) => {
                if let Some(value) = ret.value {
                    self.expression(value)?;
                }

                self.return_statement(ret)
            }
            lume_hir::StatementKind::InfiniteLoop(stmt) => self.define_block_scope(&stmt.block),
            lume_hir::StatementKind::IteratorLoop(stmt) => {
                self.expression(stmt.collection)?;

                self.define_block_scope(&stmt.block)
            }
            lume_hir::StatementKind::Expression(expr) => self.expression(*expr),
            lume_hir::StatementKind::Break(_) | lume_hir::StatementKind::Continue(_) => Ok(()),
        }
    }

    /// Asserts that the type declared on the given variable declaration matches
    /// the value assigned to it. For instance, given the following statement:
    ///
    /// ```lm
    /// let _: Int32 = false;
    /// ```
    ///
    /// this method would raise an error, since the value expands to be of type `Boolean`,
    /// which is incompatible with `Int32`.
    fn variable_declaration(&self, stmt: &lume_hir::VariableDeclaration) -> Result<()> {
        let value_type = self.type_of(stmt.value)?;
        let resolved_type = self.type_of_vardecl(stmt)?;

        if !self.check_type_compatibility(&value_type, &resolved_type)? {
            let value_expr = self.hir().expect_expression(stmt.value)?;
            let declared_type = stmt.declared_type.clone().unwrap();

            return Err(MismatchedTypes {
                found_loc: value_expr.location,
                reason_loc: declared_type.location,
                expected: self.new_named_type(&resolved_type, false)?,
                found: self.new_named_type(&value_type, false)?,
            }
            .into());
        }

        Ok(())
    }

    /// Asserts that the type returned from a function and/or method is compatible with
    /// the expected type of the context. For instance, given the following statement:
    ///
    /// ```lm
    /// fn foo() -> Int32 {
    ///     return false;
    /// }
    /// ```
    ///
    /// this method would raise an error, since the value expands to be of type `Boolean`,
    /// which is incompatible with `Int32`.
    fn return_statement(&self, stmt: &lume_hir::Return) -> Result<()> {
        let expected = self.hir_ctx_return_type(stmt.id)?;

        let actual = match stmt.value {
            Some(val) => self.type_of(val)?,
            None => TypeRef::void(),
        };

        if !self.check_type_compatibility(&actual, &expected)? {
            let found_loc = match stmt.value {
                Some(val) => self.hir().expect_expression(val)?.location,
                None => self.hir_expect_stmt(stmt.id).location,
            };

            return Err(MismatchedTypes {
                found_loc,
                reason_loc: expected.location,
                expected: self.new_named_type(&expected, false)?,
                found: self.new_named_type(&actual, false)?,
            }
            .into());
        }

        Ok(())
    }

    /// Type checks the given HIR expression.
    fn expression(&self, expr: NodeId) -> Result<()> {
        let expr = self.hir().expect_expression(expr)?;

        // Even if the expression type is not yet handled, we still
        // need to verify that the expression itself is valid by
        // querying it's resulting type.
        let _ = self.type_of_expr(expr)?;

        match &expr.kind {
            lume_hir::ExpressionKind::Assignment(expr) => {
                self.expression(expr.target)?;
                self.expression(expr.value)?;

                self.assignment_expression(expr)
            }
            lume_hir::ExpressionKind::Binary(expr) => {
                self.expression(expr.lhs)?;
                self.expression(expr.rhs)?;

                self.binary_expression(expr)
            }
            lume_hir::ExpressionKind::Cast(cast) => {
                self.expression(cast.source)?;

                self.cast_expression(cast)
            }
            lume_hir::ExpressionKind::Construct(expr) => {
                for field in &expr.fields {
                    self.expression(field.value)?;
                }

                self.construct_expression(expr)
            }
            lume_hir::ExpressionKind::StaticCall(call) => {
                for arg in &call.arguments {
                    self.expression(*arg)?;
                }

                self.call_expression(lume_hir::CallExpression::Static(call))
            }
            lume_hir::ExpressionKind::InstanceCall(call) => {
                for arg in &call.arguments {
                    self.expression(*arg)?;
                }

                self.call_expression(lume_hir::CallExpression::Instanced(call))
            }
            lume_hir::ExpressionKind::IntrinsicCall(call) => {
                for arg in &call.arguments {
                    self.expression(*arg)?;
                }

                self.call_expression(lume_hir::CallExpression::Intrinsic(call))
            }
            lume_hir::ExpressionKind::If(cond) => {
                for case in &cond.cases {
                    if let Some(expr) = &case.condition {
                        let ty = self.type_of(*expr)?;

                        if !self.check_type_compatibility(&ty, &TypeRef::bool())? {
                            let hir_expr = self.hir().expect_expression(*expr)?;

                            return Err(super::errors::MismatchedTypesBoolean {
                                source: hir_expr.location,
                                found: self.new_named_type(&ty, false)?,
                                expected: self.new_named_type(&TypeRef::bool(), false)?,
                            }
                            .into());
                        }

                        self.expression(*expr)?;
                    }

                    self.define_block_scope(&case.block)?;
                }

                self.matching_type_of_cond(cond.id, &cond.cases)?;

                Ok(())
            }
            lume_hir::ExpressionKind::Is(is) => {
                self.expression(is.target)?;

                self.is_expression(is)
            }
            lume_hir::ExpressionKind::Logical(expr) => {
                self.expression(expr.lhs)?;
                self.expression(expr.rhs)?;

                self.logical_expression(expr)
            }
            lume_hir::ExpressionKind::Member(expr) => self.expression(expr.callee),
            lume_hir::ExpressionKind::Scope(expr) => {
                for stmt in &expr.body {
                    self.statement(*stmt)?;
                }

                Ok(())
            }
            lume_hir::ExpressionKind::Switch(expr) => {
                for case in &expr.cases {
                    self.expression(case.branch)?;
                }

                self.switch_expression(expr)
            }
            lume_hir::ExpressionKind::Variant(expr) => {
                for arg in &expr.arguments {
                    self.expression(*arg)?;
                }

                self.variant_expression(expr)
            }
            lume_hir::ExpressionKind::Literal(_)
            | lume_hir::ExpressionKind::Field(_)
            | lume_hir::ExpressionKind::Variable(_) => Ok(()),
        }
    }

    /// Asserts that an assignment expression assigns a value which is valid for the
    /// target expression. For instance, given the following statement:
    ///
    /// ```lm
    /// let a: Boolean = false;
    ///
    /// a = 16;
    /// ```
    ///
    /// this method would raise an error, since `a` is a type of `Boolean` which cannot
    /// be assigned a value other than `Boolean`.
    fn assignment_expression(&self, expr: &lume_hir::Assignment) -> Result<()> {
        let target = self.type_of(expr.target)?;
        let value = self.type_of(expr.value)?;

        let target_expr = self.hir().expect_expression(expr.target)?;

        if matches!(target_expr.kind, lume_hir::ExpressionKind::Literal(_)) {
            return Err(LiteralAssignment { source: expr.location }.into());
        }

        if !self.check_type_compatibility(&value, &target)? {
            let value_expr = self.hir().expect_expression(expr.value)?;

            return Err(NonMatchingAssignment {
                source: expr.location,
                target_loc: target_expr.location,
                value_loc: value_expr.location,
                target_ty: self.new_named_type(&target, false)?.to_string(),
                value_ty: self.new_named_type(&value, false)?.to_string(),
            }
            .into());
        }

        Ok(())
    }

    /// Asserts that the binary expression is performed on values, which actually
    /// support binary expressions. For instance, given the following statement:
    ///
    /// ```lm
    /// let _ = "Hello, world!" ^ 16;
    /// ```
    ///
    /// this method would raise an error, since [`String`] is not an integer type,
    /// making binary expressions invalid.
    fn binary_expression(&self, expr: &lume_hir::Binary) -> Result<()> {
        let lhs = self.type_of(expr.lhs)?;
        let rhs = self.type_of(expr.rhs)?;

        // NOTE: We're checking for equality - not compatibility.
        //       These types MUST be the same.
        if lhs != rhs {
            return Err(NonMatchingBinaryOp {
                source: expr.location,
                lhs: lhs.location,
                rhs: rhs.location,
                lhs_ty: self.new_named_type(&lhs, false)?.to_string(),
                rhs_ty: self.new_named_type(&rhs, false)?.to_string(),
            }
            .into());
        }

        Ok(())
    }

    /// Asserts that the type left-hand side of a casting expression is
    /// valid to be cast to the right-hand-side. For instance, given the following statement:
    ///
    /// ```lm
    /// let _ = "Hello, world!" as Boolean;
    /// ```
    ///
    /// this method would raise an error, since there exists no implementation of [`Cast<Boolean>`]
    /// on the [`String`] type.
    fn cast_expression(&self, expr: &lume_hir::Cast) -> Result<()> {
        let source_type = self.type_of(expr.source)?;
        let dest_type = self.mk_type_ref(&expr.target)?;

        let source_named = self.new_named_type(&source_type, false)?;
        let dest_named = self.new_named_type(&dest_type, false)?;

        let expr_location = self.hir_expect_expr(expr.id).location;

        // Resolve the `Cast` type from the type context
        let cast_ref = self.find_type_ref(&Path::cast())?.unwrap();
        let mut cast_trait = TypeRef::new(cast_ref.instance_of, cast_ref.location);

        cast_trait.type_arguments.push(dest_type);

        if !self.trait_impl_by(&cast_trait, &source_type)? {
            return Err(UnavailableCast {
                source: expr_location,
                from: source_named,
                to: dest_named,
            }
            .into());
        }

        Ok(())
    }

    /// Asserts that the resolved function/method which is invoked by the expression
    /// is valid for the given context it is in. This includes whether the method exists,
    /// takes the correct amount of parameters, as well as their parameter types.
    fn call_expression(&self, expr: lume_hir::CallExpression) -> Result<()> {
        let callable = self.lookup_callable(expr)?;

        if let lume_infer::query::Callable::Method(method) = callable
            && method.kind == lume_types::MethodKind::TraitDefinition
            && self.hir_body_of_node(method.id).is_none()
            && !method.is_instanced()
        {
            self.dcx().emit(
                DispatchCannotBeInferred {
                    source: expr.location(),
                }
                .into(),
            );
        }

        Ok(())
    }

    /// Asserts that the given construction expression fulfills the requirements by
    /// the declared type which is being constructed, such as all fields being initialized,
    /// types being compatible, etc. For instance, given the following statement:
    ///
    /// ```lm
    /// struct Foo {
    ///     bar: Int32,
    /// }
    ///
    /// let _: Foo = Foo {
    ///     bar: "Hello, world!",
    /// };
    /// ```
    ///
    /// this method would raise an error, since the field `bar` expands to be of type `String`,
    /// which is incompatible with `Int32`.
    fn construct_expression(&self, expr: &lume_hir::Construct) -> Result<()> {
        let constructed_type = self.find_type_ref_from(&expr.path, expr.id)?.unwrap();

        let fields = self.tdb().find_fields(constructed_type.instance_of);
        let mut fields_left = expr.fields.iter().map(|field| &field.name).collect::<IndexSet<_>>();

        for field in fields {
            let Some(constructor_field) = self.constructer_field_of(expr, &field.name) else {
                self.dcx().emit(
                    MissingField {
                        source: expr.location,
                        field: field.name.clone(),
                    }
                    .into(),
                );

                continue;
            };

            let prop_ty = &field.field_type;
            let field_ty = self.type_of(constructor_field.value)?;

            if let Err(err) = self.ensure_type_compatibility(&field_ty, prop_ty) {
                self.dcx().emit(err);
            }

            fields_left.swap_remove(&constructor_field.name);
        }

        for field_left in fields_left {
            self.dcx().emit(
                UnknownField {
                    source: field_left.location,
                    ty: self.new_named_type(&constructed_type, false)?,
                    field: field_left.to_string(),
                }
                .into(),
            );
        }

        Ok(())
    }

    /// Asserts that the logical expression is performed on boolean values, since
    /// only boolean values can be tested in logical expressions.
    #[tracing::instrument(level = "TRACE", skip_all, err)]
    fn is_expression(&self, expr: &lume_hir::Is) -> Result<()> {
        let target_ty = self.type_of(expr.target)?;
        let pattern_ty = self.type_of_pattern(&expr.pattern)?;

        if let Err(err) = self.ensure_type_compatibility(&target_ty, &pattern_ty) {
            self.dcx().emit(err);
        }

        Ok(())
    }

    /// Asserts that the logical expression is performed on boolean values, since
    /// only boolean values can be tested in logical expressions.
    #[tracing::instrument(level = "TRACE", skip_all, err)]
    fn logical_expression(&self, expr: &lume_hir::Logical) -> Result<()> {
        let lhs = self.type_of(expr.lhs)?;
        let rhs = self.type_of(expr.rhs)?;

        if self.type_ref_name(&lhs)? != &Path::boolean() {
            let lhs_expr = self.hir().expect_expression(expr.lhs)?;

            return Err(BooleanOperationOnNonBoolean {
                source: lhs_expr.location,
                expected: Path::boolean().to_string(),
                found: self.new_named_type(&lhs, false)?.to_string(),
            }
            .into());
        }

        if self.type_ref_name(&rhs)? != &Path::boolean() {
            let rhs_expr = self.hir().expect_expression(expr.rhs)?;

            return Err(BooleanOperationOnNonBoolean {
                source: rhs_expr.location,
                expected: Path::boolean().to_string(),
                found: self.new_named_type(&rhs, false)?.to_string(),
            }
            .into());
        }

        Ok(())
    }

    /// Asserts that the patterns in the switch expression are valid for the operand,
    /// as well as checking that all branch expressions are compatible.
    #[tracing::instrument(level = "TRACE", skip_all, err)]
    fn switch_expression(&self, expr: &lume_hir::Switch) -> Result<()> {
        let Some(first_case) = expr.cases.first() else {
            return Ok(());
        };

        let operand_ty = self.type_of(expr.operand)?;
        let branch_ty = self.type_of(first_case.branch)?;

        for case in &expr.cases {
            let case_branch_ty = self.type_of(case.branch)?;
            let case_pattern_ty = self.type_of_pattern(&case.pattern)?;

            if let Err(err) = self.ensure_type_compatibility(&case_branch_ty, &branch_ty) {
                self.dcx().emit(err);
            }

            if let Err(err) = self.ensure_type_compatibility(&case_pattern_ty, &operand_ty) {
                self.dcx().emit(err);
            }
        }

        Ok(())
    }

    /// Asserts that the variant exists on the enum type, as well as asserting
    /// the types of passed parameters, if any.
    #[tracing::instrument(level = "TRACE", skip_all, err)]
    fn variant_expression(&self, expr: &lume_hir::Variant) -> Result<()> {
        let enum_def = self.enum_def_of_name(&expr.name.clone().parent().unwrap())?;
        let enum_case_def = self.enum_case_with_name(&expr.name)?;

        if expr.arguments.len() != enum_case_def.parameters.len() {
            return Err(crate::query::diagnostics::ArgumentCountMismatch {
                source: expr.location,
                expected: enum_case_def.parameters.len(),
                actual: expr.arguments.len(),
            }
            .into());
        }

        for (arg, param) in expr.arguments.iter().zip(enum_case_def.parameters.iter()) {
            let arg_type = self.type_of(*arg)?;
            let param_ty = self.mk_type_ref_from(param, enum_def.id)?;

            if let Err(err) = self.ensure_type_compatibility(&arg_type, &param_ty) {
                self.dcx().emit(err);
            }
        }

        Ok(())
    }
}
