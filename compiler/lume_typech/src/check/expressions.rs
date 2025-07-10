use error_snippet::Result;
use indexmap::IndexSet;
use lume_hir::Path;
use lume_types::TypeRef;

use crate::TyCheckCtx;
use crate::check::errors::*;

impl TyCheckCtx {
    /// Type checker pass to check whether expressions yield
    /// their expected type, depending on the surrounding context.
    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    pub(crate) fn typech_expressions(&mut self) -> Result<()> {
        for (_, item) in &self.hir().items {
            if let Err(err) = self.typech_expr_item(item) {
                self.dcx().emit(err);
            }
        }

        self.dcx().drain()
    }

    fn typech_expr_item(&self, symbol: &lume_hir::Item) -> Result<()> {
        match symbol {
            lume_hir::Item::Type(ty) => match &**ty {
                lume_hir::TypeDefinition::Trait(trait_def) => self.define_trait_type(trait_def),
                _ => Ok(()),
            },
            lume_hir::Item::Impl(impl_def) => self.define_impl_type(impl_def),
            lume_hir::Item::Function(func) => self.define_function_scope(func),
            lume_hir::Item::Use(_) => Ok(()),
        }
    }

    fn define_trait_type(&self, trait_def: &lume_hir::TraitDefinition) -> Result<()> {
        for method in &trait_def.methods {
            if let Some(block) = &method.block {
                self.define_block_scope(block)?;

                let type_params = self.hir_avail_type_params(method.id);
                let return_type = self.mk_type_ref_generic(&method.return_type, &type_params)?;

                self.ensure_block_ty_match(block, &return_type)?;
            }
        }

        Ok(())
    }

    fn define_impl_type(&self, impl_def: &lume_hir::Implementation) -> Result<()> {
        for method in &impl_def.methods {
            if let Some(block) = &method.block {
                self.define_block_scope(block)?;

                let type_params = self.hir_avail_type_params(method.id);
                let return_type = self.mk_type_ref_generic(&method.return_type, &type_params)?;

                self.ensure_block_ty_match(block, &return_type)?;
            }
        }

        Ok(())
    }

    fn define_function_scope(&self, func: &lume_hir::FunctionDefinition) -> Result<()> {
        if let Some(block) = &func.block {
            self.define_block_scope(block)?;

            let type_params = self.hir_avail_type_params_item(func.id);
            let return_type = self.mk_type_ref_generic(&func.return_type, &type_params)?;

            self.ensure_block_ty_match(block, &return_type)?;
        }

        Ok(())
    }

    fn define_block_scope(&self, block: &lume_hir::Block) -> Result<()> {
        for stmt in &block.statements {
            self.statement(stmt)?;
        }

        Ok(())
    }

    /// Type checks the given HIR statement.
    fn statement(&self, stmt: &lume_hir::Statement) -> Result<()> {
        match &stmt.kind {
            lume_hir::StatementKind::Variable(var) => {
                self.expression(&var.value)?;

                self.variable_declaration(var)
            }
            lume_hir::StatementKind::Return(ret) => {
                if let Some(value) = &ret.value {
                    self.expression(value)?;
                }

                self.return_statement(ret)
            }
            lume_hir::StatementKind::If(cond) => {
                for case in &cond.cases {
                    if let Some(expr) = &case.condition {
                        let ty = self.type_of_expr(expr)?;

                        if !self.check_type_compatibility(&ty, &TypeRef::bool())? {
                            return Err(super::errors::MismatchedTypesBoolean {
                                source: expr.location,
                                found: self.new_named_type(&ty)?,
                                expected: self.new_named_type(&TypeRef::bool())?,
                            }
                            .into());
                        }

                        self.expression(expr)?;
                    }

                    self.define_block_scope(&case.block)?;
                }

                Ok(())
            }
            lume_hir::StatementKind::InfiniteLoop(stmt) => self.define_block_scope(&stmt.block),
            lume_hir::StatementKind::IteratorLoop(stmt) => {
                self.expression(&stmt.collection)?;

                self.define_block_scope(&stmt.block)
            }
            lume_hir::StatementKind::PredicateLoop(stmt) => {
                self.expression(&stmt.condition)?;

                self.define_block_scope(&stmt.block)
            }
            lume_hir::StatementKind::Expression(expr) => self.expression(expr),
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
        let value_expr = self.type_of_expr(&stmt.value)?;
        let resolved_type = self.type_of_vardecl(stmt)?;

        if !self.check_type_compatibility(&value_expr, &resolved_type)? {
            let declared_type = stmt.declared_type.clone().unwrap();

            return Err(MismatchedTypes {
                found_loc: stmt.value.location,
                reason_loc: declared_type.location,
                expected: self.new_named_type(&resolved_type)?,
                found: self.new_named_type(&value_expr)?,
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
        let expected = self.hir_ctx_return_type(lume_span::DefId::Statement(stmt.id))?;

        let actual = match &stmt.value {
            Some(val) => self.type_of_expr(val)?,
            None => TypeRef::void(),
        };

        if !self.check_type_compatibility(&actual, &expected)? {
            let found_loc = match &stmt.value {
                Some(val) => val.location,
                None => self.hir_expect_stmt(stmt.id).location,
            };

            return Err(MismatchedTypes {
                found_loc,
                reason_loc: expected.location,
                expected: self.new_named_type(&expected)?,
                found: self.new_named_type(&actual)?,
            }
            .into());
        }

        Ok(())
    }

    /// Type checks the given HIR expression.
    fn expression(&self, expr: &lume_hir::Expression) -> Result<()> {
        // Even if the expression type is not yet handled, we still
        // need to verify that the expression itself is valid by
        // querying it's resulting type.
        let _ = self.type_of_expr(expr)?;

        match &expr.kind {
            lume_hir::ExpressionKind::Binary(expr) => self.binary_expression(expr),
            lume_hir::ExpressionKind::Cast(cast) => self.cast_expression(cast),
            lume_hir::ExpressionKind::Construct(expr) => self.construct_expression(expr),
            lume_hir::ExpressionKind::InstanceCall(call) => {
                self.call_expression(lume_hir::CallExpression::Instanced(call))
            }
            lume_hir::ExpressionKind::StaticCall(call) => self.call_expression(lume_hir::CallExpression::Static(call)),
            lume_hir::ExpressionKind::Logical(expr) => self.logical_expression(expr),
            _ => Ok(()),
        }
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
        let lhs = self.type_of_expr(&expr.lhs)?;
        let rhs = self.type_of_expr(&expr.rhs)?;

        // NOTE: We're checking for equality - not compatibility.
        //       These types MUST be the same.
        if lhs != rhs {
            return Err(NonMatchingBinaryOp {
                source: expr.location,
                lhs: lhs.location,
                rhs: rhs.location,
                lhs_ty: self.new_named_type(&lhs)?.to_string(),
                rhs_ty: self.new_named_type(&rhs)?.to_string(),
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
        let source_type = self.type_of_expr(&expr.source)?;
        let dest_type = self.mk_type_ref(&expr.target)?;

        let source_named = self.new_named_type(&source_type)?;
        let dest_named = self.new_named_type(&dest_type)?;

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
        let _ = self.lookup_callable(expr)?;

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
        let constructed_type = self.find_type_ref(&expr.path)?.unwrap();
        let properties = self.tdb().find_properties(constructed_type.instance_of);

        let mut fields_left = expr.fields.iter().map(|field| &field.name).collect::<IndexSet<_>>();

        for property in properties {
            let Some(field) = expr.fields.iter().find(|field| field.name.as_str() == property.name) else {
                self.dcx().emit(
                    MissingPropertyField {
                        source: expr.location,
                        field: property.name.clone(),
                    }
                    .into(),
                );

                continue;
            };

            let prop_ty = &property.property_type;
            let field_ty = self.type_of_expr(&field.value)?;

            if let Err(err) = self.ensure_type_compatibility(&field_ty, prop_ty) {
                self.dcx().emit(err);
            }

            fields_left.swap_remove(&field.name);
        }

        for field_left in fields_left {
            self.dcx().emit(
                UnknownPropertyField {
                    source: field_left.location,
                    ty: self.new_named_type(&constructed_type)?,
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
    fn logical_expression(&self, expr: &lume_hir::Logical) -> Result<()> {
        let lhs = self.type_of_expr(&expr.lhs)?;
        let rhs = self.type_of_expr(&expr.rhs)?;

        if self.type_ref_name(&lhs)? != &Path::boolean() {
            return Err(BooleanOperationOnNonBoolean {
                source: expr.lhs.location,
                expected: Path::boolean().to_string(),
                found: self.new_named_type(&lhs)?.to_string(),
            }
            .into());
        }

        if self.type_ref_name(&rhs)? != &Path::boolean() {
            return Err(BooleanOperationOnNonBoolean {
                source: expr.rhs.location,
                expected: Path::boolean().to_string(),
                found: self.new_named_type(&rhs)?.to_string(),
            }
            .into());
        }

        Ok(())
    }
}
