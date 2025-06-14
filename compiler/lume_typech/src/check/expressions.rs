use error_snippet::Result;
use lume_types::TypeRef;

use crate::ThirBuildCtx;
use crate::check::{TypeCheckerPass, errors::*};

/// Type checker pass to check whether expressions yield
/// their expected type, depending on the surrounding context.
pub(super) struct Expressions<'a> {
    tcx: &'a ThirBuildCtx,
}

impl TypeCheckerPass for Expressions<'_> {
    #[tracing::instrument(level = "DEBUG", name = "Expressions::run", skip_all, err)]
    fn run(tcx: &mut ThirBuildCtx) -> Result<()> {
        for (_, symbol) in &tcx.hir.items {
            if let Err(err) = (Expressions { tcx }.visit(symbol)) {
                tcx.dcx.emit(err);
            }
        }

        tcx.dcx().drain()
    }
}

impl Expressions<'_> {
    fn visit(&self, symbol: &lume_hir::Item) -> Result<()> {
        match symbol {
            lume_hir::Item::Type(ty) => match &**ty {
                lume_hir::TypeDefinition::Struct(struct_def) => self.define_struct_type(struct_def),
                lume_hir::TypeDefinition::Trait(trait_def) => self.define_trait_type(trait_def),
                _ => Ok(()),
            },
            lume_hir::Item::Function(func) => self.define_function_scope(func),
            _ => Ok(()),
        }
    }

    fn define_struct_type(&self, struct_def: &lume_hir::StructDefinition) -> Result<()> {
        for method in struct_def.methods() {
            if let Some(block) = &method.block {
                self.define_block_scope(block)?;
            }
        }

        Ok(())
    }

    fn define_trait_type(&self, trait_def: &lume_hir::TraitDefinition) -> Result<()> {
        for method in &trait_def.methods {
            if let Some(block) = &method.block {
                self.define_block_scope(block)?;
            }
        }

        Ok(())
    }

    fn define_function_scope(&self, func: &lume_hir::FunctionDefinition) -> Result<()> {
        if let Some(block) = &func.block {
            self.define_block_scope(block)?;
        }

        Ok(())
    }

    fn define_block_scope(&self, block: &lume_hir::Block) -> Result<()> {
        for stmt in &block.statements {
            self.statement(stmt)?;
        }

        Ok(())
    }

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
                        self.expression(expr)?;
                    }

                    self.define_block_scope(&case.block)?;
                }

                Ok(())
            }
            lume_hir::StatementKind::Unless(cond) => {
                for case in &cond.cases {
                    if let Some(expr) = &case.condition {
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

    fn expression(&self, expr: &lume_hir::Expression) -> Result<()> {
        // Even if the expression type is not yet handled, we still
        // need to verify that the expression itself is valid by
        // querying it's resulting type.
        let _ = self.tcx.type_of_expr(expr)?;

        match &expr.kind {
            lume_hir::ExpressionKind::Cast(cast) => self.cast_expression(cast),
            _ => Ok(()),
        }
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
        let source_type = self.tcx.type_of_expr(&expr.source)?;
        let dest_type = self.tcx.mk_type_ref(&expr.target)?;

        let source_named = self.tcx.new_named_type(&source_type)?;
        let dest_named = self.tcx.new_named_type(&dest_type)?;

        let expr_location = &self.tcx.hir_expect_expr(expr.id).location;

        // Resolve the `Cast` type from the type context
        let cast_ref = self.tcx.find_type_ref(&lume_hir::SymbolName::cast())?.unwrap();
        let mut cast_trait = TypeRef::new(cast_ref.instance_of, cast_ref.location.clone());

        cast_trait.type_arguments.push(dest_type);

        if !self.tcx.trait_impl_by(&cast_trait, &source_type)? {
            return Err(UnavailableCast {
                source: expr_location.file.clone(),
                range: expr_location.index.clone(),
                from: source_named,
                to: dest_named,
            }
            .into());
        }

        Ok(())
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
        let value_expr = self.tcx.type_of_expr(&stmt.value)?;
        let resolved_type = self.tcx.type_of_vardecl(stmt)?;

        if !self.tcx.check_type_compatibility(&value_expr, &resolved_type)? {
            let declared_type = stmt.declared_type.clone().unwrap();

            return Err(MismatchedTypes {
                found_loc: stmt.value.location.clone(),
                reason_loc: declared_type.location.clone(),
                expected: self.tcx.new_named_type(&resolved_type)?,
                found: self.tcx.new_named_type(&value_expr)?,
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
        let expected = self.tcx.hir_ctx_return_type(lume_span::DefId::Statement(stmt.id))?;

        let actual = match &stmt.value {
            Some(val) => self.tcx.type_of_expr(val)?,
            None => TypeRef::void(),
        };

        if !self.tcx.check_type_compatibility(&actual, &expected)? {
            let found_loc = match &stmt.value {
                Some(val) => &val.location,
                None => &self.tcx.hir_expect_stmt(stmt.id).location,
            };

            return Err(MismatchedTypes {
                found_loc: found_loc.clone(),
                reason_loc: expected.location.clone(),
                expected: self.tcx.new_named_type(&expected)?,
                found: self.tcx.new_named_type(&actual)?,
            }
            .into());
        }

        Ok(())
    }
}
