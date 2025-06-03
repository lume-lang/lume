use error_snippet::Result;
use lume_types::TypeRef;

use crate::ThirBuildCtx;
use crate::check::{TypeCheckerPass, errors::MismatchedTypes};

/// Type checker pass to check whether expressions yield
/// their expected type, depending on the surrounding context.
pub(super) struct Expressions<'a> {
    tcx: &'a ThirBuildCtx,
}

impl TypeCheckerPass for Expressions<'_> {
    #[tracing::instrument(level = "DEBUG", name = "Expressions::run", skip_all, err)]
    fn run(tcx: &mut ThirBuildCtx) -> Result<()> {
        for (_, symbol) in &tcx.hir.items {
            Expressions { tcx }.visit(symbol)?;
        }

        Ok(())
    }
}

impl Expressions<'_> {
    fn visit(&mut self, symbol: &lume_hir::Item) -> Result<()> {
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

    fn define_struct_type(&mut self, struct_def: &lume_hir::StructDefinition) -> Result<()> {
        for method in struct_def.methods() {
            if let Some(block) = &method.block {
                self.define_block_scope(block)?;
            }
        }

        Ok(())
    }

    fn define_trait_type(&mut self, trait_def: &lume_hir::TraitDefinition) -> Result<()> {
        for method in &trait_def.methods {
            if let Some(block) = &method.block {
                self.define_block_scope(block)?;
            }
        }

        Ok(())
    }

    fn define_function_scope(&mut self, func: &lume_hir::FunctionDefinition) -> Result<()> {
        if let Some(block) = &func.block {
            self.define_block_scope(block)?;
        }

        Ok(())
    }

    fn define_block_scope(&mut self, block: &lume_hir::Block) -> Result<()> {
        for stmt in &block.statements {
            self.statement(stmt)?;
        }

        Ok(())
    }

    fn statement(&mut self, stmt: &lume_hir::Statement) -> Result<()> {
        match &stmt.kind {
            lume_hir::StatementKind::Variable(var) => self.variable_declaration(var),
            lume_hir::StatementKind::Return(ret) => self.return_statement(ret),
            _ => Ok(()),
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
        let value_expr = self.tcx.type_of_expr(&stmt.value)?;
        let resolved_type = self.tcx.type_of_vardecl(stmt)?;

        if !self.tcx.check_type_compatibility(&value_expr, &resolved_type)? {
            let declared_type = stmt.declared_type.clone().unwrap();

            return Err(MismatchedTypes {
                source: stmt.value.location.file.clone(),
                expect_range: stmt.value.location.index.clone(),
                reason_range: declared_type.location.index.clone(),
                expected: self.tcx.new_named_type(&resolved_type)?,
                found: self.tcx.new_named_type(&value_expr)?,
            }
            .into());
        }

        Ok(())
    }

    fn return_statement(&mut self, stmt: &lume_hir::Return) -> Result<()> {
        let expected = self.tcx.hir_ctx_return_type(lume_span::DefId::Statement(stmt.id))?;

        let actual = match &stmt.value {
            Some(val) => self.tcx.type_of_expr(val)?,
            None => TypeRef::void(),
        };

        if !self.tcx.check_type_compatibility(&actual, &expected)? {
            return Err(MismatchedTypes {
                source: expected.location.file.clone(),
                expect_range: actual.location.index.clone(),
                reason_range: expected.location.index.clone(),
                expected: self.tcx.new_named_type(&expected)?,
                found: self.tcx.new_named_type(&actual)?,
            }
            .into());
        }

        Ok(())
    }
}
