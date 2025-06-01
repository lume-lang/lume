use error_snippet::Result;
use lume_hir::WithTypeParameters;

use crate::ThirBuildCtx;
use crate::check::{ItemScope, TypeCheckerPass, errors::MismatchedTypes};

/// Type checker pass to check whether expressions yield
/// their expected type, depending on the surrounding context.
pub(super) struct Expressions<'a> {
    tcx: &'a mut ThirBuildCtx,
}

impl TypeCheckerPass for Expressions<'_> {
    fn run(tcx: &mut ThirBuildCtx) -> Result<()> {
        let hir = std::mem::take(&mut tcx.hir);

        for (_, symbol) in &hir.items {
            Expressions { tcx }.visit(symbol)?;
        }

        tcx.hir = hir;

        Ok(())
    }
}

impl Expressions<'_> {
    fn visit(&mut self, symbol: &lume_hir::Symbol) -> Result<()> {
        match symbol {
            lume_hir::Symbol::Type(ty) => match &**ty {
                lume_hir::TypeDefinition::Struct(struct_def) => self.define_struct_type(struct_def),
                lume_hir::TypeDefinition::Trait(trait_def) => self.define_trait_type(trait_def),
                _ => Ok(()),
            },
            lume_hir::Symbol::Function(func) => self.define_function_scope(func),
            _ => Ok(()),
        }
    }

    fn define_struct_type(&mut self, struct_def: &lume_hir::StructDefinition) -> Result<()> {
        let type_id = struct_def.type_id.unwrap();

        let mut scope = ItemScope::ty(type_id);
        scope.type_parameters.extend(struct_def.type_params().clone());

        for method in struct_def.methods() {
            self.define_method_scope(method, &scope)?;
        }

        Ok(())
    }

    fn define_method_scope(&mut self, method: &lume_hir::MethodDefinition, scope: &ItemScope) -> Result<()> {
        let method_id = method.method_id.unwrap();

        let mut scope = ItemScope::method(scope, method_id);
        scope.type_parameters.extend(method.type_params().clone());

        if let Some(block) = &method.block {
            self.define_block_scope(block, &scope)?;
        }

        Ok(())
    }

    fn define_trait_type(&mut self, trait_def: &lume_hir::TraitDefinition) -> Result<()> {
        let type_id = trait_def.type_id.unwrap();

        let mut scope = ItemScope::ty(type_id);
        scope.type_parameters.extend(trait_def.type_params().clone());

        for method in &trait_def.methods {
            self.define_trait_method(method, &scope)?;
        }

        Ok(())
    }

    fn define_trait_method(&mut self, method: &lume_hir::TraitMethodDefinition, scope: &ItemScope) -> Result<()> {
        let method_id = method.method_id.unwrap();

        let mut scope = ItemScope::method(scope, method_id);
        scope.type_parameters.extend(method.type_params().clone());

        if let Some(block) = &method.block {
            self.define_block_scope(block, &scope)?;
        }

        Ok(())
    }

    fn define_function_scope(&mut self, func: &lume_hir::FunctionDefinition) -> Result<()> {
        let func_id = func.func_id.unwrap();

        let mut scope = ItemScope::function(func_id);
        scope.type_parameters.extend(func.type_params().clone());

        if let Some(block) = &func.block {
            self.define_block_scope(block, &scope)?;
        }

        Ok(())
    }

    fn define_block_scope(&mut self, block: &lume_hir::Block, scope: &ItemScope) -> Result<()> {
        for stmt in &block.statements {
            self.statement(stmt, scope)?;
        }

        Ok(())
    }

    fn statement(&mut self, stmt: &lume_hir::Statement, scope: &ItemScope) -> Result<()> {
        match &stmt.kind {
            lume_hir::StatementKind::Variable(var) => self.variable_declaration(var, scope),
            _ => Ok(()),
        }
    }

    fn variable_declaration(&mut self, stmt: &lume_hir::VariableDeclaration, scope: &ItemScope) -> Result<()> {
        let value_expr = self.tcx.type_of_expr(&stmt.value)?;

        let resolved_type = if let Some(declared_type) = &stmt.declared_type {
            let type_params = scope.flat_type_params();
            let declared_type_ref = self.tcx.mk_type_ref_generic(declared_type, &type_params)?;

            if !self.tcx.check_type_compatibility(&value_expr, &declared_type_ref)? {
                return Err(MismatchedTypes {
                    source: stmt.value.location.file.clone(),
                    expect_range: stmt.value.location.index.clone(),
                    reason_range: declared_type.location.index.clone(),
                    expected: declared_type_ref.clone(),
                    found: value_expr.clone(),
                }
                .into());
            }

            declared_type_ref
        } else {
            value_expr.clone()
        };

        self.tcx.resolved_stmts.insert(stmt.id, resolved_type);

        Ok(())
    }
}
