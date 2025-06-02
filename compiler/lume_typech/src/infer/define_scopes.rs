use std::collections::BTreeMap;

use error_snippet::Result;
use lume_hir::{self};
use lume_span::DefId;

use crate::ThirBuildCtx;

pub(super) struct DefineScopes<'a> {
    ancestry: &'a mut BTreeMap<DefId, DefId>,
}

impl DefineScopes<'_> {
    pub(super) fn run_all(tcx: &mut ThirBuildCtx) -> Result<()> {
        let mut define = DefineScopes {
            ancestry: &mut tcx.ancestry,
        };

        for (_, symbol) in &tcx.hir.items {
            match symbol {
                lume_hir::Item::Type(ty) => match ty.as_ref() {
                    lume_hir::TypeDefinition::Struct(f) => define.define_struct_scope(f)?,
                    lume_hir::TypeDefinition::Trait(f) => define.define_trait_scope(f)?,
                    _ => (),
                },
                lume_hir::Item::Impl(f) => define.define_impl_scope(f)?,
                lume_hir::Item::Use(f) => define.define_use_scope(f)?,
                lume_hir::Item::Function(f) => define.define_function_scope(f)?,
                _ => (),
            }
        }

        Ok(())
    }

    fn define_struct_scope(&mut self, struct_def: &lume_hir::StructDefinition) -> Result<()> {
        let parent = DefId::Item(struct_def.id);

        for property in &struct_def.properties {
            if let Some(default) = &property.default_value {
                let item_id = DefId::Item(property.id);
                let _ = self.ancestry.try_insert(item_id, parent);

                self.define_expr_scope(default, item_id)?;
            }
        }

        Ok(())
    }

    fn define_trait_scope(&mut self, trait_def: &lume_hir::TraitDefinition) -> Result<()> {
        let parent = DefId::Item(trait_def.id);

        for method in &trait_def.methods {
            if let Some(block) = &method.block {
                let item_id = DefId::Item(method.id);
                let _ = self.ancestry.try_insert(item_id, parent);

                self.define_block_scope(block, item_id)?;
            }
        }

        Ok(())
    }

    fn define_impl_scope(&mut self, implementation: &lume_hir::Implementation) -> Result<()> {
        let parent = DefId::Item(implementation.id);

        for method in &implementation.methods {
            if let Some(block) = &method.block {
                let item_id = DefId::Item(method.id);
                let _ = self.ancestry.try_insert(item_id, parent);

                self.define_block_scope(block, item_id)?;
            }
        }

        Ok(())
    }

    fn define_use_scope(&mut self, trait_impl: &lume_hir::TraitImplementation) -> Result<()> {
        let parent = DefId::Item(trait_impl.id);

        for method in &trait_impl.methods {
            let item_id = DefId::Item(method.id);
            let _ = self.ancestry.try_insert(item_id, parent);

            self.define_block_scope(&method.block, item_id)?;
        }

        Ok(())
    }

    fn define_function_scope(&mut self, func: &lume_hir::FunctionDefinition) -> Result<()> {
        if let Some(block) = &func.block {
            let parent = DefId::Item(func.id);

            self.define_block_scope(block, parent)?;
        }

        Ok(())
    }

    fn define_block_scope(&mut self, block: &lume_hir::Block, parent: DefId) -> Result<()> {
        for stmt in &block.statements {
            self.define_stmt_scope(stmt, parent)?;
        }

        Ok(())
    }

    fn define_stmt_scope(&mut self, stmt: &lume_hir::Statement, parent: DefId) -> Result<()> {
        let stmt_id = DefId::Statement(stmt.id);
        let _ = self.ancestry.try_insert(stmt_id, parent);

        match &stmt.kind {
            lume_hir::StatementKind::Variable(s) => self.define_expr_scope(&s.value, stmt_id),
            lume_hir::StatementKind::Break(_) | lume_hir::StatementKind::Continue(_) => Ok(()),
            lume_hir::StatementKind::Return(s) => {
                if let Some(value) = &s.value {
                    self.define_expr_scope(value, stmt_id)
                } else {
                    Ok(())
                }
            }
            lume_hir::StatementKind::If(s) => {
                for case in &s.cases {
                    self.define_condition_scope(case, stmt_id)?;
                }

                Ok(())
            }
            lume_hir::StatementKind::Unless(s) => {
                for case in &s.cases {
                    self.define_condition_scope(case, stmt_id)?;
                }

                Ok(())
            }
            lume_hir::StatementKind::InfiniteLoop(s) => self.define_block_scope(&s.block, stmt_id),
            lume_hir::StatementKind::IteratorLoop(s) => {
                self.define_block_scope(&s.block, stmt_id)?;
                self.define_expr_scope(&s.collection, stmt_id)?;

                Ok(())
            }
            lume_hir::StatementKind::PredicateLoop(s) => {
                self.define_block_scope(&s.block, stmt_id)?;
                self.define_expr_scope(&s.condition, stmt_id)?;

                Ok(())
            }
            lume_hir::StatementKind::Expression(s) => self.define_expr_scope(s, stmt_id),
        }
    }

    fn define_condition_scope(&mut self, cond: &lume_hir::Condition, parent: DefId) -> Result<()> {
        let stmt_id = DefId::Statement(cond.id);
        let _ = self.ancestry.try_insert(stmt_id, parent);

        if let Some(condition) = &cond.condition {
            self.define_expr_scope(condition, stmt_id)?;
        }

        self.define_block_scope(&cond.block, stmt_id)?;

        Ok(())
    }

    fn define_expr_scope(&mut self, expr: &lume_hir::Expression, parent: DefId) -> Result<()> {
        let expr_id = DefId::Expression(expr.id);
        let _ = self.ancestry.try_insert(expr_id, parent);

        match &expr.kind {
            lume_hir::ExpressionKind::Assignment(s) => {
                self.define_expr_scope(&s.target, expr_id)?;
                self.define_expr_scope(&s.value, expr_id)?;

                Ok(())
            }
            lume_hir::ExpressionKind::Cast(s) => self.define_expr_scope(&s.source, expr_id),
            lume_hir::ExpressionKind::InstanceCall(s) => {
                self.define_expr_scope(&s.callee, expr_id)?;

                for arg in &s.arguments {
                    self.define_expr_scope(arg, expr_id)?;
                }

                Ok(())
            }
            lume_hir::ExpressionKind::Member(s) => self.define_expr_scope(&s.callee, expr_id),
            lume_hir::ExpressionKind::StaticCall(s) => {
                for arg in &s.arguments {
                    self.define_expr_scope(arg, expr_id)?;
                }

                Ok(())
            }
            lume_hir::ExpressionKind::Literal(_) | lume_hir::ExpressionKind::Variable(_) => Ok(()),
        }
    }
}
