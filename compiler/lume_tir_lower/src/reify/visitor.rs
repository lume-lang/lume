use lume_errors::Result;
use lume_tir::*;

pub(crate) trait Visitor {
    fn visit(&mut self, func: &mut Function) -> Result<()> {
        if let Some(block) = &mut func.block {
            self.visit_block(block)?;
        }

        Ok(())
    }

    fn visit_block(&mut self, block: &mut Block) -> Result<()> {
        self.visit_block_inner(block)
    }

    fn visit_block_inner(&mut self, block: &mut Block) -> Result<()> {
        for stmt in &mut block.statements {
            self.visit_statement(stmt)?;
        }

        Ok(())
    }

    fn visit_statement(&mut self, stmt: &mut Statement) -> Result<()> {
        self.visit_statement_inner(stmt)
    }

    fn visit_statement_inner(&mut self, stmt: &mut Statement) -> Result<()> {
        match stmt {
            Statement::Variable(stmt) => self.visit_expression(&mut stmt.value),
            Statement::Final(stmt) => {
                self.visit_expression(&mut stmt.value)?;

                Ok(())
            }
            Statement::Return(stmt) => {
                if let Some(value) = &mut stmt.value {
                    self.visit_expression(value)?;
                }

                Ok(())
            }
            Statement::InfiniteLoop(stmt) => self.visit_block(&mut stmt.block),
            Statement::IteratorLoop(stmt) => {
                self.visit_expression(&mut stmt.collection)?;
                self.visit_block(&mut stmt.block)?;

                Ok(())
            }
            Statement::Expression(expr) => self.visit_expression(expr),
            Statement::Break(_) | Statement::Continue(_) => Ok(()),
        }
    }

    fn visit_expression(&mut self, expr: &mut Expression) -> Result<()> {
        self.visit_expression_inner(expr)
    }

    fn visit_expression_inner(&mut self, expr: &mut Expression) -> Result<()> {
        match &mut expr.kind {
            ExpressionKind::Assignment(expr) => {
                self.visit_expression(&mut expr.target)?;
                self.visit_expression(&mut expr.value)?;

                Ok(())
            }
            ExpressionKind::Binary(expr) => {
                self.visit_expression(&mut expr.lhs)?;
                self.visit_expression(&mut expr.rhs)?;

                Ok(())
            }
            ExpressionKind::Bitcast(expr) => self.visit_expression(&mut expr.source),
            ExpressionKind::Construct(expr) => {
                for field in &mut expr.fields {
                    self.visit_expression(&mut field.value)?;
                }

                Ok(())
            }
            ExpressionKind::Call(expr) => {
                for arg in &mut expr.arguments {
                    self.visit_expression(arg)?;
                }

                Ok(())
            }
            ExpressionKind::If(expr) => {
                for case in &mut expr.cases {
                    if let Some(cond) = &mut case.condition {
                        self.visit_expression(cond)?;
                    }

                    self.visit_block(&mut case.block)?;
                }

                Ok(())
            }
            ExpressionKind::IntrinsicCall(expr) => {
                for arg in &mut expr.arguments {
                    self.visit_expression(arg)?;
                }

                Ok(())
            }
            ExpressionKind::Is(expr) => {
                self.visit_expression(&mut expr.target)?;

                Ok(())
            }
            ExpressionKind::Logical(expr) => {
                self.visit_expression(&mut expr.lhs)?;
                self.visit_expression(&mut expr.rhs)?;

                Ok(())
            }
            ExpressionKind::Switch(expr) => {
                self.visit_expression(&mut expr.operand)?;

                for (_, branch) in &mut expr.entries {
                    self.visit_expression(branch)?;
                }

                Ok(())
            }
            ExpressionKind::Member(expr) => self.visit_expression(&mut expr.callee),
            ExpressionKind::Scope(expr) => {
                for stmt in &mut expr.body {
                    self.visit_statement(stmt)?;
                }

                Ok(())
            }
            ExpressionKind::Literal(_) | ExpressionKind::Variable(_) | ExpressionKind::Variant(_) => Ok(()),
        }
    }
}
