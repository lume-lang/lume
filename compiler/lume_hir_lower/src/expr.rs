use error_snippet::Result;

use crate::LowerModule;
use crate::err;
use crate::errors::*;
use crate::{ARRAY_STD_TYPE, ARRAY_WITH_CAPACITY_FUNC, RANGE_INCLUSIVE_STD_TYPE, RANGE_NEW_FUNC, RANGE_STD_TYPE};

use lume_ast::{self as ast, Node};
use lume_hir::{self as hir};

impl LowerModule<'_> {
    #[tracing::instrument(level = "DEBUG", skip_all)]
    pub(super) fn expressions(&mut self, expressions: Vec<ast::Expression>) -> Vec<hir::Expression> {
        expressions
            .into_iter()
            .filter_map(|expr| match self.expression(expr) {
                Ok(e) => Some(e),
                Err(err) => {
                    self.dcx.emit(err);
                    None
                }
            })
            .collect::<Vec<_>>()
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    pub(super) fn expression(&mut self, statement: ast::Expression) -> Result<hir::Expression> {
        let expr = match statement {
            ast::Expression::Array(e) => self.expr_array(*e)?,
            ast::Expression::Assignment(e) => self.expr_assignment(*e)?,
            ast::Expression::Binary(e) => self.expr_binary(*e)?,
            ast::Expression::Call(e) => self.expr_call(*e)?,
            ast::Expression::Cast(e) => self.expr_cast(*e)?,
            ast::Expression::Literal(e) => self.expr_literal(*e),
            ast::Expression::Logical(e) => self.expr_logical(*e)?,
            ast::Expression::Member(e) => self.expr_member(*e)?,
            ast::Expression::Range(e) => self.expr_range(*e)?,
            ast::Expression::Variable(e) => self.expr_variable(*e)?,
        };

        self.map.expressions.insert(expr.id, expr.clone());

        Ok(expr)
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    pub(super) fn opt_expression(&mut self, statement: Option<ast::Expression>) -> Result<Option<hir::Expression>> {
        match statement {
            Some(expr) => Ok(Some(self.expression(expr)?)),
            None => Ok(None),
        }
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn expr_array(&mut self, expr: ast::Array) -> Result<hir::Expression> {
        // TODO: Implement proper array expression lowering
        let array_path = lume_ast::Path::with_root(vec!["std", ARRAY_STD_TYPE], ARRAY_WITH_CAPACITY_FUNC);

        let id = self.next_expr_id();
        let location = self.location(expr.location);

        Ok(hir::Expression {
            id,
            location: location.clone(),
            kind: hir::ExpressionKind::StaticCall(Box::new(hir::StaticCall {
                id,
                name: self.resolve_symbol_name(&array_path)?,
                type_arguments: vec![hir::TypeArgument::Implicit {
                    location: lume_span::Location::empty(),
                }],
                arguments: vec![hir::Expression {
                    id: self.next_expr_id(),
                    kind: hir::ExpressionKind::Literal(Box::new(hir::Literal {
                        id: self.next_expr_id(),
                        kind: hir::LiteralKind::Int(Box::new(hir::IntLiteral {
                            id: self.next_expr_id(),
                            value: usize::cast_signed(expr.values.len()) as i64,
                            kind: hir::IntKind::U64,
                        })),
                        location: lume_span::Location::empty(),
                    })),
                    location: lume_span::Location::empty(),
                }],
            })),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn expr_assignment(&mut self, expr: ast::Assignment) -> Result<hir::Expression> {
        let id = self.next_expr_id();
        let location = self.location(expr.location);
        let target = self.expression(expr.target)?;
        let value = self.expression(expr.value)?;

        Ok(hir::Expression {
            id,
            location,
            kind: hir::ExpressionKind::Assignment(Box::new(hir::Assignment { id, target, value })),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn expr_binary(&mut self, expr: ast::Binary) -> Result<hir::Expression> {
        let id = self.next_expr_id();
        let location = self.location(expr.location);
        let lhs = self.expression(expr.lhs)?;
        let rhs = self.expression(expr.rhs)?;

        let operator_kind = match expr.op.kind {
            ast::BinaryOperatorKind::And => hir::BinaryOperatorKind::And,
            ast::BinaryOperatorKind::Or => hir::BinaryOperatorKind::Or,
            ast::BinaryOperatorKind::Xor => hir::BinaryOperatorKind::Xor,
        };

        let operator_loc = self.location(expr.op.location);

        Ok(hir::Expression {
            id,
            location: location.clone(),
            kind: hir::ExpressionKind::Binary(Box::new(hir::Binary {
                id,
                lhs,
                op: hir::BinaryOperator {
                    kind: operator_kind,
                    location: operator_loc,
                },
                rhs,
                location,
            })),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn expr_call(&mut self, expr: ast::Call) -> Result<hir::Expression> {
        let id = self.next_expr_id();
        let name = self.resolve_symbol_name(&expr.name)?;
        let type_arguments = self.type_arguments(expr.type_arguments)?;
        let arguments = self.expressions(expr.arguments);
        let location = self.location(expr.location);

        let kind = if let Some(callee) = expr.callee {
            let callee = self.expression(callee)?;

            hir::ExpressionKind::InstanceCall(Box::new(hir::InstanceCall {
                id,
                callee,
                name: name.name,
                type_arguments,
                arguments,
            }))
        } else {
            hir::ExpressionKind::StaticCall(Box::new(hir::StaticCall {
                id,
                name,
                type_arguments,
                arguments,
            }))
        };

        Ok(hir::Expression { id, location, kind })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn expr_cast(&mut self, expr: ast::Cast) -> Result<hir::Expression> {
        let id = self.next_expr_id();
        let source = self.expression(expr.source)?;
        let target = self.type_ref(expr.target_type)?;
        let location = self.location(expr.location);

        Ok(hir::Expression {
            id,
            location,
            kind: hir::ExpressionKind::Cast(Box::new(hir::Cast { id, source, target })),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn expr_literal(&mut self, expr: ast::Literal) -> hir::Expression {
        let literal = self.literal(expr);

        hir::Expression {
            id: literal.id,
            location: literal.location.clone(),
            kind: hir::ExpressionKind::Literal(Box::new(literal)),
        }
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn expr_logical(&mut self, expr: ast::Logical) -> Result<hir::Expression> {
        let id = self.next_expr_id();
        let location = self.location(expr.location);
        let lhs = self.expression(expr.lhs)?;
        let rhs = self.expression(expr.rhs)?;

        let operator_kind = match expr.op.kind {
            ast::LogicalOperatorKind::And => hir::LogicalOperatorKind::And,
            ast::LogicalOperatorKind::Or => hir::LogicalOperatorKind::Or,
        };

        let operator_loc = self.location(expr.op.location);

        Ok(hir::Expression {
            id,
            location: location.clone(),
            kind: hir::ExpressionKind::Logical(Box::new(hir::Logical {
                id,
                lhs,
                op: hir::LogicalOperator {
                    kind: operator_kind,
                    location: operator_loc,
                },
                rhs,
                location,
            })),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn expr_member(&mut self, expr: ast::Member) -> Result<hir::Expression> {
        let id = self.next_expr_id();
        let location = self.location(expr.location);
        let callee = self.expression(expr.callee)?;

        Ok(hir::Expression {
            id,
            location: location.clone(),
            kind: hir::ExpressionKind::Member(Box::new(hir::Member {
                id,
                callee,
                name: expr.name,
                location,
            })),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn expr_range(&mut self, expr: ast::Range) -> Result<hir::Expression> {
        let range_type_name = if expr.inclusive {
            RANGE_INCLUSIVE_STD_TYPE
        } else {
            RANGE_STD_TYPE
        };

        let range_type = lume_ast::Path::with_root(vec!["std", range_type_name], RANGE_NEW_FUNC);

        let id = self.next_expr_id();
        let location = self.location(expr.location);
        let lower = self.expression(expr.lower)?;
        let upper = self.expression(expr.upper)?;

        Ok(hir::Expression {
            id,
            location,
            kind: hir::ExpressionKind::StaticCall(Box::new(hir::StaticCall {
                id,
                name: self.resolve_symbol_name(&range_type)?,
                type_arguments: vec![hir::TypeArgument::Implicit {
                    location: lume_span::Location::empty(),
                }],
                arguments: vec![lower, upper],
            })),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn expr_variable(&mut self, expr: ast::Variable) -> Result<hir::Expression> {
        let id = self.next_expr_id();

        let location = self.location(expr.location().clone());
        let Some(local_id) = self.locals.retrieve(&expr.name.name) else {
            return Err(err!(self, location, UndeclaredVariable, name, expr.name.name));
        };

        Ok(hir::Expression {
            id,
            location: location.clone(),
            kind: hir::ExpressionKind::Variable(Box::new(hir::Variable {
                id,
                reference: local_id.id,
                name: self.identifier(expr.name.clone()),
                location,
            })),
        })
    }
}
