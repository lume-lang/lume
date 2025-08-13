use error_snippet::Result;

use crate::LowerModule;
use crate::err;
use crate::errors::*;
use crate::{ARRAY_NEW_FUNC, ARRAY_STD_TYPE, RANGE_INCLUSIVE_STD_TYPE, RANGE_NEW_FUNC, RANGE_STD_TYPE};

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
            ast::Expression::Construct(e) => self.expr_construct(*e)?,
            ast::Expression::If(e) => self.expr_if(*e)?,
            ast::Expression::IntrinsicCall(e) => self.expr_intrinsic_call(*e)?,
            ast::Expression::Literal(e) => self.expr_literal(*e),
            ast::Expression::Logical(e) => self.expr_logical(*e)?,
            ast::Expression::Member(e) => self.expr_member(*e)?,
            ast::Expression::Range(e) => self.expr_range(*e)?,
            ast::Expression::Scope(e) => self.expr_scope(*e)?,
            ast::Expression::Switch(e) => self.expr_switch(*e)?,
            ast::Expression::Variable(e) => self.expr_variable(*e)?,
            ast::Expression::Variant(e) => self.expr_variant(*e)?,
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
        let array_path = lume_ast::Path::with_root(
            vec![ast::PathSegment::namespace("std"), ast::PathSegment::ty(ARRAY_STD_TYPE)],
            ast::PathSegment::callable(ARRAY_NEW_FUNC),
        );

        let id = self.next_expr_id();
        let values = self.expressions(expr.values);
        let location = self.location(expr.location);

        Ok(hir::Expression {
            id,
            location,
            kind: hir::ExpressionKind::StaticCall(Box::new(hir::StaticCall {
                id,
                name: self.resolve_symbol_name(&array_path)?,
                arguments: values,
                location,
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
            kind: hir::ExpressionKind::Assignment(Box::new(hir::Assignment {
                id,
                target,
                value,
                location,
            })),
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
            location,
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
        let arguments = self.expressions(expr.arguments);
        let location = self.location(expr.location);

        let kind = if let Some(callee) = expr.callee {
            let callee = self.expression(callee)?;

            hir::ExpressionKind::InstanceCall(Box::new(hir::InstanceCall {
                id,
                callee,
                name: name.name,
                arguments,
                location,
            }))
        } else {
            hir::ExpressionKind::StaticCall(Box::new(hir::StaticCall {
                id,
                name,
                arguments,
                location,
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
            kind: hir::ExpressionKind::Cast(Box::new(hir::Cast {
                id,
                source,
                target,
                location,
            })),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn expr_construct(&mut self, expr: ast::Construct) -> Result<hir::Expression> {
        let id = self.next_expr_id();
        let path = self.resolve_symbol_name(&expr.path)?;
        let fields = expr
            .fields
            .into_iter()
            .map(|field| self.expr_constructor_field(field))
            .collect::<Result<Vec<_>>>()?;

        let location = self.location(expr.location);

        Ok(hir::Expression {
            id,
            location,
            kind: hir::ExpressionKind::Construct(Box::new(hir::Construct {
                id,
                path,
                fields,
                location,
            })),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn expr_constructor_field(&mut self, expr: ast::ConstructorField) -> Result<hir::ConstructorField> {
        let name = self.identifier(expr.name);
        let value = self.expression(expr.value)?;
        let location = self.location(expr.location);

        Ok(hir::ConstructorField { name, value, location })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn expr_if(&mut self, expr: ast::IfCondition) -> Result<hir::Expression> {
        let id = self.next_expr_id();
        let location = self.location(expr.location);

        let cases = expr
            .cases
            .into_iter()
            .map(|c| self.expr_condition(c))
            .collect::<Result<Vec<_>>>()?;

        Ok(hir::Expression {
            id,
            location,
            kind: hir::ExpressionKind::If(Box::new(hir::If { id, cases, location })),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn expr_condition(&mut self, expr: ast::Condition) -> Result<hir::Condition> {
        let location = self.location(expr.location);

        let condition = if let Some(cond) = expr.condition {
            Some(self.expression(cond)?)
        } else {
            None
        };

        let block = self.block(expr.block);

        Ok(hir::Condition {
            condition,
            block,
            location,
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn expr_intrinsic_call(&mut self, expr: ast::IntrinsicCall) -> Result<hir::Expression> {
        let id = self.next_expr_id();
        let name = self.resolve_symbol_name(&expr.name)?;
        let location = self.location(expr.location);

        let mut arguments = vec![self.expression(expr.callee)?];
        arguments.extend_from_slice(&self.expressions(expr.arguments));

        Ok(hir::Expression {
            id,
            location,
            kind: hir::ExpressionKind::IntrinsicCall(Box::new(hir::IntrinsicCall {
                id,
                name: name.name,
                arguments,
                location,
            })),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn expr_literal(&mut self, expr: ast::Literal) -> hir::Expression {
        let literal = self.literal(expr);

        hir::Expression {
            id: literal.id,
            location: literal.location,
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
            location,
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
            location,
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

        let range_type = lume_ast::Path::with_root(
            vec![
                ast::PathSegment::namespace("std"),
                ast::PathSegment::ty(range_type_name),
            ],
            ast::PathSegment::callable(RANGE_NEW_FUNC),
        );

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
                arguments: vec![lower, upper],
                location,
            })),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn expr_scope(&mut self, expr: ast::Scope) -> Result<hir::Expression> {
        self.locals.push_frame();

        let id = self.next_expr_id();
        let body = self.statements(expr.body);
        let location = self.location(expr.location);

        self.locals.pop_frame();

        Ok(hir::Expression {
            id,
            location,
            kind: hir::ExpressionKind::Scope(Box::new(hir::Scope { id, body, location })),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn expr_switch(&mut self, expr: ast::Switch) -> Result<hir::Expression> {
        let id = self.next_expr_id();
        let operand = self.expression(expr.operand)?;
        let cases = expr
            .cases
            .into_iter()
            .map(|field| self.expr_switch_case(field))
            .collect::<Result<Vec<_>>>()?;

        let location = self.location(expr.location);

        Ok(hir::Expression {
            id,
            kind: lume_hir::ExpressionKind::Switch(Box::new(hir::Switch {
                id,
                operand,
                cases,
                location,
            })),
            location,
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn expr_switch_case(&mut self, expr: ast::SwitchCase) -> Result<hir::SwitchCase> {
        self.locals.push_frame();

        let pattern = self.pattern(expr.pattern)?;
        let branch = self.expression(expr.branch)?;
        let location = self.location(expr.location);

        self.locals.pop_frame();

        Ok(hir::SwitchCase {
            pattern,
            branch,
            location,
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn expr_variable(&mut self, expr: ast::Variable) -> Result<hir::Expression> {
        let id = self.next_expr_id();
        let location = self.location(expr.location().clone());

        let Some(var_source) = self.locals.retrieve(&expr.name.name) else {
            return Err(err!(self, location, UndeclaredVariable, name, expr.name.name));
        };

        Ok(hir::Expression {
            id,
            location,
            kind: hir::ExpressionKind::Variable(Box::new(hir::Variable {
                id,
                reference: var_source.clone(),
                name: self.identifier(expr.name.clone()),
                location,
            })),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn expr_variant(&mut self, expr: ast::Variant) -> Result<hir::Expression> {
        let id = self.next_expr_id();
        let name = self.resolve_symbol_name(&expr.name)?;
        let location = self.location(expr.location().clone());
        let arguments = self.expressions(expr.arguments);

        Ok(hir::Expression {
            id,
            location,
            kind: hir::ExpressionKind::Variant(Box::new(hir::Variant {
                id,
                name,
                arguments,
                location,
            })),
        })
    }
}
