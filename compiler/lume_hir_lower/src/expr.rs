use error_snippet::Result;

use crate::LowerModule;
use crate::err;
use crate::errors::*;
use crate::{ARRAY_NEW_FUNC, ARRAY_STD_TYPE, RANGE_INCLUSIVE_STD_TYPE, RANGE_NEW_FUNC, RANGE_STD_TYPE};

use lume_ast::Node;

impl LowerModule<'_> {
    #[tracing::instrument(level = "DEBUG", skip_all)]
    pub(super) fn expressions(&mut self, expressions: Vec<lume_ast::Expression>) -> Vec<lume_span::ExpressionId> {
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
    pub(super) fn expression(&mut self, statement: lume_ast::Expression) -> Result<lume_span::ExpressionId> {
        let expr = match statement {
            lume_ast::Expression::Array(e) => self.expr_array(*e)?,
            lume_ast::Expression::Assignment(e) => self.expr_assignment(*e)?,
            lume_ast::Expression::Binary(e) => self.expr_binary(*e)?,
            lume_ast::Expression::Call(e) => self.expr_call(*e)?,
            lume_ast::Expression::Cast(e) => self.expr_cast(*e)?,
            lume_ast::Expression::Construct(e) => self.expr_construct(*e)?,
            lume_ast::Expression::If(e) => self.expr_if(*e)?,
            lume_ast::Expression::IntrinsicCall(e) => self.expr_intrinsic_call(*e)?,
            lume_ast::Expression::Literal(e) => self.expr_literal(*e),
            lume_ast::Expression::Logical(e) => self.expr_logical(*e)?,
            lume_ast::Expression::Member(e) => self.expr_member(*e)?,
            lume_ast::Expression::Range(e) => self.expr_range(*e)?,
            lume_ast::Expression::Scope(e) => self.expr_scope(*e)?,
            lume_ast::Expression::Switch(e) => self.expr_switch(*e)?,
            lume_ast::Expression::Variable(e) => self.expr_variable(*e)?,
            lume_ast::Expression::Variant(e) => self.expr_variant(*e)?,
        };

        let id = expr.id;
        self.map.expressions.insert(id, expr);

        Ok(id)
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    pub(super) fn opt_expression(
        &mut self,
        statement: Option<lume_ast::Expression>,
    ) -> Result<Option<lume_span::ExpressionId>> {
        match statement {
            Some(expr) => Ok(Some(self.expression(expr)?)),
            None => Ok(None),
        }
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn expr_array(&mut self, expr: lume_ast::Array) -> Result<lume_hir::Expression> {
        let array_path = lume_ast::Path::with_root(
            vec![
                lume_ast::PathSegment::namespace("std"),
                lume_ast::PathSegment::ty(ARRAY_STD_TYPE),
            ],
            lume_ast::PathSegment::callable(ARRAY_NEW_FUNC),
        );

        let id = self.next_expr_id();
        let values = self.expressions(expr.values);
        let location = self.location(expr.location);

        Ok(lume_hir::Expression {
            id,
            location,
            kind: lume_hir::ExpressionKind::StaticCall(lume_hir::StaticCall {
                id,
                name: self.resolve_symbol_name(&array_path)?,
                arguments: values,
                location,
            }),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn expr_assignment(&mut self, expr: lume_ast::Assignment) -> Result<lume_hir::Expression> {
        let id = self.next_expr_id();
        let location = self.location(expr.location);
        let target = self.expression(expr.target)?;
        let value = self.expression(expr.value)?;

        Ok(lume_hir::Expression {
            id,
            location,
            kind: lume_hir::ExpressionKind::Assignment(lume_hir::Assignment {
                id,
                target,
                value,
                location,
            }),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn expr_binary(&mut self, expr: lume_ast::Binary) -> Result<lume_hir::Expression> {
        let id = self.next_expr_id();
        let location = self.location(expr.location);
        let lhs = self.expression(expr.lhs)?;
        let rhs = self.expression(expr.rhs)?;

        let operator_kind = match expr.op.kind {
            lume_ast::BinaryOperatorKind::And => lume_hir::BinaryOperatorKind::And,
            lume_ast::BinaryOperatorKind::Or => lume_hir::BinaryOperatorKind::Or,
            lume_ast::BinaryOperatorKind::Xor => lume_hir::BinaryOperatorKind::Xor,
        };

        let operator_loc = self.location(expr.op.location);

        Ok(lume_hir::Expression {
            id,
            location,
            kind: lume_hir::ExpressionKind::Binary(lume_hir::Binary {
                id,
                lhs,
                op: lume_hir::BinaryOperator {
                    kind: operator_kind,
                    location: operator_loc,
                },
                rhs,
                location,
            }),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn expr_call(&mut self, expr: lume_ast::Call) -> Result<lume_hir::Expression> {
        let id = self.next_expr_id();
        let name = self.resolve_symbol_name(&expr.name)?;
        let arguments = self.expressions(expr.arguments);
        let location = self.location(expr.location);

        let kind = if let Some(callee) = expr.callee {
            let callee = self.expression(callee)?;

            lume_hir::ExpressionKind::InstanceCall(lume_hir::InstanceCall {
                id,
                callee,
                name: name.name,
                arguments,
                location,
            })
        } else {
            lume_hir::ExpressionKind::StaticCall(lume_hir::StaticCall {
                id,
                name,
                arguments,
                location,
            })
        };

        Ok(lume_hir::Expression { id, location, kind })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn expr_cast(&mut self, expr: lume_ast::Cast) -> Result<lume_hir::Expression> {
        let id = self.next_expr_id();
        let source = self.expression(expr.source)?;
        let target = self.type_ref(expr.target_type)?;
        let location = self.location(expr.location);

        Ok(lume_hir::Expression {
            id,
            location,
            kind: lume_hir::ExpressionKind::Cast(lume_hir::Cast {
                id,
                source,
                target,
                location,
            }),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn expr_construct(&mut self, expr: lume_ast::Construct) -> Result<lume_hir::Expression> {
        let id = self.next_expr_id();
        let path = self.resolve_symbol_name(&expr.path)?;
        let fields = expr
            .fields
            .into_iter()
            .map(|field| self.expr_constructor_field(field))
            .collect::<Result<Vec<_>>>()?;

        let location = self.location(expr.location);

        Ok(lume_hir::Expression {
            id,
            location,
            kind: lume_hir::ExpressionKind::Construct(lume_hir::Construct {
                id,
                path,
                fields,
                location,
            }),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn expr_constructor_field(&mut self, expr: lume_ast::ConstructorField) -> Result<lume_hir::ConstructorField> {
        let name = self.identifier(expr.name);
        let value = self.expression(expr.value)?;
        let location = self.location(expr.location);

        Ok(lume_hir::ConstructorField { name, value, location })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn expr_if(&mut self, expr: lume_ast::IfCondition) -> Result<lume_hir::Expression> {
        let id = self.next_expr_id();
        let location = self.location(expr.location);

        let cases = expr
            .cases
            .into_iter()
            .map(|c| self.expr_condition(c))
            .collect::<Result<Vec<_>>>()?;

        Ok(lume_hir::Expression {
            id,
            location,
            kind: lume_hir::ExpressionKind::If(lume_hir::If { id, cases, location }),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn expr_condition(&mut self, expr: lume_ast::Condition) -> Result<lume_hir::Condition> {
        let location = self.location(expr.location);

        let condition = if let Some(cond) = expr.condition {
            Some(self.expression(cond)?)
        } else {
            None
        };

        let block = self.block(expr.block);

        Ok(lume_hir::Condition {
            condition,
            block,
            location,
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn expr_intrinsic_call(&mut self, expr: lume_ast::IntrinsicCall) -> Result<lume_hir::Expression> {
        let id = self.next_expr_id();
        let name = self.resolve_symbol_name(&expr.name)?;
        let location = self.location(expr.location);

        let mut arguments = vec![self.expression(expr.callee)?];
        arguments.extend_from_slice(&self.expressions(expr.arguments));

        Ok(lume_hir::Expression {
            id,
            location,
            kind: lume_hir::ExpressionKind::IntrinsicCall(lume_hir::IntrinsicCall {
                id,
                name: name.name,
                arguments,
                location,
            }),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn expr_literal(&mut self, expr: lume_ast::Literal) -> lume_hir::Expression {
        let literal = self.literal(expr);

        lume_hir::Expression {
            id: literal.id,
            location: literal.location,
            kind: lume_hir::ExpressionKind::Literal(literal),
        }
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn expr_logical(&mut self, expr: lume_ast::Logical) -> Result<lume_hir::Expression> {
        let id = self.next_expr_id();
        let location = self.location(expr.location);
        let lhs = self.expression(expr.lhs)?;
        let rhs = self.expression(expr.rhs)?;

        let operator_kind = match expr.op.kind {
            lume_ast::LogicalOperatorKind::And => lume_hir::LogicalOperatorKind::And,
            lume_ast::LogicalOperatorKind::Or => lume_hir::LogicalOperatorKind::Or,
        };

        let operator_loc = self.location(expr.op.location);

        Ok(lume_hir::Expression {
            id,
            location,
            kind: lume_hir::ExpressionKind::Logical(lume_hir::Logical {
                id,
                lhs,
                op: lume_hir::LogicalOperator {
                    kind: operator_kind,
                    location: operator_loc,
                },
                rhs,
                location,
            }),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn expr_member(&mut self, expr: lume_ast::Member) -> Result<lume_hir::Expression> {
        let id = self.next_expr_id();
        let location = self.location(expr.location);
        let callee = self.expression(expr.callee)?;

        Ok(lume_hir::Expression {
            id,
            location,
            kind: lume_hir::ExpressionKind::Member(lume_hir::Member {
                id,
                callee,
                name: expr.name,
                location,
            }),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn expr_range(&mut self, expr: lume_ast::Range) -> Result<lume_hir::Expression> {
        let range_type_name = if expr.inclusive {
            RANGE_INCLUSIVE_STD_TYPE
        } else {
            RANGE_STD_TYPE
        };

        let range_type = lume_ast::Path::with_root(
            vec![
                lume_ast::PathSegment::namespace("std"),
                lume_ast::PathSegment::ty(range_type_name),
            ],
            lume_ast::PathSegment::callable(RANGE_NEW_FUNC),
        );

        let id = self.next_expr_id();
        let location = self.location(expr.location);
        let lower = self.expression(expr.lower)?;
        let upper = self.expression(expr.upper)?;

        Ok(lume_hir::Expression {
            id,
            location,
            kind: lume_hir::ExpressionKind::StaticCall(lume_hir::StaticCall {
                id,
                name: self.resolve_symbol_name(&range_type)?,
                arguments: vec![lower, upper],
                location,
            }),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn expr_scope(&mut self, expr: lume_ast::Scope) -> Result<lume_hir::Expression> {
        self.locals.push_frame();

        let id = self.next_expr_id();
        let body = self.statements(expr.body);
        let location = self.location(expr.location);

        self.locals.pop_frame();

        Ok(lume_hir::Expression {
            id,
            location,
            kind: lume_hir::ExpressionKind::Scope(lume_hir::Scope { id, body, location }),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn expr_switch(&mut self, expr: lume_ast::Switch) -> Result<lume_hir::Expression> {
        let id = self.next_expr_id();
        let operand = self.expression(expr.operand)?;
        let cases = expr
            .cases
            .into_iter()
            .map(|field| self.expr_switch_case(field))
            .collect::<Result<Vec<_>>>()?;

        let location = self.location(expr.location);

        Ok(lume_hir::Expression {
            id,
            kind: lume_hir::ExpressionKind::Switch(lume_hir::Switch {
                id,
                operand,
                cases,
                location,
            }),
            location,
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn expr_switch_case(&mut self, expr: lume_ast::SwitchCase) -> Result<lume_hir::SwitchCase> {
        self.locals.push_frame();

        let pattern = self.pattern(expr.pattern)?;
        let branch = self.expression(expr.branch)?;
        let location = self.location(expr.location);

        self.locals.pop_frame();

        Ok(lume_hir::SwitchCase {
            pattern,
            branch,
            location,
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn expr_variable(&mut self, expr: lume_ast::Variable) -> Result<lume_hir::Expression> {
        let id = self.next_expr_id();
        let location = self.location(expr.location().clone());

        let Some(var_source) = self.locals.retrieve(&expr.name.name) else {
            return Err(err!(self, location, UndeclaredVariable, name, expr.name.name));
        };

        Ok(lume_hir::Expression {
            id,
            location,
            kind: lume_hir::ExpressionKind::Variable(lume_hir::Variable {
                id,
                reference: var_source.clone(),
                name: self.identifier(expr.name.clone()),
                location,
            }),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn expr_variant(&mut self, expr: lume_ast::Variant) -> Result<lume_hir::Expression> {
        let id = self.next_expr_id();
        let name = self.resolve_symbol_name(&expr.name)?;
        let location = self.location(expr.location().clone());
        let arguments = self.expressions(expr.arguments);

        Ok(lume_hir::Expression {
            id,
            location,
            kind: lume_hir::ExpressionKind::Variant(lume_hir::Variant {
                id,
                name,
                arguments,
                location,
            }),
        })
    }
}
