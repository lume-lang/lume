use std::sync::LazyLock;

use error_snippet::Result;
use lume_ast::Node;

use crate::errors::*;
use crate::*;

static ARRAY_INTERNAL_NAME: &str = "!array";

static ARRAY_NEW_PATH: LazyLock<lume_hir::Path> = LazyLock::new(|| {
    lume_hir::Path::from_parts(
        Some(vec![
            lume_hir::PathSegment::namespace("std"),
            lume_hir::PathSegment::ty(ARRAY_STD_TYPE),
        ]),
        lume_hir::PathSegment::callable(ARRAY_NEW_FUNC),
    )
});

static ARRAY_PUSH_PATH: LazyLock<lume_hir::PathSegment> =
    LazyLock::new(|| lume_hir::PathSegment::callable(ARRAY_PUSH_FUNC));

impl LowerModule<'_> {
    #[libftrace::traced(level = Debug)]
    pub(super) fn expressions(&mut self, expressions: Vec<lume_ast::Expression>) -> Vec<lume_span::NodeId> {
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

    #[libftrace::traced(level = Debug)]
    pub(super) fn expression(&mut self, statement: lume_ast::Expression) -> Result<lume_span::NodeId> {
        let expr = match statement {
            lume_ast::Expression::Array(e) => self.expr_array(*e)?,
            lume_ast::Expression::Assignment(e) => self.expr_assignment(*e)?,
            lume_ast::Expression::Call(e) => self.expr_call(*e)?,
            lume_ast::Expression::Cast(e) => self.expr_cast(*e)?,
            lume_ast::Expression::Construct(e) => self.expr_construct(*e)?,
            lume_ast::Expression::If(e) => self.expr_if(*e)?,
            lume_ast::Expression::IntrinsicCall(e) => self.expr_intrinsic_call(*e)?,
            lume_ast::Expression::Is(e) => self.expr_is(*e)?,
            lume_ast::Expression::Literal(e) => self.expr_literal(*e),
            lume_ast::Expression::Member(e) => self.expr_member(*e)?,
            lume_ast::Expression::Range(e) => self.expr_range(*e)?,
            lume_ast::Expression::Scope(e) => self.expr_scope(*e)?,
            lume_ast::Expression::Switch(e) => self.expr_switch(*e)?,
            lume_ast::Expression::Variable(e) => self.expr_variable(*e)?,
            lume_ast::Expression::Variant(e) => self.expr_variant(*e)?,
        };

        let id = expr.id;
        self.map.nodes.insert(id, lume_hir::Node::Expression(expr));

        Ok(id)
    }

    #[libftrace::traced(level = Debug)]
    pub(super) fn opt_expression(
        &mut self,
        statement: Option<lume_ast::Expression>,
    ) -> Result<Option<lume_span::NodeId>> {
        match statement {
            Some(expr) => Ok(Some(self.expression(expr)?)),
            None => Ok(None),
        }
    }

    #[libftrace::traced(level = Debug)]
    fn expr_array(&mut self, expr: lume_ast::Array) -> Result<lume_hir::Expression> {
        let location = self.location(expr.location);

        let var = {
            let val_id = self.next_node_id();
            let val = lume_hir::Expression::static_call(val_id, ARRAY_NEW_PATH.clone(), vec![], location);
            self.map.nodes.insert(val_id, lume_hir::Node::Expression(val));

            let var_id = self.next_node_id();
            let var = lume_hir::Statement::define_variable(var_id, ARRAY_INTERNAL_NAME.into(), val_id, location);
            self.map.nodes.insert(var_id, lume_hir::Node::Statement(var.clone()));

            var
        };

        let lume_hir::StatementKind::Variable(decl) = &var.kind else {
            unreachable!()
        };

        let mut body = vec![var.id];

        let var_ref_id = self.next_node_id();
        let var_ref = lume_hir::Expression::variable(var_ref_id, ARRAY_INTERNAL_NAME.into(), decl.clone(), location);
        self.map.nodes.insert(var_ref_id, lume_hir::Node::Expression(var_ref));

        for value in expr.values {
            let value = self.expression(value)?;

            let val_id = self.next_node_id();
            let val = lume_hir::Expression::call(val_id, ARRAY_PUSH_PATH.clone(), var_ref_id, vec![value], location);
            self.map.nodes.insert(val_id, lume_hir::Node::Expression(val));

            let push_id = self.next_node_id();
            let push = lume_hir::Statement::expression(push_id, val_id, location);
            self.map.nodes.insert(push_id, lume_hir::Node::Statement(push));

            body.push(push_id);
        }

        let var_ref_id = self.next_node_id();
        let var_ref = lume_hir::Expression::variable(var_ref_id, ARRAY_INTERNAL_NAME.into(), decl.clone(), location);
        self.map.nodes.insert(var_ref_id, lume_hir::Node::Expression(var_ref));

        let res_id = self.next_node_id();
        let res = lume_hir::Statement::final_ref(res_id, var_ref_id, location);
        self.map.nodes.insert(res_id, lume_hir::Node::Statement(res));

        body.push(res_id);

        Ok(lume_hir::Expression {
            id: self.next_node_id(),
            location,
            kind: lume_hir::ExpressionKind::Scope(lume_hir::Scope {
                id: self.next_node_id(),
                body,
                location,
            }),
        })
    }

    #[libftrace::traced(level = Debug)]
    fn expr_assignment(&mut self, expr: lume_ast::Assignment) -> Result<lume_hir::Expression> {
        let id = self.next_node_id();
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

    #[libftrace::traced(level = Debug)]
    fn expr_call(&mut self, expr: lume_ast::Call) -> Result<lume_hir::Expression> {
        let id = self.next_node_id();
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

    #[libftrace::traced(level = Debug)]
    fn expr_cast(&mut self, expr: lume_ast::Cast) -> Result<lume_hir::Expression> {
        let id = self.next_node_id();
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

    #[libftrace::traced(level = Debug)]
    fn expr_construct(&mut self, expr: lume_ast::Construct) -> Result<lume_hir::Expression> {
        let id = self.next_node_id();
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

    #[libftrace::traced(level = Debug)]
    fn expr_constructor_field(&mut self, expr: lume_ast::ConstructorField) -> Result<lume_hir::ConstructorField> {
        let value = expr.value.unwrap_or_else(|| expr.name.clone().as_var());

        let name = self.identifier(expr.name);
        let value = self.expression(value)?;
        let location = self.location(expr.location);

        Ok(lume_hir::ConstructorField {
            name,
            value,
            is_default: false,
            location,
        })
    }

    #[libftrace::traced(level = Debug)]
    fn expr_if(&mut self, expr: lume_ast::IfCondition) -> Result<lume_hir::Expression> {
        let id = self.next_node_id();
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

    #[libftrace::traced(level = Debug)]
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

    #[libftrace::traced(level = Debug)]
    fn expr_intrinsic_call(&mut self, expr: lume_ast::IntrinsicCall) -> Result<lume_hir::Expression> {
        let id = self.next_node_id();
        let location = self.location(expr.location);

        let kind = match expr.kind {
            // Arithmetic intrinsics
            lume_ast::IntrinsicKind::Add { lhs, rhs } => lume_hir::IntrinsicKind::Add {
                lhs: self.expression(*lhs)?,
                rhs: self.expression(*rhs)?,
            },
            lume_ast::IntrinsicKind::Sub { lhs, rhs } => lume_hir::IntrinsicKind::Sub {
                lhs: self.expression(*lhs)?,
                rhs: self.expression(*rhs)?,
            },
            lume_ast::IntrinsicKind::Mul { lhs, rhs } => lume_hir::IntrinsicKind::Mul {
                lhs: self.expression(*lhs)?,
                rhs: self.expression(*rhs)?,
            },
            lume_ast::IntrinsicKind::Div { lhs, rhs } => lume_hir::IntrinsicKind::Div {
                lhs: self.expression(*lhs)?,
                rhs: self.expression(*rhs)?,
            },
            lume_ast::IntrinsicKind::And { lhs, rhs } => lume_hir::IntrinsicKind::And {
                lhs: self.expression(*lhs)?,
                rhs: self.expression(*rhs)?,
            },
            lume_ast::IntrinsicKind::Or { lhs, rhs } => lume_hir::IntrinsicKind::Or {
                lhs: self.expression(*lhs)?,
                rhs: self.expression(*rhs)?,
            },
            lume_ast::IntrinsicKind::Negate { target } => lume_hir::IntrinsicKind::Negate {
                target: self.expression(*target)?,
            },

            lume_ast::IntrinsicKind::Increment { target } => {
                let dst = self.expression(*target.clone())?;
                let src = self.expression(*target)?;

                let rhs_id = self.next_node_id();
                self.map.nodes.insert(
                    rhs_id,
                    lume_hir::Node::Expression(lume_hir::Expression::lit(
                        rhs_id,
                        lume_hir::LiteralKind::Int(Box::new(lume_hir::IntLiteral {
                            id: rhs_id,
                            value: 1,
                            kind: None,
                        })),
                    )),
                );

                let value_id = self.next_node_id();
                self.map.nodes.insert(
                    value_id,
                    lume_hir::Node::Expression(lume_hir::Expression {
                        id: value_id,
                        kind: lume_hir::ExpressionKind::IntrinsicCall(lume_hir::IntrinsicCall {
                            id: value_id,
                            kind: lume_hir::IntrinsicKind::Add { lhs: src, rhs: rhs_id },
                            location,
                        }),
                        location,
                    }),
                );

                return Ok(lume_hir::Expression {
                    id,
                    kind: lume_hir::ExpressionKind::Assignment(lume_hir::Assignment {
                        id,
                        target: dst,
                        value: value_id,
                        location,
                    }),
                    location,
                });
            }
            lume_ast::IntrinsicKind::Decrement { target } => {
                let dst = self.expression(*target.clone())?;
                let src = self.expression(*target)?;

                let rhs_id = self.next_node_id();
                self.map.nodes.insert(
                    rhs_id,
                    lume_hir::Node::Expression(lume_hir::Expression::lit(
                        rhs_id,
                        lume_hir::LiteralKind::Int(Box::new(lume_hir::IntLiteral {
                            id: rhs_id,
                            value: 1,
                            kind: None,
                        })),
                    )),
                );

                let value_id = self.next_node_id();
                self.map.nodes.insert(
                    value_id,
                    lume_hir::Node::Expression(lume_hir::Expression {
                        id: value_id,
                        kind: lume_hir::ExpressionKind::IntrinsicCall(lume_hir::IntrinsicCall {
                            id: value_id,
                            kind: lume_hir::IntrinsicKind::Sub { lhs: src, rhs: rhs_id },
                            location,
                        }),
                        location,
                    }),
                );

                return Ok(lume_hir::Expression {
                    id,
                    kind: lume_hir::ExpressionKind::Assignment(lume_hir::Assignment {
                        id,
                        target: dst,
                        value: value_id,
                        location,
                    }),
                    location,
                });
            }

            // Logical intrinsics
            lume_ast::IntrinsicKind::BinaryAnd { lhs, rhs } => lume_hir::IntrinsicKind::BinaryAnd {
                lhs: self.expression(*lhs)?,
                rhs: self.expression(*rhs)?,
            },
            lume_ast::IntrinsicKind::BinaryOr { lhs, rhs } => lume_hir::IntrinsicKind::BinaryOr {
                lhs: self.expression(*lhs)?,
                rhs: self.expression(*rhs)?,
            },
            lume_ast::IntrinsicKind::BinaryXor { lhs, rhs } => lume_hir::IntrinsicKind::BinaryXor {
                lhs: self.expression(*lhs)?,
                rhs: self.expression(*rhs)?,
            },
            lume_ast::IntrinsicKind::Not { target } => lume_hir::IntrinsicKind::Not {
                target: self.expression(*target)?,
            },

            // Comparison intrinsics
            lume_ast::IntrinsicKind::Equal { lhs, rhs } => lume_hir::IntrinsicKind::Equal {
                lhs: self.expression(*lhs)?,
                rhs: self.expression(*rhs)?,
            },
            lume_ast::IntrinsicKind::NotEqual { lhs, rhs } => lume_hir::IntrinsicKind::NotEqual {
                lhs: self.expression(*lhs)?,
                rhs: self.expression(*rhs)?,
            },
            lume_ast::IntrinsicKind::Less { lhs, rhs } => lume_hir::IntrinsicKind::Less {
                lhs: self.expression(*lhs)?,
                rhs: self.expression(*rhs)?,
            },
            lume_ast::IntrinsicKind::LessEqual { lhs, rhs } => lume_hir::IntrinsicKind::LessEqual {
                lhs: self.expression(*lhs)?,
                rhs: self.expression(*rhs)?,
            },
            lume_ast::IntrinsicKind::Greater { lhs, rhs } => lume_hir::IntrinsicKind::Greater {
                lhs: self.expression(*lhs)?,
                rhs: self.expression(*rhs)?,
            },
            lume_ast::IntrinsicKind::GreaterEqual { lhs, rhs } => lume_hir::IntrinsicKind::GreaterEqual {
                lhs: self.expression(*lhs)?,
                rhs: self.expression(*rhs)?,
            },
        };

        Ok(lume_hir::Expression {
            id,
            location,
            kind: lume_hir::ExpressionKind::IntrinsicCall(lume_hir::IntrinsicCall { id, kind, location }),
        })
    }

    #[libftrace::traced(level = Debug)]
    fn expr_is(&mut self, expr: lume_ast::Is) -> Result<lume_hir::Expression> {
        let id = self.next_node_id();
        let target = self.expression(expr.target)?;
        let pattern = self.pattern(expr.pattern)?;
        let location = self.location(expr.location);

        Ok(lume_hir::Expression {
            id,
            kind: lume_hir::ExpressionKind::Is(lume_hir::Is {
                id,
                target,
                pattern,
                location,
            }),
            location,
        })
    }

    #[libftrace::traced(level = Debug)]
    fn expr_literal(&mut self, expr: lume_ast::Literal) -> lume_hir::Expression {
        let literal = self.literal(expr);

        lume_hir::Expression {
            id: literal.id,
            location: literal.location,
            kind: lume_hir::ExpressionKind::Literal(literal),
        }
    }

    #[libftrace::traced(level = Debug)]
    fn expr_member(&mut self, expr: lume_ast::Member) -> Result<lume_hir::Expression> {
        let id = self.next_node_id();
        let location = self.location(expr.location);
        let callee = self.expression(expr.callee)?;
        let name = self.identifier(expr.name);

        Ok(lume_hir::Expression {
            id,
            location,
            kind: lume_hir::ExpressionKind::Member(lume_hir::Member {
                id,
                callee,
                name,
                location,
            }),
        })
    }

    #[libftrace::traced(level = Debug)]
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

        let id = self.next_node_id();
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

    #[libftrace::traced(level = Debug)]
    fn expr_scope(&mut self, expr: lume_ast::Scope) -> Result<lume_hir::Expression> {
        self.locals.push_frame();

        let id = self.next_node_id();
        let body = self.statements(expr.body);
        let location = self.location(expr.location);

        self.locals.pop_frame();

        Ok(lume_hir::Expression {
            id,
            location,
            kind: lume_hir::ExpressionKind::Scope(lume_hir::Scope { id, body, location }),
        })
    }

    #[libftrace::traced(level = Debug)]
    fn expr_switch(&mut self, expr: lume_ast::Switch) -> Result<lume_hir::Expression> {
        let id = self.next_node_id();
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

    #[libftrace::traced(level = Debug)]
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

    #[libftrace::traced(level = Debug)]
    fn expr_variable(&mut self, expr: lume_ast::Variable) -> Result<lume_hir::Expression> {
        let id = self.next_node_id();
        let location = self.location(expr.location().clone());

        let Some(var_source) = self.locals.retrieve(&expr.name.name) else {
            return Err(UndeclaredVariable {
                source: self.file.clone(),
                range: location.index.clone(),
                name: expr.name.name,
            }
            .into());
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

    #[libftrace::traced(level = Debug)]
    fn expr_variant(&mut self, expr: lume_ast::Variant) -> Result<lume_hir::Expression> {
        let id = self.next_node_id();
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
