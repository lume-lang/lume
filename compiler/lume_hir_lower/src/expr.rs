use std::sync::LazyLock;

use lume_parser::Target;

use crate::*;

static ARRAY_NEW_PATH: LazyLock<lume_hir::Path> = LazyLock::new(|| {
    lume_hir::Path::with_root(
        lume_hir::hir_std_type_path!(Array),
        lume_hir::PathSegment::callable("new"),
    )
});

static ARRAY_PUSH_PATH: LazyLock<lume_hir::PathSegment> = LazyLock::new(|| lume_hir::PathSegment::callable("push"));

impl LoweringContext<'_> {
    #[tracing::instrument(level = "DEBUG", skip_all)]
    pub(super) fn expressions<I>(&mut self, expressions: I) -> Vec<lume_span::NodeId>
    where
        I: IntoIterator<Item = lume_ast::Expr>,
    {
        expressions
            .into_iter()
            .map(|expr| self.expression(expr))
            .collect::<Vec<_>>()
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    pub(crate) fn expression(&mut self, statement: lume_ast::Expr) -> NodeId {
        match statement {
            lume_ast::Expr::ArrayExpr(e) => self.expr_array(e),
            lume_ast::Expr::AssignmentExpr(e) => self.expr_assignment(e),
            lume_ast::Expr::InstanceCallExpr(e) => self.instance_expr_call(e),
            lume_ast::Expr::StaticCallExpr(e) => self.static_expr_call(e),
            lume_ast::Expr::CastExpr(e) => self.expr_cast(e),
            lume_ast::Expr::ConstructExpr(e) => self.expr_construct(e),
            lume_ast::Expr::IfExpr(e) => self.expr_if(e),
            lume_ast::Expr::BinExpr(e) => self.expr_bin(e),
            lume_ast::Expr::PostfixExpr(e) => self.expr_postfix(e),
            lume_ast::Expr::UnaryExpr(e) => self.expr_unary(e),
            lume_ast::Expr::IsExpr(e) => self.expr_is(e),
            lume_ast::Expr::LitExpr(e) => self.expr_literal(e),
            lume_ast::Expr::MemberExpr(e) => self.expr_member(e),
            lume_ast::Expr::RangeExpr(e) => self.expr_range(e),
            lume_ast::Expr::ScopeExpr(e) => self.expr_scope(e),
            lume_ast::Expr::SwitchExpr(e) => self.expr_switch(e),
            lume_ast::Expr::UnsafeExpr(e) => self.expr_unsafe(e),
            lume_ast::Expr::VariableExpr(e) => self.expr_variable(e),
            lume_ast::Expr::VariantExpr(e) => self.expr_variant(e),
            lume_ast::Expr::ParenExpr(e) => match e.expr() {
                Some(inner) => self.expression(inner),
                None => self.missing_expr(None),
            },
        }
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    pub(super) fn opt_expression(&mut self, expr: Option<lume_ast::Expr>) -> NodeId {
        if let Some(expr) = expr {
            self.expression(expr)
        } else {
            self.missing_expr(None)
        }
    }

    pub(crate) fn missing_expr(&mut self, id: Option<NodeId>) -> NodeId {
        let id = id.unwrap_or_else(|| self.next_node_id());

        self.alloc_expr(lume_hir::Expression {
            id,
            kind: lume_hir::ExpressionKind::Missing,
            location: Location::empty(),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn expr_array(&mut self, expr: lume_ast::ArrayExpr) -> NodeId {
        static ARRAY_INTERNAL_NAME: &str = "!array";

        let location = self.location(expr.location());

        let array_id = {
            let val_id = self.next_node_id();
            let val = lume_hir::Expression::static_call(val_id, ARRAY_NEW_PATH.clone(), vec![], location);
            self.map.nodes.insert(val_id, lume_hir::Node::Expression(val));

            let var_id = self.next_node_id();
            let var = lume_hir::Statement::define_variable(var_id, ARRAY_INTERNAL_NAME.into(), val_id, location);
            self.map.nodes.insert(var_id, lume_hir::Node::Statement(var.clone()));

            var_id
        };

        let mut body = vec![array_id];

        for value in expr.items() {
            let value = self.expression(value);

            let var_ref_id = self.next_node_id();
            let var_ref = lume_hir::Expression::variable(var_ref_id, ARRAY_INTERNAL_NAME.into(), array_id, location);
            self.map.nodes.insert(var_ref_id, lume_hir::Node::Expression(var_ref));

            let val_id = self.next_node_id();
            let val = lume_hir::Expression::call(val_id, ARRAY_PUSH_PATH.clone(), var_ref_id, vec![value], location);
            self.map.nodes.insert(val_id, lume_hir::Node::Expression(val));

            let push_id = self.next_node_id();
            let push = lume_hir::Statement::expression(push_id, val_id, location);
            self.map.nodes.insert(push_id, lume_hir::Node::Statement(push));

            body.push(push_id);
        }

        let var_ref_id = self.next_node_id();
        let var_ref = lume_hir::Expression::variable(var_ref_id, ARRAY_INTERNAL_NAME.into(), array_id, location);
        self.map.nodes.insert(var_ref_id, lume_hir::Node::Expression(var_ref));

        let res_id = self.next_node_id();
        let res = lume_hir::Statement::final_ref(res_id, var_ref_id, location);
        self.map.nodes.insert(res_id, lume_hir::Node::Statement(res));

        body.push(res_id);

        let expr_id = self.next_node_id();
        let scope_id = self.next_node_id();

        self.alloc_expr(lume_hir::Expression {
            id: expr_id,
            location,
            kind: lume_hir::ExpressionKind::Scope(lume_hir::Scope {
                id: scope_id,
                body,
                unsafe_: false,
                location,
            }),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn expr_assignment(&mut self, expr: lume_ast::AssignmentExpr) -> NodeId {
        let id = self.next_node_id();
        let location = self.location(expr.location());
        let target = self.opt_expression(expr.lhs());
        let value = self.opt_expression(expr.rhs());

        self.alloc_expr(lume_hir::Expression {
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

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn instance_expr_call(&mut self, expr: lume_ast::InstanceCallExpr) -> NodeId {
        let id = self.next_node_id();
        let callee = self.opt_expression(expr.callee());
        let location = self.location(expr.location());

        let bound_types = if let Some(bound_types) = expr.generic_args() {
            bound_types.args().map(|arg| self.hir_type(arg)).collect::<Vec<_>>()
        } else {
            Vec::new()
        };

        let name = lume_hir::PathSegment::Callable {
            name: self.ident_opt(expr.name()),
            bound_types,
            location,
        };

        let arguments = match expr.arg_list().map(|list| list.expr()) {
            Some(args) => self.expressions(args),
            None => Vec::new(),
        };

        self.alloc_expr(lume_hir::Expression {
            id,
            location,
            kind: lume_hir::ExpressionKind::InstanceCall(lume_hir::InstanceCall {
                id,
                callee,
                name,
                bound_types: Vec::new(),
                arguments,
                location,
            }),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn static_expr_call(&mut self, expr: lume_ast::StaticCallExpr) -> NodeId {
        let id = self.next_node_id();
        let name = self.resolve_symbol_name_opt(expr.path());
        let location = self.location(expr.location());

        let arguments = match expr.arg_list().map(|list| list.expr()) {
            Some(args) => self.expressions(args),
            None => Vec::new(),
        };

        self.alloc_expr(lume_hir::Expression {
            id,
            location,
            kind: lume_hir::ExpressionKind::StaticCall(lume_hir::StaticCall {
                id,
                name,
                arguments,
                location,
            }),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn expr_cast(&mut self, expr: lume_ast::CastExpr) -> NodeId {
        let id = self.next_node_id();
        let source = self.opt_expression(expr.expr());
        let target = self.type_or_void(expr.ty());
        let location = self.location(expr.location());

        self.alloc_expr(lume_hir::Expression {
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

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn expr_construct(&mut self, expr: lume_ast::ConstructExpr) -> NodeId {
        let id = self.next_node_id();
        let path = self.resolve_symbol_name_opt(expr.ty());
        let fields = expr
            .fields()
            .map(|field| self.expr_constructor_field(field))
            .collect::<Vec<_>>();

        let location = self.location(expr.location());

        self.alloc_expr(lume_hir::Expression {
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

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn expr_constructor_field(&mut self, expr: lume_ast::ConstructorField) -> lume_hir::ConstructorField {
        let name = self.ident_opt(expr.name());
        let value = if let Some(value) = expr.value() {
            self.expression(value)
        } else {
            let ast_expr: lume_ast::Expr = crate::make::parse_from_text(name.as_str(), Target::Statement);
            let expr_id = self.expression(ast_expr);

            self.map.expression_mut(expr_id).unwrap().location = name.location;

            expr_id
        };

        let location = self.location(expr.location());

        lume_hir::ConstructorField {
            name,
            value,
            is_default: false,
            location,
        }
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn expr_if(&mut self, expr: lume_ast::IfExpr) -> NodeId {
        let id = self.next_node_id();
        let location = self.location(expr.location());

        let cases = expr.cases().map(|c| self.expr_condition(c)).collect::<Vec<_>>();

        self.alloc_expr(lume_hir::Expression {
            id,
            location,
            kind: lume_hir::ExpressionKind::If(lume_hir::If { id, cases, location }),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn expr_condition(&mut self, expr: lume_ast::Condition) -> lume_hir::Condition {
        let condition = expr.condition().map(|expr| self.expression(expr));
        let block = self.block_opt(expr.block());
        let location = self.location(expr.location());

        lume_hir::Condition {
            condition,
            block,
            location,
        }
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn expr_bin(&mut self, expr: lume_ast::BinExpr) -> NodeId {
        let id = self.next_node_id();
        let location = self.location(expr.location());

        let kind = match expr {
            // Arithmetic intrinsics
            _ if expr.add().is_some() => lume_hir::IntrinsicKind::Add {
                lhs: self.opt_expression(expr.lhs()),
                rhs: self.opt_expression(expr.rhs()),
            },
            _ if expr.sub().is_some() => lume_hir::IntrinsicKind::Sub {
                lhs: self.opt_expression(expr.lhs()),
                rhs: self.opt_expression(expr.rhs()),
            },
            _ if expr.mul().is_some() => lume_hir::IntrinsicKind::Mul {
                lhs: self.opt_expression(expr.lhs()),
                rhs: self.opt_expression(expr.rhs()),
            },
            _ if expr.div().is_some() => lume_hir::IntrinsicKind::Div {
                lhs: self.opt_expression(expr.lhs()),
                rhs: self.opt_expression(expr.rhs()),
            },
            _ if expr.and().is_some() => lume_hir::IntrinsicKind::And {
                lhs: self.opt_expression(expr.lhs()),
                rhs: self.opt_expression(expr.rhs()),
            },
            _ if expr.or().is_some() => lume_hir::IntrinsicKind::Or {
                lhs: self.opt_expression(expr.lhs()),
                rhs: self.opt_expression(expr.rhs()),
            },

            // Logical intrinsics
            _ if expr.binary_and().is_some() => lume_hir::IntrinsicKind::BinaryAnd {
                lhs: self.opt_expression(expr.lhs()),
                rhs: self.opt_expression(expr.rhs()),
            },
            _ if expr.binary_or().is_some() => lume_hir::IntrinsicKind::BinaryOr {
                lhs: self.opt_expression(expr.lhs()),
                rhs: self.opt_expression(expr.rhs()),
            },
            _ if expr.binary_xor().is_some() => lume_hir::IntrinsicKind::BinaryXor {
                lhs: self.opt_expression(expr.lhs()),
                rhs: self.opt_expression(expr.rhs()),
            },

            // Comparison intrinsics
            _ if expr.equal().is_some() => lume_hir::IntrinsicKind::Equal {
                lhs: self.opt_expression(expr.lhs()),
                rhs: self.opt_expression(expr.rhs()),
            },
            _ if expr.nequal().is_some() => lume_hir::IntrinsicKind::NotEqual {
                lhs: self.opt_expression(expr.lhs()),
                rhs: self.opt_expression(expr.rhs()),
            },
            _ if expr.less().is_some() => lume_hir::IntrinsicKind::Less {
                lhs: self.opt_expression(expr.lhs()),
                rhs: self.opt_expression(expr.rhs()),
            },
            _ if expr.lequal().is_some() => lume_hir::IntrinsicKind::LessEqual {
                lhs: self.opt_expression(expr.lhs()),
                rhs: self.opt_expression(expr.rhs()),
            },
            _ if expr.greater().is_some() => lume_hir::IntrinsicKind::Greater {
                lhs: self.opt_expression(expr.lhs()),
                rhs: self.opt_expression(expr.rhs()),
            },
            _ if expr.gequal().is_some() => lume_hir::IntrinsicKind::GreaterEqual {
                lhs: self.opt_expression(expr.lhs()),
                rhs: self.opt_expression(expr.rhs()),
            },

            _ => unreachable!(),
        };

        self.alloc_expr(lume_hir::Expression {
            id,
            location,
            kind: lume_hir::ExpressionKind::IntrinsicCall(lume_hir::IntrinsicCall {
                id,
                kind,
                bound_types: Vec::new(),
                location,
            }),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn expr_postfix(&mut self, expr: lume_ast::PostfixExpr) -> NodeId {
        let id = self.next_node_id();
        let location = self.location(expr.location());

        match expr {
            _ if expr.increment().is_some() => {
                let dst = self.opt_expression(expr.expr());
                let src = self.opt_expression(expr.expr());

                let rhs_id = self.next_node_id();
                self.map.nodes.insert(
                    rhs_id,
                    lume_hir::Node::Expression(lume_hir::Expression::lit(
                        rhs_id,
                        lume_hir::LiteralKind::Int(lume_hir::IntLiteral {
                            id: rhs_id,
                            value: 1,
                            kind: None,
                        }),
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
                            bound_types: Vec::new(),
                            location,
                        }),
                        location,
                    }),
                );

                self.alloc_expr(lume_hir::Expression {
                    id,
                    kind: lume_hir::ExpressionKind::Assignment(lume_hir::Assignment {
                        id,
                        target: dst,
                        value: value_id,
                        location,
                    }),
                    location,
                })
            }
            _ if expr.decrement().is_some() => {
                let dst = self.opt_expression(expr.expr());
                let src = self.opt_expression(expr.expr());

                let rhs_id = self.next_node_id();
                self.map.nodes.insert(
                    rhs_id,
                    lume_hir::Node::Expression(lume_hir::Expression::lit(
                        rhs_id,
                        lume_hir::LiteralKind::Int(lume_hir::IntLiteral {
                            id: rhs_id,
                            value: 1,
                            kind: None,
                        }),
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
                            bound_types: Vec::new(),
                            location,
                        }),
                        location,
                    }),
                );

                self.alloc_expr(lume_hir::Expression {
                    id,
                    kind: lume_hir::ExpressionKind::Assignment(lume_hir::Assignment {
                        id,
                        target: dst,
                        value: value_id,
                        location,
                    }),
                    location,
                })
            }
            _ => unreachable!(),
        }
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn expr_unary(&mut self, expr: lume_ast::UnaryExpr) -> NodeId {
        let id = self.next_node_id();
        let location = self.location(expr.location());

        let kind = match expr {
            _ if expr.sub().is_some() => lume_hir::IntrinsicKind::Negate {
                target: self.opt_expression(expr.expr()),
            },
            _ if expr.not().is_some() => lume_hir::IntrinsicKind::Not {
                target: self.opt_expression(expr.expr()),
            },
            _ => unreachable!(),
        };

        self.alloc_expr(lume_hir::Expression {
            id,
            location,
            kind: lume_hir::ExpressionKind::IntrinsicCall(lume_hir::IntrinsicCall {
                id,
                kind,
                bound_types: Vec::new(),
                location,
            }),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn expr_is(&mut self, expr: lume_ast::IsExpr) -> NodeId {
        let id = self.next_node_id();
        let target = self.opt_expression(expr.expr());
        let pattern = self.pattern_opt(expr.pat());
        let location = self.location(expr.location());

        self.alloc_expr(lume_hir::Expression {
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

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn expr_literal(&mut self, expr: lume_ast::LitExpr) -> NodeId {
        let literal = self.literal_opt(expr.literal());

        self.alloc_expr(lume_hir::Expression {
            id: literal.id,
            location: literal.location,
            kind: lume_hir::ExpressionKind::Literal(literal),
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn expr_member(&mut self, expr: lume_ast::MemberExpr) -> NodeId {
        let id = self.next_node_id();
        let callee = self.opt_expression(expr.expr());
        let name = self.ident_opt(expr.name());
        let location = self.location(expr.location());

        self.alloc_expr(lume_hir::Expression {
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

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn expr_range(&mut self, expr: lume_ast::RangeExpr) -> NodeId {
        let range_new_name = if expr.assign().is_some() {
            lume_hir::Path::with_root(
                lume_hir::hir_std_type_path!(RangeInclusive),
                lume_hir::PathSegment::callable("new"),
            )
        } else {
            lume_hir::Path::with_root(
                lume_hir::hir_std_type_path!(Range),
                lume_hir::PathSegment::callable("new"),
            )
        };

        let lower = self.opt_expression(expr.lower());
        let upper = self.opt_expression(expr.upper());
        let location = self.location(expr.location());

        self.alloc_static_call(range_new_name, vec![lower, upper], location)
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn expr_scope(&mut self, expr: lume_ast::ScopeExpr) -> NodeId {
        let location = self.location(expr.location());

        self.alloc_within_scope(
            |hir| expr.block().map_or_else(Vec::new, |block| hir.statements(block.stmt())),
            false,
            location,
        )
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn expr_switch(&mut self, expr: lume_ast::SwitchExpr) -> NodeId {
        let id = self.next_node_id();
        let operand = self.opt_expression(expr.expr());
        let cases = expr.arms().map(|arm| self.expr_switch_case(arm)).collect::<Vec<_>>();
        let location = self.location(expr.location());

        self.alloc_expr(lume_hir::Expression {
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

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn expr_unsafe(&mut self, expr: lume_ast::UnsafeExpr) -> NodeId {
        let location = self.location(expr.location());

        self.alloc_within_scope(
            |hir| expr.block().map_or_else(Vec::new, |block| hir.statements(block.stmt())),
            true,
            location,
        )
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn expr_switch_case(&mut self, expr: lume_ast::SwitchArm) -> lume_hir::SwitchCase {
        self.current_locals.push_frame();

        let pattern = self.pattern_opt(expr.pat());
        let branch = self.opt_expression(expr.expr());
        let location = self.location(expr.location());

        self.current_locals.pop_frame();

        lume_hir::SwitchCase {
            pattern,
            branch,
            location,
        }
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn expr_variable(&mut self, expr: lume_ast::VariableExpr) -> NodeId {
        let name = self.ident_opt(expr.name());
        let location = self.location(expr.location().clone());

        self.alloc_variable_ref(name, location)
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn expr_variant(&mut self, expr: lume_ast::VariantExpr) -> NodeId {
        let id = self.next_node_id();
        let name = self.resolve_symbol_name_opt(expr.path());
        let location = self.location(expr.location().clone());
        let arguments = match expr.arg_list().map(|list| list.expr()) {
            Some(args) => self.expressions(args),
            None => Vec::new(),
        };

        self.alloc_expr(lume_hir::Expression {
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
