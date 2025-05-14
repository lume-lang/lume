use error_snippet::Result;

use crate::errors::*;
use crate::lower::{
    ARRAY_STD_TYPE, ARRAY_WITH_CAPACITY_FUNC, RANGE_INCLUSIVE_STD_TYPE, RANGE_NEW_FUNC, RANGE_STD_TYPE,
};
use crate::{self as hir, err, lower::LowerModule};
use lume_ast::{self as ast, Node};

impl<'a> LowerModule<'a> {
    pub(super) fn expressions(&mut self, expressions: Vec<ast::Expression>) -> Result<Vec<hir::Expression>> {
        expressions.into_iter().map(|expr| self.expression(expr)).collect()
    }

    pub(super) fn expression(&mut self, statement: ast::Expression) -> Result<hir::Expression> {
        let expr = match statement {
            ast::Expression::Array(e) => self.expr_array(*e)?,
            ast::Expression::Assignment(e) => self.expr_assignment(*e)?,
            ast::Expression::Call(e) => self.expr_call(*e)?,
            ast::Expression::Literal(e) => self.expr_literal(*e)?,
            ast::Expression::Member(e) => self.expr_member(*e)?,
            ast::Expression::Range(e) => self.expr_range(*e)?,
            ast::Expression::Variable(e) => self.expr_variable(*e)?,
        };

        self.map.expressions.insert(expr.id, expr.clone());

        Ok(expr)
    }

    pub(super) fn opt_expression(&mut self, statement: Option<ast::Expression>) -> Result<Option<hir::Expression>> {
        match statement {
            Some(expr) => Ok(Some(self.expression(expr)?)),
            None => Ok(None),
        }
    }

    fn expr_array(&mut self, expr: ast::Array) -> Result<hir::Expression> {
        // TODO: Implement proper array expression lowering
        let array_path = lume_ast::Path {
            name: lume_ast::Identifier {
                name: String::from(ARRAY_WITH_CAPACITY_FUNC),
                location: expr.location.clone(),
            },
            root: lume_ast::NamespacePath::new(&["std", ARRAY_STD_TYPE]),
            location: expr.location.clone(),
        };

        let id = self.next_expr_id();
        let location = self.location(expr.location);

        Ok(hir::Expression {
            id,
            location: location.clone(),
            kind: hir::ExpressionKind::StaticCall(Box::new(hir::StaticCall {
                id,
                name: self.resolve_symbol_name(&array_path),
                type_arguments: vec![hir::TypeArgument::Implicit {
                    location: hir::Location::empty(),
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
                        location: hir::Location::empty(),
                    })),
                    location: hir::Location::empty(),
                }],
            })),
        })
    }

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

    fn expr_call(&mut self, expr: ast::Call) -> Result<hir::Expression> {
        let id = self.next_expr_id();
        let name = self.resolve_symbol_name(&expr.name);
        let type_arguments = self.type_arguments(expr.type_arguments)?;
        let arguments = self.expressions(expr.arguments)?;
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

    fn expr_literal(&mut self, expr: ast::Literal) -> Result<hir::Expression> {
        let literal = self.literal(expr)?;

        Ok(hir::Expression {
            id: literal.id,
            location: literal.location.clone(),
            kind: hir::ExpressionKind::Literal(Box::new(literal)),
        })
    }

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

    fn expr_range(&mut self, expr: ast::Range) -> Result<hir::Expression> {
        let range_type_name = if expr.inclusive {
            RANGE_INCLUSIVE_STD_TYPE
        } else {
            RANGE_STD_TYPE
        };

        let range_type = lume_ast::Path {
            name: lume_ast::Identifier {
                name: String::from(range_type_name),
                location: expr.location.clone(),
            },
            root: lume_ast::NamespacePath::new(&["std", RANGE_NEW_FUNC]),
            location: expr.location.clone(),
        };

        let id = self.next_expr_id();
        let location = self.location(expr.location);
        let lower = self.expression(expr.lower)?;
        let upper = self.expression(expr.upper)?;

        Ok(hir::Expression {
            id,
            location,
            kind: hir::ExpressionKind::StaticCall(Box::new(hir::StaticCall {
                id,
                name: self.resolve_symbol_name(&range_type),
                type_arguments: vec![hir::TypeArgument::Implicit {
                    location: hir::Location::empty(),
                }],
                arguments: vec![lower, upper],
            })),
        })
    }

    fn expr_variable(&mut self, expr: ast::Variable) -> Result<hir::Expression> {
        let id = self.next_expr_id();

        let location = self.location(expr.location().clone());
        let local_id = match self.locals.retrieve(&expr.name.name) {
            Some(id) => id,
            None => return Err(err!(self, location, UndeclaredVariable, name, expr.name.name)),
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
