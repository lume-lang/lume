use lume_errors::Result;

use crate::*;

/// Visitor trait for traversing functions in a provided TIR.
pub trait Visitor {
    fn visit_type(&mut self, _ty: &mut TypeRef) -> Result<()> {
        Ok(())
    }

    fn visit_stmt(&mut self, _stmt: &mut Statement) -> Result<()> {
        Ok(())
    }

    fn visit_expr(&mut self, _expr: &mut Expression) -> Result<()> {
        Ok(())
    }

    fn visit_pattern(&mut self, _pattern: &mut Pattern) -> Result<()> {
        Ok(())
    }

    fn visit_path(&mut self, _path: &mut Path) -> Result<()> {
        Ok(())
    }
}

/// Traverses the given TIR function using the provided visitor.
pub fn traverse<V: Visitor>(func: &mut Function, visitor: &mut V) -> Result<()> {
    traverse_path(visitor, &mut func.name)?;
    traverse_type_params(visitor, &mut func.type_params)?;

    for param in &mut func.parameters {
        visitor.visit_type(&mut param.ty)?;
    }

    if let Some(block) = &mut func.block {
        for stmt in &mut block.statements {
            traverse_stmt(visitor, stmt)?;
        }
    }

    visitor.visit_type(&mut func.return_type)?;

    Ok(())
}

fn traverse_type_params<V: Visitor>(visitor: &mut V, type_params: &mut Vec<TypeParameter>) -> Result<()> {
    for type_param in type_params {
        for constraint in &mut type_param.constraints {
            visitor.visit_type(constraint)?;
        }
    }

    Ok(())
}

fn traverse_stmt<V: Visitor>(visitor: &mut V, stmt: &mut Statement) -> Result<()> {
    visitor.visit_stmt(stmt)?;

    match stmt {
        Statement::Variable(stmt) => {
            traverse_expr(visitor, &mut stmt.value)?;
        }
        Statement::Break(_) | Statement::Continue(_) => {}
        Statement::Final(stmt) => {
            traverse_expr(visitor, &mut stmt.value)?;
        }
        Statement::Return(stmt) => {
            if let Some(value) = &mut stmt.value {
                traverse_expr(visitor, value)?;
            }
        }
        Statement::InfiniteLoop(stmt) => {
            for stmt in &mut stmt.block.statements {
                traverse_stmt(visitor, stmt)?;
            }
        }
        Statement::IteratorLoop(stmt) => {
            traverse_expr(visitor, &mut stmt.collection)?;

            for stmt in &mut stmt.block.statements {
                traverse_stmt(visitor, stmt)?;
            }
        }
        Statement::Expression(expr) => {
            traverse_expr(visitor, expr)?;
        }
    }

    Ok(())
}

fn traverse_expr<V: Visitor>(visitor: &mut V, expr: &mut Expression) -> Result<()> {
    visitor.visit_expr(expr)?;

    match &mut expr.kind {
        ExpressionKind::Assignment(expr) => {
            traverse_expr(visitor, &mut expr.target)?;
            traverse_expr(visitor, &mut expr.value)?;
        }
        ExpressionKind::Binary(expr) => {
            traverse_expr(visitor, &mut expr.lhs)?;
            traverse_expr(visitor, &mut expr.rhs)?;
        }
        ExpressionKind::Bitcast(expr) => {
            traverse_expr(visitor, &mut expr.source)?;
            visitor.visit_type(&mut expr.target)?;
        }
        ExpressionKind::Construct(expr) => {
            visitor.visit_type(&mut expr.ty)?;

            for field in &mut expr.fields {
                traverse_expr(visitor, &mut field.value)?;
            }
        }
        ExpressionKind::Call(expr) => {
            for type_argument in &mut expr.type_arguments {
                visitor.visit_type(type_argument)?;
            }

            for argument in &mut expr.arguments {
                traverse_expr(visitor, argument)?;
            }
        }
        ExpressionKind::If(expr) => {
            for case in &mut expr.cases {
                if let Some(condition) = &mut case.condition {
                    traverse_expr(visitor, condition)?;
                }

                for stmt in &mut case.block.statements {
                    traverse_stmt(visitor, stmt)?;
                }
            }
        }
        ExpressionKind::IntrinsicCall(expr) => {
            for argument in &mut expr.arguments {
                traverse_expr(visitor, argument)?;
            }
        }
        ExpressionKind::Is(expr) => {
            traverse_expr(visitor, &mut expr.target)?;
            traverse_pattern(visitor, &mut expr.pattern)?;
        }
        ExpressionKind::Logical(expr) => {
            traverse_expr(visitor, &mut expr.lhs)?;
            traverse_expr(visitor, &mut expr.rhs)?;
        }
        ExpressionKind::Member(expr) => {
            traverse_expr(visitor, &mut expr.callee)?;
        }
        ExpressionKind::Scope(expr) => {
            for stmt in &mut expr.body {
                traverse_stmt(visitor, stmt)?;
            }

            visitor.visit_type(&mut expr.return_type)?;
        }
        ExpressionKind::Switch(expr) => {
            traverse_expr(visitor, &mut expr.operand)?;

            for (_, branch) in &mut expr.entries {
                traverse_expr(visitor, branch)?;
            }

            traverse_expr(visitor, &mut expr.fallback)?;
        }
        ExpressionKind::Variant(expr) => {
            traverse_path(visitor, &mut expr.name)?;

            for argument in &mut expr.arguments {
                traverse_expr(visitor, argument)?;
            }
        }
        ExpressionKind::Literal(_) | ExpressionKind::Variable(_) => {}
    }

    Ok(())
}

fn traverse_pattern<V: Visitor>(visitor: &mut V, pattern: &mut Pattern) -> Result<()> {
    visitor.visit_pattern(pattern)?;

    match &mut pattern.kind {
        PatternKind::Variant(pat) => {
            traverse_path(visitor, &mut pat.name)?;

            for field in &mut pat.fields {
                traverse_pattern(visitor, field)?;
            }
        }
        PatternKind::Literal(_) | PatternKind::Variable(_) | PatternKind::Wildcard => {}
    }

    Ok(())
}

fn traverse_path<V: Visitor>(visitor: &mut V, path: &mut Path) -> Result<()> {
    visitor.visit_path(path)?;

    for root in &mut path.root {
        traverse_path_segment(visitor, root)?;
    }

    traverse_path_segment(visitor, &mut path.name)
}

fn traverse_path_segment<V: Visitor>(visitor: &mut V, path: &mut PathSegment) -> Result<()> {
    match path {
        PathSegment::Namespace { .. } | PathSegment::Variant { .. } => {}
        PathSegment::Callable { type_arguments, .. } | PathSegment::Type { type_arguments, .. } => {
            for type_arg in type_arguments {
                visitor.visit_type(type_arg)?;
            }
        }
    }

    Ok(())
}
