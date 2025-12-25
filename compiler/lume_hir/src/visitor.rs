use lume_errors::Result;

use crate::*;

/// Visitor trait for traversing the HIR map.
pub trait Visitor {
    fn visit_node(&mut self, _node: &Node) -> Result<()> {
        Ok(())
    }

    fn visit_type(&mut self, _ty: &Type) -> Result<()> {
        Ok(())
    }

    fn visit_stmt(&mut self, _stmt: &Statement) -> Result<()> {
        Ok(())
    }

    fn visit_expr(&mut self, _expr: &Expression) -> Result<()> {
        Ok(())
    }

    fn visit_pattern(&mut self, _pattern: &Pattern) -> Result<()> {
        Ok(())
    }

    fn visit_path(&mut self, _path: &Path) -> Result<()> {
        Ok(())
    }

    fn visit_identifier(&mut self, _ident: &Identifier) -> Result<()> {
        Ok(())
    }
}

/// Traverses the given HIR map using the provided visitor.
pub fn traverse<V: Visitor>(hir: &Map, visitor: &mut V) -> Result<()> {
    for node in hir.nodes().values() {
        traverse_node(hir, visitor, node)?;
    }

    Ok(())
}

fn traverse_node<V: Visitor>(hir: &Map, visitor: &mut V, node: &Node) -> Result<()> {
    visitor.visit_node(node)?;

    match node {
        Node::Function(n) => {
            traverse_path(hir, visitor, &n.name)?;
            traverse_type_params(hir, visitor, n.type_parameters.iter().copied())?;

            for param in &n.parameters {
                visitor.visit_identifier(&param.name)?;

                traverse_type(hir, visitor, &param.param_type)?;
            }

            if let Some(block) = &n.block {
                for stmt in &block.statements {
                    traverse_stmt(hir, visitor, hir.expect_statement(*stmt)?)?;
                }
            }

            traverse_type(hir, visitor, &n.return_type)?;
        }
        Node::Type(ty) => match ty {
            TypeDefinition::Struct(struct_def) => {
                traverse_path(hir, visitor, &struct_def.name)?;
                traverse_type_params(hir, visitor, struct_def.type_parameters.iter().copied())?;

                for field in &struct_def.fields {
                    visitor.visit_identifier(&field.name)?;
                    traverse_type(hir, visitor, &field.field_type)?;

                    if let Some(default_value) = &field.default_value {
                        traverse_expr(hir, visitor, hir.expect_expression(*default_value)?)?;
                    }
                }
            }
            TypeDefinition::Trait(trait_def) => {
                traverse_path(hir, visitor, &trait_def.name)?;
                traverse_type_params(hir, visitor, trait_def.type_parameters.iter().copied())?;

                for method in &trait_def.methods {
                    visitor.visit_identifier(&method.name)?;
                    traverse_type_params(hir, visitor, method.type_parameters.iter().copied())?;

                    for param in &method.parameters {
                        visitor.visit_identifier(&param.name)?;

                        traverse_type(hir, visitor, &param.param_type)?;
                    }

                    if let Some(block) = &method.block {
                        for stmt in &block.statements {
                            traverse_stmt(hir, visitor, hir.expect_statement(*stmt)?)?;
                        }
                    }

                    traverse_type(hir, visitor, &method.return_type)?;
                }
            }
            TypeDefinition::Enum(enum_def) => {
                traverse_path(hir, visitor, &enum_def.name)?;
                traverse_type_params(hir, visitor, enum_def.type_parameters.iter().copied())?;

                for case in &enum_def.cases {
                    traverse_path(hir, visitor, &case.name)?;

                    for param in &case.parameters {
                        traverse_type(hir, visitor, param)?;
                    }
                }
            }
            TypeDefinition::TypeParameter(type_parameter) => {
                visitor.visit_identifier(&type_parameter.name)?;

                for constraint in &type_parameter.constraints {
                    traverse_type(hir, visitor, constraint)?;
                }
            }
        },
        Node::TraitImpl(trait_impl) => {
            traverse_type(hir, visitor, &trait_impl.name)?;
            traverse_type(hir, visitor, &trait_impl.target)?;
            traverse_type_params(hir, visitor, trait_impl.type_parameters.iter().copied())?;

            for method in &trait_impl.methods {
                visitor.visit_identifier(&method.name)?;
                traverse_type_params(hir, visitor, method.type_parameters.iter().copied())?;

                for param in &method.parameters {
                    visitor.visit_identifier(&param.name)?;

                    traverse_type(hir, visitor, &param.param_type)?;
                }

                if let Some(block) = &method.block {
                    for stmt in &block.statements {
                        traverse_stmt(hir, visitor, hir.expect_statement(*stmt)?)?;
                    }
                }

                traverse_type(hir, visitor, &method.return_type)?;
            }
        }
        Node::Impl(type_impl) => {
            traverse_type(hir, visitor, &type_impl.target)?;
            traverse_type_params(hir, visitor, type_impl.type_parameters.iter().copied())?;

            for method in &type_impl.methods {
                visitor.visit_identifier(&method.name)?;
                traverse_type_params(hir, visitor, method.type_parameters.iter().copied())?;

                for param in &method.parameters {
                    visitor.visit_identifier(&param.name)?;

                    traverse_type(hir, visitor, &param.param_type)?;
                }

                if let Some(block) = &method.block {
                    for stmt in &block.statements {
                        traverse_stmt(hir, visitor, hir.expect_statement(*stmt)?)?;
                    }
                }

                traverse_type(hir, visitor, &method.return_type)?;
            }
        }
        Node::Field(_)
        | Node::Method(_)
        | Node::TraitMethodDef(_)
        | Node::TraitMethodImpl(_)
        | Node::Pattern(_)
        | Node::Statement(_)
        | Node::Expression(_) => {}
    }

    Ok(())
}

fn traverse_type_params<V: Visitor, I: Iterator<Item = NodeId>>(hir: &Map, visitor: &mut V, iter: I) -> Result<()> {
    for type_param_id in iter {
        let Node::Type(TypeDefinition::TypeParameter(type_param)) = hir.expect_node(type_param_id)? else {
            continue;
        };

        visitor.visit_identifier(&type_param.name)?;

        for constraint in &type_param.constraints {
            traverse_type(hir, visitor, constraint)?;
        }
    }

    Ok(())
}

fn traverse_stmt<V: Visitor>(hir: &Map, visitor: &mut V, stmt: &Statement) -> Result<()> {
    visitor.visit_stmt(stmt)?;

    match &stmt.kind {
        StatementKind::Variable(stmt) => {
            visitor.visit_identifier(&stmt.name)?;

            if let Some(declared_type) = &stmt.declared_type {
                traverse_type(hir, visitor, declared_type)?;
            }

            traverse_expr(hir, visitor, hir.expect_expression(stmt.value)?)?;
        }
        StatementKind::Break(_) | StatementKind::Continue(_) => {}
        StatementKind::Final(stmt) => {
            traverse_expr(hir, visitor, hir.expect_expression(stmt.value)?)?;
        }
        StatementKind::Return(stmt) => {
            if let Some(value) = stmt.value {
                traverse_expr(hir, visitor, hir.expect_expression(value)?)?;
            }
        }
        StatementKind::InfiniteLoop(stmt) => {
            for stmt in &stmt.block.statements {
                traverse_stmt(hir, visitor, hir.expect_statement(*stmt)?)?;
            }
        }
        StatementKind::IteratorLoop(stmt) => {
            traverse_expr(hir, visitor, hir.expect_expression(stmt.collection)?)?;

            for stmt in &stmt.block.statements {
                traverse_stmt(hir, visitor, hir.expect_statement(*stmt)?)?;
            }
        }
        StatementKind::Expression(expr) => {
            traverse_expr(hir, visitor, hir.expect_expression(*expr)?)?;
        }
    }

    Ok(())
}

fn traverse_expr<V: Visitor>(hir: &Map, visitor: &mut V, expr: &Expression) -> Result<()> {
    visitor.visit_expr(expr)?;

    match &expr.kind {
        ExpressionKind::Assignment(expr) => {
            traverse_expr(hir, visitor, hir.expect_expression(expr.target)?)?;
            traverse_expr(hir, visitor, hir.expect_expression(expr.value)?)?;
        }
        ExpressionKind::Cast(expr) => {
            traverse_expr(hir, visitor, hir.expect_expression(expr.source)?)?;
            traverse_type(hir, visitor, &expr.target)?;
        }
        ExpressionKind::Construct(expr) => {
            traverse_path(hir, visitor, &expr.path)?;

            for field in &expr.fields {
                traverse_expr(hir, visitor, hir.expect_expression(field.value)?)?;
            }
        }
        ExpressionKind::StaticCall(expr) => {
            traverse_path(hir, visitor, &expr.name)?;

            for argument in &expr.arguments {
                traverse_expr(hir, visitor, hir.expect_expression(*argument)?)?;
            }
        }
        ExpressionKind::InstanceCall(expr) => {
            traverse_path_segment(hir, visitor, &expr.name)?;
            traverse_expr(hir, visitor, hir.expect_expression(expr.callee)?)?;

            for argument in &expr.arguments {
                traverse_expr(hir, visitor, hir.expect_expression(*argument)?)?;
            }
        }
        ExpressionKind::IntrinsicCall(expr) => {
            for argument in &expr.kind.arguments() {
                traverse_expr(hir, visitor, hir.expect_expression(*argument)?)?;
            }
        }
        ExpressionKind::If(expr) => {
            for case in &expr.cases {
                if let Some(condition) = case.condition {
                    traverse_expr(hir, visitor, hir.expect_expression(condition)?)?;
                }

                for stmt in &case.block.statements {
                    traverse_stmt(hir, visitor, hir.expect_statement(*stmt)?)?;
                }
            }
        }
        ExpressionKind::Is(expr) => {
            traverse_expr(hir, visitor, hir.expect_expression(expr.target)?)?;
            traverse_pattern(hir, visitor, &expr.pattern)?;
        }
        ExpressionKind::Member(expr) => {
            traverse_expr(hir, visitor, hir.expect_expression(expr.callee)?)?;
        }
        ExpressionKind::Scope(expr) => {
            for stmt in &expr.body {
                traverse_stmt(hir, visitor, hir.expect_statement(*stmt)?)?;
            }
        }
        ExpressionKind::Switch(expr) => {
            traverse_expr(hir, visitor, hir.expect_expression(expr.operand)?)?;

            for case in &expr.cases {
                traverse_pattern(hir, visitor, &case.pattern)?;
                traverse_expr(hir, visitor, hir.expect_expression(case.branch)?)?;
            }
        }
        ExpressionKind::Variant(expr) => {
            traverse_path(hir, visitor, &expr.name)?;

            for argument in &expr.arguments {
                traverse_expr(hir, visitor, hir.expect_expression(*argument)?)?;
            }
        }
        ExpressionKind::Literal(_) | ExpressionKind::Variable(_) => {}
    }

    Ok(())
}

fn traverse_pattern<V: Visitor>(hir: &Map, visitor: &mut V, pattern: &Pattern) -> Result<()> {
    visitor.visit_pattern(pattern)?;

    match &pattern.kind {
        PatternKind::Identifier(ident) => {
            visitor.visit_identifier(&ident.name)?;
        }
        PatternKind::Literal(pat) => {
            traverse_expr(hir, visitor, hir.expect_expression(pat.literal.id)?)?;
        }
        PatternKind::Variant(pat) => {
            traverse_path(hir, visitor, &pat.name)?;

            for field in &pat.fields {
                traverse_pattern(hir, visitor, field)?;
            }
        }
        PatternKind::Wildcard(_) => {}
    }

    Ok(())
}

fn traverse_type<V: Visitor>(hir: &Map, visitor: &mut V, ty: &Type) -> Result<()> {
    visitor.visit_type(ty)?;

    traverse_path(hir, visitor, &ty.name)
}

fn traverse_path<V: Visitor>(hir: &Map, visitor: &mut V, path: &Path) -> Result<()> {
    visitor.visit_path(path)?;

    for root in &path.root {
        traverse_path_segment(hir, visitor, root)?;
    }

    traverse_path_segment(hir, visitor, &path.name)
}

fn traverse_path_segment<V: Visitor>(hir: &Map, visitor: &mut V, path: &PathSegment) -> Result<()> {
    match path {
        PathSegment::Namespace { name } | PathSegment::Variant { name, .. } => {
            visitor.visit_identifier(name)?;
        }
        PathSegment::Callable { name, bound_types, .. } | PathSegment::Type { name, bound_types, .. } => {
            visitor.visit_identifier(name)?;

            for type_arg in bound_types {
                traverse_type(hir, visitor, type_arg)?;
            }
        }
    }

    Ok(())
}
