use std::ops::ControlFlow;

use crate::*;

/// Visitor trait for traversing the HIR map.
pub trait Visitor {
    type Break;

    fn visit_node(&mut self, _node: &Node) -> ControlFlow<Self::Break> {
        ControlFlow::Continue(())
    }

    fn visit_type(&mut self, _ty: &Type) -> ControlFlow<Self::Break> {
        ControlFlow::Continue(())
    }

    fn visit_stmt(&mut self, _stmt: &Statement) -> ControlFlow<Self::Break> {
        ControlFlow::Continue(())
    }

    fn visit_expr(&mut self, _expr: &Expression) -> ControlFlow<Self::Break> {
        ControlFlow::Continue(())
    }

    fn visit_pattern(&mut self, _pattern: &Pattern) -> ControlFlow<Self::Break> {
        ControlFlow::Continue(())
    }

    fn visit_path(&mut self, _path: &Path) -> ControlFlow<Self::Break> {
        ControlFlow::Continue(())
    }

    fn visit_identifier(&mut self, _ident: &Identifier) -> ControlFlow<Self::Break> {
        ControlFlow::Continue(())
    }
}

/// Traverses the given HIR map using the provided visitor.
pub fn traverse<V: Visitor>(hir: &Map, visitor: &mut V) -> ControlFlow<V::Break> {
    for node in hir.nodes().values() {
        traverse_node(hir, visitor, node)?;
    }

    ControlFlow::Continue(())
}

pub fn traverse_node<V: Visitor>(hir: &Map, visitor: &mut V, node: &Node) -> ControlFlow<V::Break> {
    visitor.visit_node(node)?;

    match node {
        Node::Function(func) => {
            traverse_signature(hir, visitor, &func.signature)?;

            if let Some(block) = &func.block {
                for stmt in &block.statements {
                    traverse_stmt(hir, visitor, hir.expect_statement(*stmt).unwrap())?;
                }
            }
        }
        Node::Type(ty) => match ty {
            TypeDefinition::Struct(struct_def) => {
                traverse_path(hir, visitor, &struct_def.name)?;
                traverse_type_params(hir, visitor, struct_def.type_parameters.iter().copied())?;

                for field in &struct_def.fields {
                    traverse_node(hir, visitor, hir.expect_node(field.id).unwrap())?;
                }
            }
            TypeDefinition::Trait(trait_def) => {
                traverse_path(hir, visitor, &trait_def.name)?;
                traverse_type_params(hir, visitor, trait_def.type_parameters.iter().copied())?;

                for method in &trait_def.methods {
                    traverse_node(hir, visitor, hir.expect_node(method.id).unwrap())?;
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
                traverse_node(hir, visitor, hir.expect_node(method.id).unwrap())?;
            }
        }
        Node::Impl(type_impl) => {
            traverse_type(hir, visitor, &type_impl.target)?;
            traverse_type_params(hir, visitor, type_impl.type_parameters.iter().copied())?;

            for method in &type_impl.methods {
                traverse_node(hir, visitor, hir.expect_node(method.id).unwrap())?;
            }
        }
        Node::Method(method) => {
            traverse_signature(hir, visitor, &method.signature)?;

            if let Some(block) = &method.block {
                for stmt in &block.statements {
                    traverse_stmt(hir, visitor, hir.expect_statement(*stmt).unwrap())?;
                }
            }
        }
        Node::Field(field) => {
            visitor.visit_identifier(&field.name)?;
            traverse_type(hir, visitor, &field.field_type)?;

            if let Some(default_value) = field.default_value {
                traverse_expr(hir, visitor, hir.expect_expression(default_value).unwrap())?;
            }
        }
        Node::Parameter(parameter) => {
            visitor.visit_identifier(&parameter.name)?;
            traverse_type(hir, visitor, &parameter.param_type)?;
        }
        Node::TraitMethodDef(method) => {
            traverse_signature(hir, visitor, &method.signature)?;

            if let Some(block) = &method.block {
                for stmt in &block.statements {
                    traverse_stmt(hir, visitor, hir.expect_statement(*stmt).unwrap())?;
                }
            }
        }
        Node::TraitMethodImpl(method) => {
            traverse_signature(hir, visitor, &method.signature)?;

            if let Some(block) = &method.block {
                for stmt in &block.statements {
                    traverse_stmt(hir, visitor, hir.expect_statement(*stmt).unwrap())?;
                }
            }
        }
        Node::Pattern(pat) => {
            traverse_pattern(hir, visitor, pat)?;
        }
        Node::Statement(stmt) => {
            traverse_stmt(hir, visitor, stmt)?;
        }
        Node::Expression(expr) => {
            traverse_expr(hir, visitor, expr)?;
        }
        Node::TypeVariable(_) => {}
    }

    ControlFlow::Continue(())
}

pub fn traverse_signature<V: Visitor>(hir: &Map, visitor: &mut V, signature: &FnSignature) -> ControlFlow<V::Break> {
    traverse_path(hir, visitor, &signature.name)?;
    traverse_type_params(hir, visitor, signature.type_parameters.iter().copied())?;

    for param in &signature.parameters {
        visitor.visit_identifier(&param.name)?;

        traverse_type(hir, visitor, &param.param_type)?;
    }

    traverse_type(hir, visitor, &signature.return_type)?;

    ControlFlow::Continue(())
}

pub fn traverse_type_params<V: Visitor, I: Iterator<Item = NodeId>>(
    hir: &Map,
    visitor: &mut V,
    iter: I,
) -> ControlFlow<V::Break> {
    for type_param_id in iter {
        let Node::Type(TypeDefinition::TypeParameter(type_param)) = hir.expect_node(type_param_id).unwrap() else {
            continue;
        };

        visitor.visit_identifier(&type_param.name)?;

        for constraint in &type_param.constraints {
            traverse_type(hir, visitor, constraint)?;
        }
    }

    ControlFlow::Continue(())
}

pub fn traverse_stmt<V: Visitor>(hir: &Map, visitor: &mut V, stmt: &Statement) -> ControlFlow<V::Break> {
    visitor.visit_stmt(stmt)?;

    match &stmt.kind {
        StatementKind::Variable(stmt) => {
            visitor.visit_identifier(&stmt.name)?;

            if let Some(declared_type) = &stmt.declared_type {
                traverse_type(hir, visitor, declared_type)?;
            }

            traverse_expr(hir, visitor, hir.expect_expression(stmt.value).unwrap())?;
        }
        StatementKind::Break(_) | StatementKind::Continue(_) => {}
        StatementKind::Final(stmt) => {
            traverse_expr(hir, visitor, hir.expect_expression(stmt.value).unwrap())?;
        }
        StatementKind::Return(stmt) => {
            if let Some(value) = stmt.value {
                traverse_expr(hir, visitor, hir.expect_expression(value).unwrap())?;
            }
        }
        StatementKind::InfiniteLoop(stmt) => {
            for stmt in &stmt.block.statements {
                traverse_stmt(hir, visitor, hir.expect_statement(*stmt).unwrap())?;
            }
        }
        StatementKind::Expression(expr) => {
            traverse_expr(hir, visitor, hir.expect_expression(*expr).unwrap())?;
        }
    }

    ControlFlow::Continue(())
}

pub fn traverse_expr<V: Visitor>(hir: &Map, visitor: &mut V, expr: &Expression) -> ControlFlow<V::Break> {
    visitor.visit_expr(expr)?;

    match &expr.kind {
        ExpressionKind::Assignment(expr) => {
            traverse_expr(hir, visitor, hir.expect_expression(expr.target).unwrap())?;
            traverse_expr(hir, visitor, hir.expect_expression(expr.value).unwrap())?;
        }
        ExpressionKind::Cast(expr) => {
            traverse_expr(hir, visitor, hir.expect_expression(expr.source).unwrap())?;
            traverse_type(hir, visitor, &expr.target)?;
        }
        ExpressionKind::Construct(expr) => {
            traverse_path(hir, visitor, &expr.path)?;

            for field in &expr.fields {
                traverse_expr(hir, visitor, hir.expect_expression(field.value).unwrap())?;
            }
        }
        ExpressionKind::Ref(expr) => {
            traverse_expr(hir, visitor, hir.expect_expression(expr.target).unwrap())?;
        }
        ExpressionKind::Deref(expr) => {
            traverse_expr(hir, visitor, hir.expect_expression(expr.target).unwrap())?;
        }
        ExpressionKind::StaticCall(expr) => {
            traverse_path(hir, visitor, &expr.name)?;

            for argument in &expr.arguments {
                traverse_expr(hir, visitor, hir.expect_expression(*argument).unwrap())?;
            }
        }
        ExpressionKind::InstanceCall(expr) => {
            traverse_path_segment(hir, visitor, &expr.name)?;
            traverse_expr(hir, visitor, hir.expect_expression(expr.callee).unwrap())?;

            for argument in &expr.arguments {
                traverse_expr(hir, visitor, hir.expect_expression(*argument).unwrap())?;
            }
        }
        ExpressionKind::IntrinsicCall(expr) => {
            for argument in &expr.kind.arguments() {
                traverse_expr(hir, visitor, hir.expect_expression(*argument).unwrap())?;
            }
        }
        ExpressionKind::If(expr) => {
            for case in &expr.cases {
                if let Some(condition) = case.condition {
                    traverse_expr(hir, visitor, hir.expect_expression(condition).unwrap())?;
                }

                for stmt in &case.block.statements {
                    traverse_stmt(hir, visitor, hir.expect_statement(*stmt).unwrap())?;
                }
            }
        }
        ExpressionKind::Is(expr) => {
            traverse_expr(hir, visitor, hir.expect_expression(expr.target).unwrap())?;
            traverse_pattern(hir, visitor, hir.expect_pattern(expr.pattern).unwrap())?;
        }
        ExpressionKind::Member(expr) => {
            traverse_expr(hir, visitor, hir.expect_expression(expr.callee).unwrap())?;
        }
        ExpressionKind::Scope(expr) => {
            for stmt in &expr.body {
                traverse_stmt(hir, visitor, hir.expect_statement(*stmt).unwrap())?;
            }
        }
        ExpressionKind::Switch(expr) => {
            traverse_expr(hir, visitor, hir.expect_expression(expr.operand).unwrap())?;

            for case in &expr.cases {
                traverse_pattern(hir, visitor, hir.expect_pattern(case.pattern).unwrap())?;
                traverse_expr(hir, visitor, hir.expect_expression(case.branch).unwrap())?;
            }
        }
        ExpressionKind::Variant(expr) => {
            traverse_path(hir, visitor, &expr.name)?;

            for argument in &expr.arguments {
                traverse_expr(hir, visitor, hir.expect_expression(*argument).unwrap())?;
            }
        }
        ExpressionKind::Literal(_) | ExpressionKind::Variable(_) | ExpressionKind::Missing => {}
    }

    ControlFlow::Continue(())
}

pub fn traverse_pattern<V: Visitor>(hir: &Map, visitor: &mut V, pattern: &Pattern) -> ControlFlow<V::Break> {
    visitor.visit_pattern(pattern)?;

    match &pattern.kind {
        PatternKind::Identifier(ident) => {
            visitor.visit_identifier(&ident.name)?;
        }
        PatternKind::Variant(pat) => {
            traverse_path(hir, visitor, &pat.name)?;

            for &field in &pat.fields {
                traverse_pattern(hir, visitor, hir.expect_pattern(field).unwrap())?;
            }
        }
        PatternKind::Literal(_) | PatternKind::Wildcard(_) | PatternKind::Missing => {}
    }

    ControlFlow::Continue(())
}

pub fn traverse_type<V: Visitor>(hir: &Map, visitor: &mut V, ty: &Type) -> ControlFlow<V::Break> {
    visitor.visit_type(ty)?;

    traverse_path(hir, visitor, &ty.name)
}

pub fn traverse_path<V: Visitor>(hir: &Map, visitor: &mut V, path: &Path) -> ControlFlow<V::Break> {
    visitor.visit_path(path)?;

    for root in &path.root {
        traverse_path_segment(hir, visitor, root)?;
    }

    traverse_path_segment(hir, visitor, &path.name)
}

pub fn traverse_path_segment<V: Visitor>(hir: &Map, visitor: &mut V, path: &PathSegment) -> ControlFlow<V::Break> {
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
        PathSegment::Missing => {}
    }

    ControlFlow::Continue(())
}

pub trait Visit {
    fn visit<V: Visitor>(&self, hir: &Map, visitor: &mut V) -> ControlFlow<V::Break>;
}

impl Visit for Node {
    fn visit<V: Visitor>(&self, hir: &Map, visitor: &mut V) -> ControlFlow<V::Break> {
        traverse_node(hir, visitor, self)
    }
}

impl Visit for Statement {
    fn visit<V: Visitor>(&self, hir: &Map, visitor: &mut V) -> ControlFlow<V::Break> {
        traverse_stmt(hir, visitor, self)
    }
}

impl Visit for Expression {
    fn visit<V: Visitor>(&self, hir: &Map, visitor: &mut V) -> ControlFlow<V::Break> {
        traverse_expr(hir, visitor, self)
    }
}

impl Visit for Pattern {
    fn visit<V: Visitor>(&self, hir: &Map, visitor: &mut V) -> ControlFlow<V::Break> {
        traverse_pattern(hir, visitor, self)
    }
}

impl Visit for Type {
    fn visit<V: Visitor>(&self, hir: &Map, visitor: &mut V) -> ControlFlow<V::Break> {
        traverse_type(hir, visitor, self)
    }
}

impl Visit for Path {
    fn visit<V: Visitor>(&self, hir: &Map, visitor: &mut V) -> ControlFlow<V::Break> {
        traverse_path(hir, visitor, self)
    }
}

impl Visit for PathSegment {
    fn visit<V: Visitor>(&self, hir: &Map, visitor: &mut V) -> ControlFlow<V::Break> {
        traverse_path_segment(hir, visitor, self)
    }
}

impl Visit for Identifier {
    fn visit<V: Visitor>(&self, _hir: &Map, visitor: &mut V) -> ControlFlow<V::Break> {
        visitor.visit_identifier(self)
    }
}
