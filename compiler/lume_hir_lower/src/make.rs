use std::sync::Arc;

use lume_ast::AstNode;
use lume_parser::Target;

use crate::*;

#[track_caller]
pub(crate) fn parse_from_text<N: AstNode>(text: &str, target: Target) -> N {
    let parse = lume_parser::Parser::from_source(Arc::new(lume_span::SourceFile::internal(text)))
        .map(|parser| parser.parse(target))
        .expect("failed to tokenize input");

    let node: N = if let Some(it) = parse.syntax().descendants().find_map(N::cast) {
        it
    } else {
        let node = std::any::type_name::<N>();
        panic!("Failed to make ast node `{node}` from text {text}")
    };

    let node = node.clone_subtree();
    assert_eq!(node.syntax().text_range().start(), 0.into());

    node
}

impl LoweringContext<'_> {
    pub(crate) fn alloc_stmt(&mut self, stmt: lume_hir::Statement) -> NodeId {
        let id = stmt.id;
        let existing = self.map.nodes.insert(id, lume_hir::Node::Statement(stmt));
        assert!(existing.is_none(), "bug!: overwrite HIR node {id:?} with statement");

        id
    }

    pub(crate) fn alloc_final_stmt(&mut self, expr: NodeId) -> NodeId {
        let id = self.next_node_id();
        let location = self.map.expect_node(expr).unwrap().location();

        self.alloc_stmt(lume_hir::Statement {
            id,
            location,
            kind: lume_hir::StatementKind::Final(lume_hir::Final {
                id,
                value: expr,
                location,
            }),
        })
    }

    pub(crate) fn alloc_expr_stmt(&mut self, expr: NodeId) -> NodeId {
        let id = self.next_node_id();
        let location = self.map.expect_node(expr).unwrap().location();

        self.alloc_stmt(lume_hir::Statement {
            id,
            location,
            kind: lume_hir::StatementKind::Expression(expr),
        })
    }

    pub(crate) fn alloc_break(&mut self, location: Location) -> NodeId {
        let id = self.next_node_id();

        self.alloc_stmt(lume_hir::Statement {
            id,
            location,
            kind: lume_hir::StatementKind::Break(lume_hir::Break { id, location }),
        })
    }

    /// Allocates a new variable within the current scope.
    pub(crate) fn alloc_variable_decl<N>(
        &mut self,
        name: N,
        value: NodeId,
        declared_type: Option<lume_hir::Type>,
        location: Location,
    ) -> NodeId
    where
        N: Into<lume_hir::Identifier>,
    {
        let id = self.next_node_id();
        let name = name.into();

        let decl = lume_hir::VariableDeclaration {
            id,
            name: name.clone(),
            declared_type,
            value,
            location,
        };

        self.current_locals
            .define(name.to_string(), lume_hir::VariableSource::Variable(decl.id));

        self.alloc_stmt(lume_hir::Statement {
            id,
            location,
            kind: lume_hir::StatementKind::Variable(decl),
        })
    }

    /// Allocates a new variable within the current scope.
    pub(crate) fn alloc_infinite_loop(&mut self, block: lume_hir::Block, location: Location) -> NodeId {
        let id = self.next_node_id();

        self.alloc_stmt(lume_hir::Statement {
            id,
            location,
            kind: lume_hir::StatementKind::InfiniteLoop(lume_hir::InfiniteLoop { id, block, location }),
        })
    }
}

impl LoweringContext<'_> {
    pub(crate) fn alloc_expr(&mut self, expr: lume_hir::Expression) -> NodeId {
        let id = expr.id;
        let existing = self.map.nodes.insert(id, lume_hir::Node::Expression(expr));
        assert!(existing.is_none(), "bug!: overwrite HIR node {id:?} with expression");

        id
    }

    /// Allocates a new variable reference expression within the current scope,
    /// which references the given name.
    pub(crate) fn alloc_variable_ref<N>(&mut self, name: N, location: Location) -> NodeId
    where
        N: Into<lume_hir::Identifier>,
    {
        let id = self.next_node_id();
        let name = name.into();

        let Some(reference) = self.current_locals.retrieve(&name.name).copied() else {
            self.dcx.emit_and_push(
                crate::errors::UndeclaredVariable {
                    source: self.current_file().clone(),
                    range: location.index.clone(),
                    name: name.to_string(),
                }
                .into(),
            );

            return self.missing_expr(Some(id));
        };

        self.alloc_expr(lume_hir::Expression {
            id,
            location,
            kind: lume_hir::ExpressionKind::Variable(lume_hir::Variable {
                id,
                name,
                reference,
                location,
            }),
        })
    }

    pub(crate) fn alloc_static_call<I>(&mut self, path: lume_hir::Path, arguments: I, location: Location) -> NodeId
    where
        I: IntoIterator<Item = NodeId>,
    {
        let id = self.next_node_id();

        self.alloc_expr(lume_hir::Expression {
            id,
            location,
            kind: lume_hir::ExpressionKind::StaticCall(lume_hir::StaticCall {
                id,
                name: path,
                arguments: arguments.into_iter().collect(),
                location,
            }),
        })
    }

    pub(crate) fn alloc_if_conditional<C>(&mut self, conditions: C, location: Location) -> NodeId
    where
        C: IntoIterator<Item = lume_hir::Condition>,
    {
        let id = self.next_node_id();

        self.alloc_expr(lume_hir::Expression {
            id,
            location,
            kind: lume_hir::ExpressionKind::If(lume_hir::If {
                id,
                cases: conditions.into_iter().collect(),
                location,
            }),
        })
    }

    pub(crate) fn alloc_switch_expr<A>(&mut self, operand: NodeId, arms: A, location: Location) -> NodeId
    where
        A: IntoIterator<Item = lume_hir::SwitchCase>,
    {
        let id = self.next_node_id();

        self.alloc_expr(lume_hir::Expression {
            id,
            location,
            kind: lume_hir::ExpressionKind::Switch(lume_hir::Switch {
                id,
                operand,
                cases: arms.into_iter().collect(),
                location,
            }),
        })
    }

    pub(crate) fn alloc_within_scope<I, F>(&mut self, body: F, location: Location) -> NodeId
    where
        I: IntoIterator<Item = NodeId>,
        F: FnOnce(&mut Self) -> I,
    {
        self.current_locals.push_frame();

        let id = self.next_node_id();
        let body = body(self).into_iter().collect();

        self.current_locals.pop_frame();

        self.alloc_expr(lume_hir::Expression {
            id,
            location,
            kind: lume_hir::ExpressionKind::Scope(lume_hir::Scope { id, body, location }),
        })
    }
}

impl LoweringContext<'_> {
    pub(crate) fn alloc_pat(&mut self, pat: lume_hir::Pattern) -> NodeId {
        let id = pat.id;
        let existing = self.map.nodes.insert(id, lume_hir::Node::Pattern(pat));
        assert!(existing.is_none(), "bug!: overwrite HIR node {id:?} with pattern");

        id
    }

    pub(crate) fn alloc_pat_ident<N>(&mut self, name: N, location: Location) -> NodeId
    where
        N: Into<lume_hir::Identifier>,
    {
        let id = self.next_node_id();
        let name = name.into();

        self.alloc_pat(lume_hir::Pattern {
            id,
            kind: lume_hir::PatternKind::Identifier(lume_hir::IdentifierPattern {
                name: name.clone(),
                location,
            }),
            location,
        });

        self.current_locals
            .define(name.to_string(), lume_hir::VariableSource::Pattern(id));

        id
    }

    pub(crate) fn alloc_pat_variant<F>(&mut self, name: lume_hir::Path, fields: F, location: Location) -> NodeId
    where
        F: IntoIterator<Item = NodeId>,
    {
        let id = self.next_node_id();

        self.alloc_pat(lume_hir::Pattern {
            id,
            kind: lume_hir::PatternKind::Variant(lume_hir::VariantPattern {
                name,
                fields: fields.into_iter().collect(),
                location,
            }),
            location,
        })
    }
}
