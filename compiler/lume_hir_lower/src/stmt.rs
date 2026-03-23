use crate::*;

/*
static AS_ITERATOR_PATH: LazyLock<lume_ast::Path> = LazyLock::new(|| {
    lume_ast::Path::with_root(
        lume_ast::ast_type_path!(std::iter::AsIterator),
        lume_ast::PathSegment::callable("as_iter"),
    )
});

static ITERATOR_NEXT_PATH: LazyLock<lume_ast::Path> = LazyLock::new(|| {
    lume_ast::Path::with_root(
        lume_ast::ast_type_path!(std::iter::Iterator),
        lume_ast::PathSegment::callable("next"),
    )
});

static OPTIONAL_SOME_PATH: LazyLock<lume_ast::Path> = LazyLock::new(|| {
    lume_ast::Path::with_root(
        lume_ast::ast_type_path!(std::Optional),
        lume_ast::PathSegment::variant("Some"),
    )
});

static OPTIONAL_NONE_PATH: LazyLock<lume_ast::Path> = LazyLock::new(|| {
    lume_ast::Path::with_root(
        lume_ast::ast_type_path!(std::Optional),
        lume_ast::PathSegment::variant("None"),
    )
});
 */

impl LoweringContext<'_> {
    /// Lowers the given AST block into a HIR block, within an nested scope.
    #[tracing::instrument(level = "DEBUG", skip_all)]
    pub(super) fn block(&mut self, expr: lume_ast::Block) -> lume_hir::Block {
        lume_hir::with_frame!(self.current_locals, || {
            let id = self.next_node_id();
            let statements = self.statements(expr.stmt());
            let location = self.location(expr.location());

            lume_hir::Block {
                id,
                statements,
                location,
            }
        })
    }

    /// Lowers the given AST block into a HIR block, within an nested scope.
    #[tracing::instrument(level = "DEBUG", skip_all)]
    pub(super) fn block_opt(&mut self, block: Option<lume_ast::Block>) -> lume_hir::Block {
        match block {
            Some(block) => self.block(block),
            None => lume_hir::Block::missing(),
        }
    }

    /// Lowers the given AST block into a HIR block, within an isolated scope.
    ///
    /// This functions much like [`block`], but pushes a boundary into the
    /// symbol table, separating local variables within the block from the
    /// parent scope.
    #[tracing::instrument(level = "DEBUG", skip_all)]
    pub(super) fn isolated_block(&mut self, expr: lume_ast::Block, params: &[lume_hir::Parameter]) -> lume_hir::Block {
        lume_hir::with_boundary!(self.current_locals, || {
            for param in params {
                self.current_locals
                    .define(param.name.to_string(), lume_hir::VariableSource::Parameter(param.id));
            }

            let id = self.next_node_id();
            let statements = self.statements(expr.stmt());
            let location = self.location(expr.location());

            lume_hir::Block {
                id,
                statements,
                location,
            }
        })
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    pub(super) fn statements<I>(&mut self, statements: I) -> Vec<lume_span::NodeId>
    where
        I: IntoIterator<Item = lume_ast::Stmt>,
    {
        statements
            .into_iter()
            .filter_map(|expr| match self.statement(expr) {
                Ok(e) => Some(e),
                Err(err) => {
                    self.dcx.emit(err);
                    None
                }
            })
            .collect::<Vec<_>>()
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn statement(&mut self, expr: lume_ast::Stmt) -> Result<lume_span::NodeId> {
        let stmt = match expr {
            lume_ast::Stmt::LetStmt(s) => self.stmt_variable(s),
            lume_ast::Stmt::BreakStmt(s) => self.stmt_break(s),
            lume_ast::Stmt::ContinueStmt(s) => self.stmt_continue(s),
            lume_ast::Stmt::FinalStmt(s) => self.stmt_final(s),
            lume_ast::Stmt::ReturnStmt(s) => self.stmt_return(s),
            lume_ast::Stmt::LoopStmt(e) => self.stmt_infinite_loop(e),
            lume_ast::Stmt::ForStmt(e) => self.stmt_iterator_loop(e),
            lume_ast::Stmt::WhileStmt(e) => self.stmt_predicate_loop(e),
            lume_ast::Stmt::ExprStmt(s) => self.stmt_expression(s),
        };

        let id = stmt.id;

        self.map.nodes.insert(id, lume_hir::Node::Statement(stmt));

        Ok(id)
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn stmt_variable(&mut self, statement: lume_ast::LetStmt) -> lume_hir::Statement {
        let id = self.next_node_id();
        let name = self.ident_opt(statement.name());
        let declared_type = statement.ty().map(|t| self.hir_type(t));
        let value = self.opt_expression(statement.expr());
        let location = self.location(statement.location());

        let decl = lume_hir::VariableDeclaration {
            id,
            name: name.clone(),
            declared_type,
            value,
            location,
        };

        self.current_locals
            .define(name.to_string(), lume_hir::VariableSource::Variable(decl.id));

        lume_hir::Statement {
            id,
            location,
            kind: lume_hir::StatementKind::Variable(decl),
        }
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn stmt_break(&mut self, statement: lume_ast::BreakStmt) -> lume_hir::Statement {
        let id = self.next_node_id();
        let location = self.location(statement.location());

        lume_hir::Statement {
            id,
            location,
            kind: lume_hir::StatementKind::Break(lume_hir::Break { id, location }),
        }
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn stmt_continue(&mut self, statement: lume_ast::ContinueStmt) -> lume_hir::Statement {
        let id = self.next_node_id();
        let location = self.location(statement.location());

        lume_hir::Statement {
            id,
            location,
            kind: lume_hir::StatementKind::Continue(lume_hir::Continue { id, location }),
        }
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn stmt_final(&mut self, statement: lume_ast::FinalStmt) -> lume_hir::Statement {
        let id = self.next_node_id();
        let value = self.opt_expression(statement.expr());
        let location = self.location(statement.location());

        lume_hir::Statement {
            id,
            location,
            kind: lume_hir::StatementKind::Final(lume_hir::Final { id, value, location }),
        }
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn stmt_return(&mut self, statement: lume_ast::ReturnStmt) -> lume_hir::Statement {
        let id = self.next_node_id();
        let value = statement.expr().map(|expr| self.expression(expr));
        let location = self.location(statement.location());

        lume_hir::Statement {
            id,
            location,
            kind: lume_hir::StatementKind::Return(lume_hir::Return { id, value, location }),
        }
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn stmt_infinite_loop(&mut self, expr: lume_ast::LoopStmt) -> lume_hir::Statement {
        let id = self.next_node_id();
        let location = self.location(expr.location());
        let block = self.block_opt(expr.block());

        lume_hir::Statement {
            id,
            location,
            kind: lume_hir::StatementKind::InfiniteLoop(lume_hir::InfiniteLoop { id, block, location }),
        }
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn stmt_iterator_loop(&mut self, expr: lume_ast::ForStmt) -> lume_hir::Statement {
        const VAR_COLLECTION_NAME: &str = "__collection";
        const VAR_ITERATOR_NAME: &str = "__iter";

        let stmt = crate::make::parse_from_text::<lume_ast::ExprStmt>(
            &format!(
                "{{
                    let {collection_var} = {collection};
                    let {iter_var} = std::iter::AsIterator::as_iter({collection_var});

                    loop {{
                        switch std::iter::Iterator::next({iter_var}) {{
                            std::Optional::Some({pattern}) => {{
                                {block}
                            }},
                            std::Optional::None => {{ break; }},
                        }}
                    }}
                }}",
                collection_var = VAR_COLLECTION_NAME,
                iter_var = VAR_ITERATOR_NAME,
                collection = expr
                    .collection()
                    .map_or_else(|| String::from("[]"), |condition| condition.as_text()),
                pattern = expr
                    .pattern()
                    .map_or_else(|| String::from("[missing]"), |pat| pat.as_text()),
                block = expr.block().map_or_else(|| String::from("{}"), |block| block.as_text()),
            ),
            lume_parser::Target::Statement,
        );

        self.stmt_expression(stmt)

        /*
        // `let $collection = <collection>;`
        let collection_variable_decl = {
            lume_ast::Statement::VariableDeclaration(Box::new(lume_ast::VariableDeclaration {
                name: lume_ast::Identifier::from(VAR_COLLECTION_NAME),
                variable_type: None,
                value: expr.collection,
                location: expr.location.clone(),
            }))
        };

        // `let $iter = std::iter::AsIterator::as_iter($collection);`
        let iter_variable_decl = {
            // `$collection`
            let collection_ref = lume_ast::Expression::Variable(Box::new(lume_ast::Variable {
                name: lume_ast::Identifier::from(VAR_COLLECTION_NAME),
            }));

            // `std::iter::AsIterator::as_iter($collection)`
            let var_value = lume_ast::Expression::Call(Box::new(lume_ast::Call {
                callee: None,
                name: AS_ITERATOR_PATH.clone(),
                arguments: vec![collection_ref],
                location: expr.location.clone(),
            }));

            lume_ast::Statement::VariableDeclaration(Box::new(lume_ast::VariableDeclaration {
                name: lume_ast::Identifier::from(VAR_ITERATOR_NAME),
                variable_type: None,
                value: var_value,
                location: expr.location.clone(),
            }))
        };

        // `std::Optional::Some(<pattern>) => <body>`
        let some_case = {
            let some_pattern = {
                let some_subpattern = lume_ast::Pattern::Identifier(expr.pattern);

                lume_ast::Pattern::Variant(lume_ast::VariantPattern {
                    name: OPTIONAL_SOME_PATH.clone(),
                    fields: vec![some_subpattern],
                    location: expr.location.clone(),
                })
            };

            let some_branch = {
                lume_ast::Expression::Scope(Box::new(lume_ast::Scope {
                    body: expr.block.statements,
                    location: expr.block.location.clone(),
                }))
            };

            lume_ast::SwitchCase {
                pattern: some_pattern,
                branch: some_branch,
                location: expr.location.clone(),
            }
        };

        // `std::Optional::None => { break; }`
        let none_case = {
            let none_pattern = lume_ast::Pattern::Variant(lume_ast::VariantPattern {
                name: OPTIONAL_NONE_PATH.clone(),
                fields: Vec::new(),
                location: expr.location.clone(),
            });

            let break_stmt = lume_ast::Statement::Break(Box::new(lume_ast::Break {
                location: expr.location.clone(),
            }));

            let none_branch = {
                lume_ast::Expression::Scope(Box::new(lume_ast::Scope {
                    body: vec![break_stmt],
                    location: expr.location.clone(),
                }))
            };

            lume_ast::SwitchCase {
                pattern: none_pattern,
                branch: none_branch,
                location: expr.location.clone(),
            }
        };

        let switch_expr = {
            let iter_ref = lume_ast::Expression::Variable(Box::new(lume_ast::Variable {
                name: lume_ast::Identifier::from(VAR_ITERATOR_NAME),
            }));

            let iter_next_call = lume_ast::Expression::Call(Box::new(lume_ast::Call {
                callee: None,
                name: ITERATOR_NEXT_PATH.clone(),
                arguments: vec![iter_ref],
                location: expr.location.clone(),
            }));

            lume_ast::Expression::Switch(Box::new(lume_ast::Switch {
                operand: iter_next_call,
                cases: vec![some_case, none_case],
                location: expr.location.clone(),
            }))
        };

        let loop_stmt = {
            let switch_stmt = lume_ast::Statement::Expression(Box::new(switch_expr));

            lume_ast::Statement::InfiniteLoop(Box::new(lume_ast::InfiniteLoop {
                block: lume_ast::Block {
                    statements: vec![switch_stmt],
                    location: expr.block.location.clone(),
                },
                location: expr.block.location.clone(),
            }))
        };

        self.stmt_expression(lume_ast::Expression::Scope(Box::new(lume_ast::Scope {
            body: vec![collection_variable_decl, iter_variable_decl, loop_stmt],
            location: expr.location(),
        })))
         */
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn stmt_predicate_loop(&mut self, expr: lume_ast::WhileStmt) -> lume_hir::Statement {
        let stmt = crate::make::parse_from_text::<lume_ast::LoopStmt>(
            &format!(
                "loop {{
                    if {condition} {{
                        {block}
                    }} else {{
                        break;
                    }}
                }}",
                condition = expr
                    .condition()
                    .map_or_else(|| String::from("false"), |condition| condition.as_text()),
                block = expr.block().map_or_else(String::new, |block| block.as_text()),
            ),
            lume_parser::Target::Statement,
        );

        self.stmt_infinite_loop(stmt)
    }

    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn stmt_expression(&mut self, expr: lume_ast::ExprStmt) -> lume_hir::Statement {
        let id = self.next_node_id();
        let expr = self.opt_expression(expr.expr());
        let location = self.map.expression(expr).unwrap().location;

        lume_hir::Statement {
            id,
            location,
            kind: lume_hir::StatementKind::Expression(expr),
        }
    }
}
