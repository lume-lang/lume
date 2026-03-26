use crate::*;

static AS_ITERATOR_PATH: LazyLock<lume_hir::Path> = LazyLock::new(|| {
    lume_hir::Path::with_root(
        lume_hir::hir_type_path!(std::iter::AsIterator),
        lume_hir::PathSegment::callable("as_iter"),
    )
});

static ITERATOR_NEXT_PATH: LazyLock<lume_hir::Path> = LazyLock::new(|| {
    lume_hir::Path::with_root(
        lume_hir::hir_type_path!(std::iter::Iterator),
        lume_hir::PathSegment::callable("next"),
    )
});

static OPTIONAL_SOME_PATH: LazyLock<lume_hir::Path> = LazyLock::new(|| {
    lume_hir::Path::with_root(
        lume_hir::hir_type_path!(std::Optional),
        lume_hir::PathSegment::variant("Some"),
    )
});

static OPTIONAL_NONE_PATH: LazyLock<lume_hir::Path> = LazyLock::new(|| {
    lume_hir::Path::with_root(
        lume_hir::hir_type_path!(std::Optional),
        lume_hir::PathSegment::variant("None"),
    )
});

impl LoweringContext<'_> {
    /// Desugars `while` loops into a conditional loop with a `break` statement.
    ///
    /// This method lowers `while` loops:
    /// ```lm
    /// while <condition> {
    ///     <body>
    /// }
    /// ```
    /// into the following:
    /// ```lm
    /// loop {
    ///     if <condition> {
    ///         <body>
    ///     } else {
    ///         break;
    ///     }
    /// }
    /// ```
    pub(crate) fn desugar_while_loop(
        &mut self,
        condition: NodeId,
        block: lume_hir::Block,
        location: Location,
    ) -> NodeId {
        let loop_body = {
            let then_cond = lume_hir::Condition {
                condition: Some(condition),
                block,
                location,
            };

            let else_cond = lume_hir::Condition {
                condition: None,
                block: lume_hir::Block {
                    id: self.next_node_id(),
                    statements: vec![self.alloc_break(location)],
                    location,
                },
                location,
            };

            let if_cond_expr = self.alloc_if_conditional(vec![then_cond, else_cond], location);
            let if_cond_stmt = self.alloc_expr_stmt(if_cond_expr);

            lume_hir::Block {
                id: self.next_node_id(),
                statements: vec![if_cond_stmt],
                location,
            }
        };

        self.alloc_infinite_loop(loop_body, location)
    }

    /// Desugars `for` loops into a loop with a `match` statement within it.
    ///
    /// This method lowers `for` loops:
    /// ```lm
    /// for <pattern> in <collection> {
    ///     <body>
    /// }
    /// ```
    /// into the following:
    /// ```lm
    /// {
    ///     let $collection = <collection>;
    ///     let $iter = std::iter::AsIterator::as_iter($collection);
    ///
    ///     loop {
    ///         switch std::iter::Iterator::next($iter) {
    ///             std::Optional::Some(<pattern>) => <body>,
    ///             std::Optional::None => { break; },
    ///         }
    ///     }
    /// }
    /// ```
    pub(crate) fn desugar_for_loop(&mut self, stmt: lume_ast::ForStmt) -> NodeId {
        const VAR_COLLECTION_NAME: &str = "$collection";
        const VAR_ITERATOR_NAME: &str = "$iter";

        let location = self.location(stmt.location());

        let block_location = stmt
            .block()
            .map_or(Location::empty(), |block| self.location(block.location()));

        let scope_id = self.alloc_within_scope(
            |hir| {
                // `let $collection = <collection>;`
                let collection_variable_decl = {
                    let collection_expr = hir.opt_expression(stmt.collection());

                    hir.alloc_variable_decl(VAR_COLLECTION_NAME, collection_expr, None, location)
                };

                // `let $iter = std::iter::AsIterator::as_iter($collection);`
                let iter_variable_decl = {
                    // `$collection`
                    let collection_ref = hir.alloc_variable_ref(VAR_COLLECTION_NAME, location);

                    // `std::iter::AsIterator::as_iter($collection)`
                    let var_value = hir.alloc_static_call(AS_ITERATOR_PATH.clone(), vec![collection_ref], location);

                    hir.alloc_variable_decl(VAR_ITERATOR_NAME, var_value, None, location)
                };

                // `std::iter::Iterator::next($iter);`
                let switch_operand = {
                    let iter_ref = hir.alloc_variable_ref(VAR_ITERATOR_NAME, location);

                    hir.alloc_static_call(ITERATOR_NEXT_PATH.clone(), vec![iter_ref], location)
                };

                // `std::Optional::Some(<pattern>) => <body>`
                let some_case = {
                    let some_pattern = {
                        let subpattern_name = hir.ident_opt(stmt.pattern());
                        let subpattern_location = subpattern_name.location;

                        let some_subpattern = hir.alloc_pat_ident(subpattern_name, subpattern_location);

                        hir.alloc_pat_variant(OPTIONAL_SOME_PATH.clone(), vec![some_subpattern], location)
                    };

                    let some_branch =
                        { hir.alloc_within_scope(|hir| hir.block_opt(stmt.block()).statements, block_location) };

                    lume_hir::SwitchCase {
                        pattern: some_pattern,
                        branch: some_branch,
                        location,
                    }
                };

                // `std::Optional::None => { break; }`
                let none_case = {
                    // `std::Optional::None`
                    let none_pattern = { hir.alloc_pat_variant(OPTIONAL_NONE_PATH.clone(), vec![], location) };

                    // `{ break; }`
                    let none_branch = { hir.alloc_within_scope(|hir| vec![hir.alloc_break(location)], location) };

                    lume_hir::SwitchCase {
                        pattern: none_pattern,
                        branch: none_branch,
                        location,
                    }
                };

                let switch_expr = { hir.alloc_switch_expr(switch_operand, vec![some_case, none_case], location) };

                let loop_stmt = {
                    let switch_stmt = hir.alloc_final_stmt(switch_expr);

                    let loop_body = lume_hir::Block {
                        id: hir.next_node_id(),
                        statements: vec![switch_stmt],
                        location,
                    };

                    hir.alloc_infinite_loop(loop_body, block_location)
                };

                vec![collection_variable_decl, iter_variable_decl, loop_stmt]
            },
            location,
        );

        self.alloc_final_stmt(scope_id)
    }
}
