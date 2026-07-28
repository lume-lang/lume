pub mod fixtures {
    pub mod matcher;
}

use std::cell::Cell;
use std::ops::ControlFlow;
use std::sync::atomic::AtomicUsize;

use fixtures::matcher::fixture_as_hir;
use lume_hir::matcher::*;

#[test]
fn match_all_items_in_root() {
    let hir = fixture_as_hir(
        "
        fn foo() {}
        fn bar() {}
        fn baz() {}
    ",
    );

    let mut called_with = Cell::new(Vec::with_capacity(3));

    find_matches(&hir, &function([]).bind("func"), &mut |result| {
        let lume_hir::Node::Function(func) = result.bound_node("func").unwrap() else {
            unreachable!();
        };

        called_with.get_mut().push(func.path().to_string());

        ControlFlow::Continue(())
    });

    assert_eq!(called_with.into_inner(), vec!["foo", "bar", "baz"]);
}

#[test]
fn match_breaks_with_returned_flow() {
    let hir = fixture_as_hir(
        "
        fn foo() {}
        fn bar() {}
    ",
    );

    let called = AtomicUsize::new(0);

    find_matches(&hir, &function([]).bind("func"), &mut |result| {
        let lume_hir::Node::Function(_) = result.bound_node("func").unwrap() else {
            unreachable!();
        };

        called.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

        ControlFlow::Break(())
    });

    assert_eq!(called.load(std::sync::atomic::Ordering::SeqCst), 1);
}

#[test]
fn match_with_outside_state() {
    let hir = fixture_as_hir(
        "
        fn foo() {}
        fn bar() {}
    ",
    );

    let mut matches = Vec::new();

    find_matches(&hir, &function([]).bind("func"), &mut |result| {
        let lume_hir::Node::Function(func) = result.bound_node("func").unwrap() else {
            unreachable!();
        };

        matches.push(func.path().to_string());

        ControlFlow::Continue(())
    });

    assert_eq!(matches, vec!["foo", "bar"]);
}

#[test]
fn match_filter_node() {
    let hir = fixture_as_hir(
        "
        fn foo() {
            let a = 5_i32;
            let b = 5_i32;
        }
    ",
    );

    find_matches(
        &hir,
        &statement([var_decl([filter(
            |var_decl: &lume_hir::VariableDeclaration| var_decl.name.as_str() == "b",
            [],
        )])])
        .bind("decl"),
        &mut |result| {
            let lume_hir::Node::Statement(stmt) = result.bound_node("decl").unwrap() else {
                unreachable!();
            };

            let lume_hir::StatementKind::Variable(var_decl) = &stmt.kind else {
                unreachable!();
            };

            assert_eq!(var_decl.name.as_str(), "b");

            ControlFlow::Continue(())
        },
    );
}

#[test]
fn match_optional_bindings() {
    let hir = fixture_as_hir(
        "
        fn foo() {
            let a = 5_i32;
            let b: T = 5_i32;
        }
    ",
    );

    #[rustfmt::skip]
    let p = statement([
        var_decl([
            optionally([has_type_decl([any().bind_type("var_type")])]),
        ]).bind("var_decl")
    ]);

    find_matches(&hir, &p, &mut |result| {
        let lume_hir::Node::Statement(stmt) = result.bound_node("var_decl").unwrap() else {
            unreachable!();
        };

        let lume_hir::StatementKind::Variable(var_decl) = &stmt.kind else {
            unreachable!();
        };

        match var_decl.name.as_str() {
            "a" => assert!(result.bound_type("var_type").is_none()),
            "b" => assert!(result.bound_type("var_type").is_some()),
            _ => unreachable!(),
        }

        ControlFlow::Continue(())
    });
}

#[test]
fn match_inside_node() {
    let hir = fixture_as_hir(
        "
        fn foo() {
            let a = 5_i32;
        }
    ",
    );

    let foo_node = hir
        .nodes
        .values()
        .find(|node| {
            if let lume_hir::Node::Function(func_def) = node
                && func_def.signature.name.to_string() == "foo"
            {
                true
            } else {
                false
            }
        })
        .unwrap();

    find_matches_in(
        &hir,
        &statement([var_decl([])]).bind("decl"),
        &mut |result| {
            let lume_hir::Node::Statement(stmt) = result.bound_node("decl").unwrap() else {
                unreachable!();
            };

            let lume_hir::StatementKind::Variable(var_decl) = &stmt.kind else {
                unreachable!();
            };

            assert_eq!(var_decl.name.as_str(), "a");

            ControlFlow::Continue(())
        },
        foo_node,
    );
}

#[test]
fn match_fn_definitions() {
    let hir = fixture_as_hir(
        "
        fn foo() { }

        /// with comments!
        fn bar() { }
    ",
    );

    find_first_match(&hir, &function([]).bind("func"), &mut |result| {
        let lume_hir::Node::Function(func) = result.bound_node("func").unwrap() else {
            unreachable!();
        };

        assert_eq!(func.signature.name.to_string(), "foo");
    });

    find_first_match(&hir, &function([has_documentation([])]).bind("func"), &mut |result| {
        let lume_hir::Node::Function(func) = result.bound_node("func").unwrap() else {
            unreachable!();
        };

        assert_eq!(func.signature.name.to_string(), "bar");
    });
}

#[test]
fn match_method_definitions() {
    let hir = fixture_as_hir(
        "
        struct Foo { }

        impl Foo {
            fn foo() { }

            /// with comments!
            fn bar() { }
        }
    ",
    );

    find_first_match(&hir, &method([]).bind("func"), &mut |result| {
        let lume_hir::Node::Method(func) = result.bound_node("func").unwrap() else {
            unreachable!();
        };

        assert_eq!(func.signature.name.to_string(), "foo");
    });

    find_first_match(&hir, &method([has_documentation([])]).bind("func"), &mut |result| {
        let lume_hir::Node::Method(func) = result.bound_node("func").unwrap() else {
            unreachable!();
        };

        assert_eq!(func.signature.name.to_string(), "bar");
    });
}
