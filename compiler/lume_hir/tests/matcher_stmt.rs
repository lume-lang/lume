pub mod fixtures {
    pub mod matcher;
}

use fixtures::matcher::fixture_as_hir;
use lume_hir::matcher::*;

#[test]
fn match_no_statements() {
    let hir = fixture_as_hir("fn foo() {}");

    find_matches(&hir, &statement([]), &mut |_result| {
        panic!("this should not be called!")
    });
}

#[test]
fn match_all_statements() {
    let hir = fixture_as_hir(
        "fn foo() {
            let a = 1;
            let b = 2;
            let c = 3;
        }",
    );

    find_first_match(
        &hir,
        &statement([var_decl([has_name("a")]).bind("stmt")]),
        &mut |result| {
            let span = result.bound_node("stmt").unwrap().location();
            assert_eq!(span.content(), Some("let a = 1;"));
        },
    );

    find_first_match(
        &hir,
        &statement([var_decl([has_name("b")]).bind("stmt")]),
        &mut |result| {
            let span = result.bound_node("stmt").unwrap().location();
            assert_eq!(span.content(), Some("let b = 2;"));
        },
    );

    find_first_match(
        &hir,
        &statement([var_decl([has_name("c")]).bind("stmt")]),
        &mut |result| {
            let span = result.bound_node("stmt").unwrap().location();
            assert_eq!(span.content(), Some("let c = 3;"));
        },
    );
}

#[test]
fn match_var_decl_type() {
    let hir = fixture_as_hir(
        "fn foo() {
            let a = 1;
            let b: T = 2;
        }",
    );

    find_first_match(&hir, &statement([var_decl([]).bind("stmt")]), &mut |result| {
        let span = result.bound_node("stmt").unwrap().location();
        assert_eq!(span.content(), Some("let a = 1;"));
    });

    find_first_match(
        &hir,
        &statement([var_decl([has_type_decl([])]).bind("stmt")]),
        &mut |result| {
            let span = result.bound_node("stmt").unwrap().location();
            assert_eq!(span.content(), Some("let b: T = 2;"));
        },
    );
}
