pub mod fixtures {
    pub mod matcher;
}

use std::ops::ControlFlow;

use fixtures::matcher::fixture_as_hir;
use lume_hir::matcher::*;

#[test]
fn match_patterns_in_switch() {
    let hir = fixture_as_hir(
        "
        fn foo() {
            switch 6_u64 {
                foo => { },
                3_i32 => { },
                Variant::B => { },
                .. => { },
            }
        }
    ",
    );

    #[rustfmt::skip]
    let p = expression([
        switch([
            has_operand([]),

            has_case([has_pattern([ident_pattern([])]), has_branch([any().bind("ident_block")])]),
            has_case([has_pattern([literal_pattern([])]), has_branch([any().bind("literal_block")])]),
            has_case([has_pattern([variant_pattern([])]), has_branch([any().bind("variant_block")])]),
            has_case([has_pattern([wildcard_pattern([])]), has_branch([any().bind("wildcard_block")])]),
        ])
    ]);

    find_matches(&hir, &p, &mut |result| {
        assert!(result.bound_node("ident_block").is_some());
        assert!(result.bound_node("literal_block").is_some());
        assert!(result.bound_node("variant_block").is_some());
        assert!(result.bound_node("wildcard_block").is_some());

        ControlFlow::Continue(())
    });
}
