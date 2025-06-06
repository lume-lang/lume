use lume_hir::SymbolName;

use super::*;

#[test]
fn infer_expr_lit_match() -> Result<()> {
    let tcx = empty_tcx();
    let matches: &[(lume_hir::Expression, SymbolName)] = &[
        (lume_hir::Expression::lit_bool(false), SymbolName::boolean()),
        (lume_hir::Expression::lit_i8(16), SymbolName::i8()),
        (lume_hir::Expression::lit_i16(16), SymbolName::i16()),
        (lume_hir::Expression::lit_i32(16), SymbolName::i32()),
        (lume_hir::Expression::lit_i64(16), SymbolName::i64()),
        (lume_hir::Expression::lit_u8(16), SymbolName::u8()),
        (lume_hir::Expression::lit_u16(16), SymbolName::u16()),
        (lume_hir::Expression::lit_u32(16), SymbolName::u32()),
        (lume_hir::Expression::lit_u64(16).unwrap(), SymbolName::u64()),
    ];

    for (expr, expected_name) in matches {
        let infered_ty = tcx.type_of_expr(expr)?;
        let expected_ty = tcx.find_type_ref(expected_name)?.unwrap();

        assert_eq!(infered_ty, expected_ty);
    }

    Ok(())
}
