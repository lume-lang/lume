use lume_hir::Path;
use lume_span::{NodeId, PackageId};

use super::*;

#[test]
fn infer_expr_lit_match() -> Result<()> {
    let tcx = type_infer("")?;
    let id = NodeId::empty(PackageId::from_usize(1));

    let matches: &[(lume_hir::Expression, Path)] = &[
        (lume_hir::Expression::lit_bool(id, false), Path::boolean()),
        (lume_hir::Expression::lit_string(id, "hello!"), Path::string()),
        (lume_hir::Expression::lit_i8(id, 16), Path::i8()),
        (lume_hir::Expression::lit_i16(id, 16), Path::i16()),
        (lume_hir::Expression::lit_i32(id, 16), Path::i32()),
        (lume_hir::Expression::lit_i64(id, 16), Path::i64()),
        (lume_hir::Expression::lit_u8(id, 16), Path::u8()),
        (lume_hir::Expression::lit_u16(id, 16), Path::u16()),
        (lume_hir::Expression::lit_u32(id, 16), Path::u32()),
        (lume_hir::Expression::lit_u64(id, 16).unwrap(), Path::u64()),
    ];

    for (expr, expected_name) in matches {
        let infered_ty = tcx.type_of_expr(expr)?;
        let expected_ty = tcx.find_type_ref(expected_name)?.unwrap();

        assert_eq!(infered_ty, expected_ty);
    }

    Ok(())
}
