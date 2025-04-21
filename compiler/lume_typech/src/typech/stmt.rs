use lume_diag::Result;

use crate::{ThirBuildCtx, typech::TypeCheckerPass};

pub(super) struct VarDeclTypeChecker;

impl<'a> TypeCheckerPass<'a> for VarDeclTypeChecker {
    fn run(hir: &'a lume_hir::map::Map, tcx: &'a mut ThirBuildCtx) -> Result<()> {
        for (_, stmt) in hir.statements() {
            match &stmt.kind {
                lume_hir::StatementKind::Variable(v) => Self::check_stmt(tcx, v)?,
                _ => continue,
            };
        }

        Ok(())
    }
}

impl VarDeclTypeChecker {
    fn check_stmt<'a>(tcx: &'a mut ThirBuildCtx, stmt: &lume_hir::VariableDeclaration) -> Result<()> {
        let value_expr = tcx.type_of_expr(stmt.value.id);

        let resolved_type = if let Some(declared_type) = &stmt.declared_type {
            let declared_type_ref = tcx.lower_type_ref(declared_type);

            tcx.check_type_compatibility(value_expr, &declared_type_ref)?;

            declared_type_ref
        } else {
            value_expr.clone()
        };

        tcx.resolved_stmts.insert(stmt.id, resolved_type);

        Ok(())
    }
}
