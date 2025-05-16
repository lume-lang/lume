use indexmap::IndexMap;
use std::{ops::Range, sync::Arc};

use error_snippet::Result;
use error_snippet_derive::Diagnostic;
use lume_hir::{self};
use lume_span::SourceFile;

use crate::ThirBuildCtx;

#[derive(Diagnostic, Debug)]
#[diagnostic(
    message = "cannot infer type argument",
    code = "LM4206",
    help = "consider explicitly defining the type for {type_arg}"
)]
pub struct CannotInferTypeArgument {
    #[span]
    pub source: Arc<SourceFile>,

    #[label("could not infer type argument {type_arg}")]
    pub range: Range<usize>,

    pub type_arg: String,
}

pub(super) struct InferTypeArgs<'a, 'b> {
    ctx: &'a mut ThirBuildCtx<'b>,
    hir: &'a mut lume_hir::map::Map,
}

impl InferTypeArgs<'_, '_> {
    pub(super) fn run_all(ctx: &mut ThirBuildCtx<'_>, hir: &mut lume_hir::map::Map) -> Result<()> {
        let mut infer = InferTypeArgs { ctx, hir };

        infer.run()?;

        Ok(())
    }

    fn run(&mut self) -> Result<()> {
        let mut updated_type_args = IndexMap::new();

        for (id, expr) in self.hir.expressions() {
            let expr = match &expr.kind {
                lume_hir::ExpressionKind::StaticCall(_) | lume_hir::ExpressionKind::InstanceCall(_) => expr,
                _ => continue,
            };

            let reference = self
                .ctx
                .resolved_calls
                .get(&expr.id)
                .unwrap_or_else(|| panic!("reference for call expression {:?} is not set", expr.id));

            let signature = reference.sig(self.ctx.tcx());
            let type_args = self.infer_type_args_in_sig(expr, &signature)?;

            if type_args.is_empty() {
                continue;
            }

            updated_type_args.insert(*id, type_args);
        }

        for (id, type_args) in updated_type_args {
            match &mut self.hir.expression_mut(id).unwrap().kind {
                lume_hir::ExpressionKind::InstanceCall(call) => call.type_arguments = type_args,
                lume_hir::ExpressionKind::StaticCall(call) => call.type_arguments = type_args,
                _ => {}
            }
        }

        Ok(())
    }

    fn infer_type_args_in_sig(
        &self,
        expr: &lume_hir::Expression,
        sig: &lume_types::FunctionSig,
    ) -> Result<Vec<lume_hir::TypeArgument>> {
        let (args, type_args) = match &expr.kind {
            lume_hir::ExpressionKind::InstanceCall(call) => (&call.arguments, &call.type_arguments),
            lume_hir::ExpressionKind::StaticCall(call) => (&call.arguments, &call.type_arguments),
            k => panic!("BUG: invalid expression kind in infer_type_args_in_sig: {k:?}"),
        };

        if type_args.is_empty() {
            return Ok(Vec::new());
        }

        let mut inferred_type_args = Vec::new();

        'outer: for (type_arg, type_param) in type_args.iter().zip(sig.type_params.iter()) {
            // Skip explicitly named type arguments, as there's nothing to infer.
            if type_arg.is_named() {
                inferred_type_args.push(type_arg.clone());
                continue;
            }

            let type_param = type_param.get(self.ctx.tcx());
            let type_param_ty = lume_types::Type::find_type_param(self.ctx.tcx(), type_param.id).unwrap();
            let type_param_ref = lume_types::TypeRef::new(type_param_ty.id);

            // First, we attempt to look over all parameters and check whether
            // the type parameter is used in the parameter list. If it is, we
            // can infer the type argument from the parameter type.
            for (param, arg) in sig.params.inner().iter().zip(args.iter()) {
                // Determine the type of the argument.
                let arg_ty = self.ctx.type_of(self.hir, arg.id)?;

                if let Some(arg_type_ref) = self.ctx.type_arg_evaluate(&param.ty, &type_param_ref, &arg_ty) {
                    inferred_type_args.push(lume_hir::TypeArgument::Resolved {
                        ty: arg_type_ref.clone(),
                        location: arg.location.clone(),
                    });

                    break 'outer;
                }
            }

            return Err(CannotInferTypeArgument {
                source: expr.location.file.clone(),
                range: expr.location.index.clone(),
                type_arg: type_param.name.clone(),
            }
            .into());
        }

        Ok(inferred_type_args)
    }
}
