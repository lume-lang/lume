use std::collections::HashSet;

use indexmap::IndexMap;
use lume_errors::Result;
use lume_hir::TypeParameterId;
use lume_span::{DefId, ExpressionId, StatementId};
use lume_types::TypeRef;

use crate::{TyInferCtx, query::Callable};

#[expect(dead_code)]
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
enum Entry {
    Statement(StatementId),
    Expression(ExpressionId),
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
struct TypeParameterReference {
    pub(crate) entry: Entry,
    pub(crate) param: TypeParameterId,
}

#[derive(Clone, PartialEq, Eq)]
struct TypeArgumentSubstitute {
    pub(crate) idx: usize,
    pub(crate) ty: TypeRef,
}

#[derive(Default)]
pub(crate) struct UnificationPass {
    substitution_map: IndexMap<TypeParameterReference, TypeArgumentSubstitute>,

    work_map: HashSet<TypeParameterReference>,
}

impl UnificationPass {
    #[tracing::instrument(level = "INFO", skip_all, err)]
    pub(crate) fn invoke<'tcx>(mut self, tcx: &'tcx mut TyInferCtx) -> Result<()> {
        self.unify(tcx)?;
        self.substitute_type_args(tcx)?;

        Ok(())
    }
}

impl UnificationPass {
    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn unify<'tcx>(&mut self, tcx: &'tcx TyInferCtx) -> Result<()> {
        for (_, item) in &tcx.hir.items {
            match item {
                lume_hir::Item::Type(ty) => match ty.as_ref() {
                    lume_hir::TypeDefinition::Struct(struct_def) => {
                        for field in &struct_def.fields {
                            if let Some(default) = field.default_value {
                                self.unify_expr(tcx, default)?;
                            }
                        }
                    }
                    lume_hir::TypeDefinition::Trait(trait_def) => {
                        for method in &trait_def.methods {
                            if let Some(block) = &method.block {
                                self.unify_block(tcx, block)?;
                            }
                        }
                    }
                    lume_hir::TypeDefinition::Enum(_) => {}
                },
                lume_hir::Item::Impl(impl_block) => {
                    for method in &impl_block.methods {
                        if let Some(block) = &method.block {
                            self.unify_block(tcx, block)?;
                        }
                    }
                }
                lume_hir::Item::TraitImpl(trait_impl) => {
                    for method in &trait_impl.methods {
                        self.unify_block(tcx, &method.block)?;
                    }
                }
                lume_hir::Item::Function(func) => {
                    if let Some(block) = &func.block {
                        self.unify_block(tcx, block)?;
                    }
                }
            }
        }

        Ok(())
    }

    fn unify_block<'tcx>(&mut self, tcx: &'tcx TyInferCtx, block: &lume_hir::Block) -> Result<()> {
        for stmt in &block.statements {
            self.unify_stmt(tcx, *stmt)?;
        }

        Ok(())
    }

    fn unify_stmt<'tcx>(&mut self, tcx: &'tcx TyInferCtx, stmt: lume_span::StatementId) -> Result<()> {
        let stmt = tcx.hir_expect_stmt(stmt);

        match &stmt.kind {
            lume_hir::StatementKind::Variable(decl) => {
                self.unify_expr(tcx, decl.value)?;
                self.try_resolve_variable_decl(tcx, decl)?;

                Ok(())
            }
            lume_hir::StatementKind::Break(_) | lume_hir::StatementKind::Continue(_) => Ok(()),
            lume_hir::StatementKind::Final(s) => {
                self.unify_expr(tcx, s.value)?;

                Ok(())
            }
            lume_hir::StatementKind::Return(s) => {
                if let Some(value) = s.value {
                    self.unify_expr(tcx, value)
                } else {
                    Ok(())
                }
            }
            lume_hir::StatementKind::InfiniteLoop(s) => self.unify_block(tcx, &s.block),
            lume_hir::StatementKind::IteratorLoop(s) => {
                self.unify_block(tcx, &s.block)?;
                self.unify_expr(tcx, s.collection)?;

                Ok(())
            }
            lume_hir::StatementKind::Expression(s) => self.unify_expr(tcx, *s),
        }
    }

    fn unify_expr<'tcx>(&mut self, tcx: &'tcx TyInferCtx, expr: lume_span::ExpressionId) -> Result<()> {
        let expr = tcx.hir_expect_expr(expr);

        match &expr.kind {
            lume_hir::ExpressionKind::Assignment(s) => {
                self.unify_expr(tcx, s.target)?;
                self.unify_expr(tcx, s.value)?;
            }
            lume_hir::ExpressionKind::Cast(s) => self.unify_expr(tcx, s.source)?,
            lume_hir::ExpressionKind::Construct(s) => {
                for field in &s.fields {
                    self.unify_expr(tcx, field.value)?;
                }
            }
            lume_hir::ExpressionKind::Binary(s) => {
                self.unify_expr(tcx, s.lhs)?;
                self.unify_expr(tcx, s.rhs)?;
            }
            lume_hir::ExpressionKind::InstanceCall(call) => {
                self.unify_expr(tcx, call.callee)?;

                for arg in &call.arguments {
                    self.unify_expr(tcx, *arg)?;
                }

                self.try_resolve_instance_call(tcx, call)?;
            }
            lume_hir::ExpressionKind::IntrinsicCall(s) => {
                self.unify_expr(tcx, s.callee())?;

                for arg in &s.arguments {
                    self.unify_expr(tcx, *arg)?;
                }
            }
            lume_hir::ExpressionKind::If(s) => {
                for case in &s.cases {
                    if let Some(condition) = case.condition {
                        self.unify_expr(tcx, condition)?;
                    }

                    self.unify_block(tcx, &case.block)?;
                }
            }
            lume_hir::ExpressionKind::Logical(s) => {
                self.unify_expr(tcx, s.lhs)?;
                self.unify_expr(tcx, s.rhs)?;
            }
            lume_hir::ExpressionKind::Member(s) => self.unify_expr(tcx, s.callee)?,
            lume_hir::ExpressionKind::StaticCall(call) => {
                for arg in &call.arguments {
                    self.unify_expr(tcx, *arg)?;
                }

                if let Callable::Method(method) = tcx.probe_callable_static(call)? {
                    self.unify_method_call(tcx, call, method)?;
                }
            }
            lume_hir::ExpressionKind::Scope(s) => {
                for stmt in &s.body {
                    self.unify_stmt(tcx, *stmt)?;
                }
            }
            lume_hir::ExpressionKind::Switch(_) => todo!(),
            lume_hir::ExpressionKind::Literal(_)
            | lume_hir::ExpressionKind::Variable(_)
            | lume_hir::ExpressionKind::Field(_)
            | lume_hir::ExpressionKind::Variant(_) => (),
        };

        Ok(())
    }

    fn unify_method_call<'tcx>(
        &mut self,
        tcx: &'tcx TyInferCtx,
        call: &lume_hir::StaticCall,
        method: &lume_types::Method,
    ) -> Result<()> {
        let full_method_name = &method.name;
        let full_type_name = full_method_name.clone().parent().expect("method name should have type");
        let invoked_method_name = call.name.clone().parent().expect("method name should have type");

        let expected_arg_count = full_type_name.type_arguments().len();
        let declared_arg_count = invoked_method_name.type_arguments().len();

        if expected_arg_count == declared_arg_count {
            return Ok(());
        }

        for method_type_param in full_type_name.type_arguments() {
            let type_param_ref = tcx.mk_type_ref_from(method_type_param, method.hir)?;
            let Some(type_param) = tcx.as_type_parameter(&type_param_ref)? else {
                continue;
            };

            self.work_map.insert(TypeParameterReference {
                entry: Entry::Expression(call.id),
                param: type_param.id,
            });
        }

        Ok(())
    }

    fn try_resolve_variable_decl<'tcx>(
        &mut self,
        tcx: &'tcx TyInferCtx,
        decl: &lume_hir::VariableDeclaration,
    ) -> Result<()> {
        // If the declaration doesn't have any explicit type, we can't use it
        // to infer type parameters on it's value.
        let Some(declared_type) = &decl.declared_type else {
            return Ok(());
        };

        let declared_type_ref = tcx.mk_type_ref_from_expr(declared_type, decl.value)?;
        let value_type_ref = tcx.type_of(decl.value)?;

        if let Some(type_param_ref) = tcx.as_type_parameter(&value_type_ref)? {
            let work_key = TypeParameterReference {
                entry: Entry::Expression(decl.value),
                param: type_param_ref.id,
            };

            self.add_substitution(0, work_key, value_type_ref);

            return Ok(());
        };

        for (idx, (decl_type_arg, type_param_arg)) in declared_type_ref
            .type_arguments
            .iter()
            .zip(value_type_ref.type_arguments.iter())
            .enumerate()
        {
            let Some(type_param_ref) = tcx.as_type_parameter(type_param_arg)? else {
                continue;
            };

            let work_key = TypeParameterReference {
                entry: Entry::Expression(decl.value),
                param: type_param_ref.id,
            };

            self.add_substitution(idx, work_key, decl_type_arg.to_owned());
        }

        Ok(())
    }

    fn try_resolve_instance_call<'tcx>(&mut self, tcx: &'tcx TyInferCtx, call: &lume_hir::InstanceCall) -> Result<()> {
        let mut callee = call.callee;

        while let lume_hir::ExpressionKind::Variable(var_ref) = &tcx.hir_expect_expr(callee).kind {
            if let lume_hir::VariableSource::Variable(decl) = &var_ref.reference {
                callee = decl.value;
            } else {
                break;
            }
        }

        let method = tcx.probe_callable_instance(call)?;
        let args = tcx.hir.expect_expressions(&call.arguments)?;

        // We skip the `self` parameter, since the type argument inference will
        // just resolve the type parameter on the callee, which is what we're currently trying to resolve.
        let method_params = method.signature().params.inner().to_vec();
        let params = lume_types::Parameters {
            params: method_params.into_iter().skip_while(|p| p.is_self()).collect(),
        };

        for (idx, call_type_arg) in tcx.type_of(call.callee)?.type_arguments.iter().enumerate() {
            let Some(type_param) = tcx.as_type_parameter(call_type_arg)? else {
                continue;
            };

            let Some(inferred_type_param) = tcx.infer_type_arg_param(type_param.id, &params, &args)? else {
                continue;
            };

            let work_key = TypeParameterReference {
                entry: Entry::Expression(callee),
                param: type_param.id,
            };

            self.add_substitution(idx, work_key, inferred_type_param);
        }

        Ok(())
    }

    fn add_substitution(&mut self, idx: usize, work_key: TypeParameterReference, replacement: TypeRef) {
        if !self.work_map.contains(&work_key) {
            return;
        }

        let substitute = TypeArgumentSubstitute { idx, ty: replacement };

        self.work_map.remove(&work_key);
        self.substitution_map.insert(work_key, substitute);
    }
}

impl UnificationPass {
    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn substitute_type_args<'tcx>(self, tcx: &'tcx mut TyInferCtx) -> Result<()> {
        for (reference, type_arg) in &self.substitution_map {
            let TypeParameterReference { entry, .. } = reference;
            let TypeArgumentSubstitute { idx, ty } = type_arg;

            let hir_ty = tcx.hir_lift_type(ty)?;

            match *entry {
                Entry::Expression(expr) => {
                    let expr = tcx.hir.expression_mut(expr).unwrap();

                    if let lume_hir::ExpressionKind::StaticCall(call) = &mut expr.kind {
                        let type_segment = call.name.root.last_mut().unwrap();

                        let mut existing_type_args = type_segment.type_arguments().to_vec();
                        existing_type_args.insert(*idx, hir_ty);

                        type_segment.place_type_arguments(existing_type_args);
                    }
                }
                Entry::Statement(_) => todo!(),
            }
        }

        for TypeParameterReference { entry, param } in self.work_map {
            let param = tcx.tdb().type_parameter(param).unwrap();

            let location = match entry {
                Entry::Statement(stmt) => tcx.hir_span_of_def(DefId::Statement(stmt)),
                Entry::Expression(expr) => tcx.hir_span_of_def(DefId::Expression(expr)),
            };

            tcx.dcx.emit(
                crate::errors::TypeArgumentInferenceFailed {
                    source: location,
                    type_param_name: param.name.clone(),
                }
                .into(),
            );
        }

        tcx.dcx().ensure_untainted()?;

        Ok(())
    }
}
