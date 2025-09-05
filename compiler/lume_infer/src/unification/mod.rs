mod diagnostics;

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
}

impl UnificationPass {
    #[tracing::instrument(level = "INFO", name = "lume_infer::UnificationPass::invoke", skip_all, err)]
    pub(crate) fn invoke<'tcx>(mut self, tcx: &'tcx mut TyInferCtx) -> Result<()> {
        self.unify(tcx)?;
        self.substitute_type_args(tcx)?;
        self.verify_fulfilled_paths(tcx)?;

        Ok(())
    }
}

impl UnificationPass {
    #[tracing::instrument(level = "DEBUG", name = "lume_infer::UnificationPass::unify", skip_all, err)]
    fn unify<'tcx>(&mut self, tcx: &'tcx TyInferCtx) -> Result<()> {
        for (_, item) in &tcx.hir.items {
            match item {
                lume_hir::Item::Type(ty) => match ty.as_ref() {
                    lume_hir::TypeDefinition::Struct(struct_def) => {
                        for field in &struct_def.fields {
                            self.verify_type_name(tcx, &field.field_type.name)?;

                            if let Some(default) = field.default_value {
                                self.unify_expr(tcx, default)?;
                            }
                        }
                    }
                    lume_hir::TypeDefinition::Trait(trait_def) => {
                        for method in &trait_def.methods {
                            for param in &method.parameters {
                                self.verify_type_name(tcx, &param.param_type.name)?;
                            }

                            self.verify_type_name(tcx, &method.return_type.name)?;

                            if let Some(block) = &method.block {
                                self.unify_block(tcx, block)?;
                            }
                        }
                    }
                    lume_hir::TypeDefinition::Enum(enum_def) => {
                        for case in &enum_def.cases {
                            for param in &case.parameters {
                                self.verify_type_name(tcx, &param.name)?;
                            }
                        }
                    }
                },
                lume_hir::Item::Impl(impl_block) => {
                    self.verify_type_name(tcx, &impl_block.target.name)?;

                    for method in &impl_block.methods {
                        for param in &method.parameters {
                            self.verify_type_name(tcx, &param.param_type.name)?;
                        }

                        self.verify_type_name(tcx, &method.return_type.name)?;

                        if let Some(block) = &method.block {
                            self.unify_block(tcx, block)?;
                        }
                    }
                }
                lume_hir::Item::TraitImpl(trait_impl) => {
                    self.verify_type_name(tcx, &trait_impl.name.name)?;
                    self.verify_type_name(tcx, &trait_impl.target.name)?;

                    for method in &trait_impl.methods {
                        for param in &method.parameters {
                            self.verify_type_name(tcx, &param.param_type.name)?;
                        }

                        self.verify_type_name(tcx, &method.return_type.name)?;

                        if let Some(block) = &method.block {
                            self.unify_block(tcx, block)?;
                        }
                    }
                }
                lume_hir::Item::Function(func) => {
                    for param in &func.parameters {
                        self.verify_type_name(tcx, &param.param_type.name)?;
                    }

                    self.verify_type_name(tcx, &func.return_type.name)?;

                    if let Some(block) = &func.block {
                        self.unify_block(tcx, block)?;
                    }
                }
            }
        }

        tcx.dcx().ensure_untainted()?;

        Ok(())
    }

    #[tracing::instrument(level = "TRACE", skip_all, err)]
    fn unify_block<'tcx>(&mut self, tcx: &'tcx TyInferCtx, block: &lume_hir::Block) -> Result<()> {
        for stmt in &block.statements {
            self.unify_stmt(tcx, *stmt)?;
        }

        Ok(())
    }

    #[tracing::instrument(level = "TRACE", skip(self, tcx), err)]
    fn unify_stmt<'tcx>(&mut self, tcx: &'tcx TyInferCtx, stmt: lume_span::StatementId) -> Result<()> {
        let stmt = tcx.hir_expect_stmt(stmt);

        match &stmt.kind {
            lume_hir::StatementKind::Variable(decl) => {
                if let Some(declared_type) = &decl.declared_type {
                    self.verify_type_name(tcx, &declared_type.name)?;
                }

                self.unify_expr(tcx, decl.value)?;

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

    #[tracing::instrument(level = "TRACE", skip(self, tcx), err)]
    fn unify_expr<'tcx>(&mut self, tcx: &'tcx TyInferCtx, expr: lume_span::ExpressionId) -> Result<()> {
        let expr = tcx.hir_expect_expr(expr);

        match &expr.kind {
            lume_hir::ExpressionKind::Assignment(s) => {
                self.unify_expr(tcx, s.target)?;
                self.unify_expr(tcx, s.value)?;
            }
            lume_hir::ExpressionKind::Cast(cast) => {
                self.unify_expr(tcx, cast.source)?;

                self.enqueue_path_unification(tcx, expr.id, &cast.target.name)?;
            }
            lume_hir::ExpressionKind::Construct(construct) => {
                for field in &construct.fields {
                    self.unify_expr(tcx, field.value)?;
                }

                self.enqueue_path_unification(tcx, expr.id, &construct.path)?;
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
            lume_hir::ExpressionKind::Is(s) => {
                self.unify_expr(tcx, s.target)?;
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

                self.enqueue_path_unification(tcx, expr.id, &call.name)?;
            }
            lume_hir::ExpressionKind::Scope(s) => {
                for stmt in &s.body {
                    self.unify_stmt(tcx, *stmt)?;
                }
            }
            lume_hir::ExpressionKind::Variant(variant) => {
                self.enqueue_path_unification(tcx, expr.id, &variant.name)?;
            }
            lume_hir::ExpressionKind::Switch(switch) => {
                self.unify_expr(tcx, switch.operand)?;

                for case in &switch.cases {
                    self.unify_expr(tcx, case.branch)?;
                    self.unify_pattern(tcx, expr.id, &case.pattern)?;
                }
            }
            lume_hir::ExpressionKind::Literal(_)
            | lume_hir::ExpressionKind::Variable(_)
            | lume_hir::ExpressionKind::Field(_) => (),
        };

        Ok(())
    }

    #[tracing::instrument(level = "TRACE", skip(self, tcx), err)]
    fn unify_pattern<'tcx>(
        &mut self,
        tcx: &'tcx TyInferCtx,
        expr: ExpressionId,
        pattern: &lume_hir::Pattern,
    ) -> Result<()> {
        match &pattern.kind {
            lume_hir::PatternKind::Literal(_) => {}
            lume_hir::PatternKind::Identifier(_) => {}
            lume_hir::PatternKind::Variant(variant) => {
                self.enqueue_path_unification(tcx, expr, &variant.name)?;

                for field in &variant.fields {
                    self.unify_pattern(tcx, expr, field)?;
                }
            }
            lume_hir::PatternKind::Wildcard(_) => {}
        }

        Ok(())
    }

    #[tracing::instrument(level = "TRACE", skip(self, tcx), err)]
    fn verify_type_name<'tcx>(&self, tcx: &'tcx TyInferCtx, path: &lume_hir::Path) -> Result<()> {
        let mut type_path = path.clone();

        // Strip back the path until we have the actual type of the path.
        //
        // This is mostly for when a method path is passed, which would throw off the method.
        while !type_path.is_type() {
            if let Some(parent) = type_path.parent() {
                type_path = parent;
            } else {
                return Ok(());
            }
        }

        let Some(matching_type) = tcx.tdb().find_type(&type_path) else {
            return Ok(());
        };

        let expected_arg_count = matching_type.kind.type_parameters().len();
        let declared_arg_count = type_path.type_arguments().len();

        if expected_arg_count != declared_arg_count {
            tcx.dcx().emit(
                diagnostics::TypeArgumentCountMismatch {
                    source: path.location,
                    type_name: path.clone(),
                    expected: expected_arg_count,
                    actual: declared_arg_count,
                }
                .into(),
            );
        }

        Ok(())
    }

    #[tracing::instrument(level = "TRACE", skip(self, tcx), err)]
    fn enqueue_path_unification<'tcx>(
        &mut self,
        tcx: &'tcx TyInferCtx,
        expr: ExpressionId,
        path: &lume_hir::Path,
    ) -> Result<()> {
        let expected_type_args = path.all_type_arguments().len();

        match &path.name {
            lume_hir::PathSegment::Type { .. } => {
                let Some(matching_type) = tcx.tdb().find_type(path) else {
                    return Err(crate::errors::MissingType {
                        name: path.to_owned(),
                        source: tcx.hir_span_of_def(DefId::Expression(expr)),
                    }
                    .into());
                };

                if expected_type_args == matching_type.kind.type_parameters().len() {
                    return Ok(());
                }

                for (idx, type_param) in matching_type
                    .kind
                    .type_parameters()
                    .iter()
                    .enumerate()
                    .skip(expected_type_args)
                {
                    self.resolve_type_param(tcx, idx, expr, *type_param)?;
                }
            }
            lume_hir::PathSegment::Callable { .. } => {
                let callable = if let Some(method) = tcx.tdb().find_method(path) {
                    Callable::Method(method)
                } else if let Some(func) = tcx.tdb().find_function(path) {
                    Callable::Function(func)
                } else {
                    return Err(diagnostics::CallableNotFound {
                        source: tcx.hir_span_of_def(DefId::Expression(expr)),
                        name: path.to_owned(),
                    }
                    .into());
                };

                let type_args = callable.name().all_root_type_arguments().len();
                let method_args = callable.signature().type_params.len();

                if expected_type_args == type_args + method_args {
                    return Ok(());
                }

                for (idx, type_param) in callable
                    .name()
                    .all_root_type_arguments()
                    .iter()
                    .enumerate()
                    .skip(path.all_root_type_arguments().len())
                {
                    let type_param_ref = tcx.mk_type_ref_from(type_param, callable.id())?;
                    let Some(type_param) = tcx.as_type_parameter(&type_param_ref)? else {
                        continue;
                    };

                    self.resolve_type_param(tcx, idx, expr, type_param.id)?;
                }

                for (idx, type_param) in callable
                    .signature()
                    .type_params
                    .iter()
                    .enumerate()
                    .skip(path.type_arguments().len())
                {
                    self.resolve_type_param(tcx, idx, expr, *type_param)?;
                }
            }
            lume_hir::PathSegment::Variant { .. } => {
                let enum_def = tcx.enum_def_of_name(&path.clone().parent().unwrap())?;

                if expected_type_args == enum_def.type_parameters.len() {
                    return Ok(());
                }

                for (idx, type_param) in enum_def.type_parameters.iter().enumerate().skip(expected_type_args) {
                    self.resolve_type_param(tcx, idx, expr, type_param.type_param_id.unwrap())?;
                }
            }
            lume_hir::PathSegment::Namespace { .. } => return Ok(()),
        }

        Ok(())
    }

    #[tracing::instrument(level = "TRACE", skip(self, tcx), err)]
    fn resolve_type_param<'tcx>(
        &mut self,
        tcx: &'tcx TyInferCtx,
        idx: usize,
        expr: ExpressionId,
        type_param: TypeParameterId,
    ) -> Result<()> {
        let Some(expected_type_of_expr) = tcx.expected_type_of(expr)? else {
            let span = tcx.hir_span_of_def(DefId::Expression(expr));
            let type_param_name = tcx.tdb().type_parameter(type_param).unwrap().name.to_owned();

            tcx.dcx().emit(
                crate::errors::TypeArgumentInferenceFailed {
                    source: span,
                    type_param_name,
                }
                .into(),
            );

            return Ok(());
        };

        let Some(expected_type_of_param) = expected_type_of_expr.type_arguments.get(idx) else {
            let type_param = tcx.tdb().type_parameter(type_param).unwrap();

            tcx.dcx().emit(
                crate::errors::TypeArgumentInferenceFailed {
                    source: type_param.location,
                    type_param_name: type_param.name.clone(),
                }
                .into(),
            );

            return Ok(());
        };

        let work_key = TypeParameterReference {
            entry: Entry::Expression(expr),
            param: type_param,
        };

        let substitute = TypeArgumentSubstitute {
            idx,
            ty: expected_type_of_param.to_owned(),
        };

        self.substitution_map.insert(work_key, substitute);

        Ok(())
    }
}

impl UnificationPass {
    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn substitute_type_args<'tcx>(&self, tcx: &'tcx mut TyInferCtx) -> Result<()> {
        for (reference, type_arg) in &self.substitution_map {
            let TypeParameterReference { entry, .. } = reference;
            let TypeArgumentSubstitute { idx, ty } = type_arg;

            let hir_ty = tcx.hir_lift_type(ty)?;

            match *entry {
                Entry::Expression(expr) => {
                    let expr = tcx.hir.expression_mut(expr).unwrap();

                    let segment = match &mut expr.kind {
                        lume_hir::ExpressionKind::Cast(cast) => &mut cast.target.name.name,
                        lume_hir::ExpressionKind::Construct(construct) => &mut construct.path.name,
                        lume_hir::ExpressionKind::StaticCall(call) => call.name.root.last_mut().unwrap(),
                        lume_hir::ExpressionKind::Variant(variant) => &mut variant.name.name,
                        kind => unreachable!("bug!: unimplemented expression substitute: {kind:#?}"),
                    };

                    let mut existing_type_args = segment.type_arguments().to_vec();
                    existing_type_args.insert(*idx, hir_ty);

                    segment.place_type_arguments(existing_type_args);
                }
                Entry::Statement(_) => todo!(),
            }
        }

        tcx.dcx().ensure_untainted()?;

        Ok(())
    }
}

impl UnificationPass {
    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn verify_fulfilled_paths<'tcx>(&self, tcx: &'tcx TyInferCtx) -> Result<()> {
        for stmt in tcx.hir.statements.values() {
            if let lume_hir::StatementKind::Variable(decl) = &stmt.kind
                && let Some(declared_type) = &decl.declared_type
            {
                self.verify_type_name(tcx, &declared_type.name)?;
            }
        }

        for expr in tcx.hir.expressions.values() {
            match &expr.kind {
                lume_hir::ExpressionKind::Cast(expr) => {
                    self.verify_type_name(tcx, &expr.target.name)?;
                }
                lume_hir::ExpressionKind::Construct(expr) => {
                    self.verify_type_name(tcx, &expr.path)?;
                }
                lume_hir::ExpressionKind::StaticCall(expr) => {
                    self.verify_type_name(tcx, &expr.name)?;
                }
                lume_hir::ExpressionKind::Variant(expr) => {
                    self.verify_type_name(tcx, &expr.name)?;
                }
                _ => {}
            }
        }

        tcx.dcx().ensure_untainted()?;

        Ok(())
    }
}
