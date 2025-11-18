mod diagnostics;

use indexmap::IndexMap;
use lume_errors::Result;
use lume_span::*;
use lume_types::TypeRef;

use crate::TyInferCtx;
use crate::query::Callable;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
struct TypeParameterReference {
    pub(crate) entry: NodeId,
    pub(crate) param: NodeId,
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
    #[libftrace::traced(level = Info, err)]
    pub(crate) fn invoke<'tcx>(mut self, tcx: &'tcx mut TyInferCtx) -> Result<()> {
        self.unify(tcx)?;
        self.substitute_type_args(tcx)?;
        self.verify_fulfilled_paths(tcx)?;

        Ok(())
    }
}

impl UnificationPass {
    #[libftrace::traced(level = Debug, err)]
    fn unify<'tcx>(&mut self, tcx: &'tcx TyInferCtx) -> Result<()> {
        for (id, item) in &tcx.hir.nodes {
            if !tcx.hir_is_local_node(*id) {
                continue;
            }

            match item {
                lume_hir::Node::Type(ty) => match ty {
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
                lume_hir::Node::Impl(impl_block) => {
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
                lume_hir::Node::TraitImpl(trait_impl) => {
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
                lume_hir::Node::Function(func) => {
                    for param in &func.parameters {
                        self.verify_type_name(tcx, &param.param_type.name)?;
                    }

                    self.verify_type_name(tcx, &func.return_type.name)?;

                    if let Some(block) = &func.block {
                        self.unify_block(tcx, block)?;
                    }
                }
                _ => {}
            }
        }

        tcx.dcx().ensure_untainted()?;

        Ok(())
    }

    #[libftrace::traced(level = Trace, err)]
    fn unify_block<'tcx>(&mut self, tcx: &'tcx TyInferCtx, block: &lume_hir::Block) -> Result<()> {
        for stmt in &block.statements {
            self.unify_stmt(tcx, *stmt)?;
        }

        Ok(())
    }

    #[libftrace::traced(level = Trace, err)]
    fn unify_stmt<'tcx>(&mut self, tcx: &'tcx TyInferCtx, stmt: NodeId) -> Result<()> {
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

    #[libftrace::traced(level = Trace, err)]
    fn unify_expr<'tcx>(&mut self, tcx: &'tcx TyInferCtx, expr: NodeId) -> Result<()> {
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
                self.unify_pattern(tcx, expr.id, &s.pattern)?;
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
            lume_hir::ExpressionKind::Literal(_) | lume_hir::ExpressionKind::Variable(_) => (),
        }

        Ok(())
    }

    #[libftrace::traced(level = Trace, err)]
    fn unify_pattern<'tcx>(&mut self, tcx: &'tcx TyInferCtx, expr: NodeId, pattern: &lume_hir::Pattern) -> Result<()> {
        match &pattern.kind {
            lume_hir::PatternKind::Literal(_)
            | lume_hir::PatternKind::Identifier(_)
            | lume_hir::PatternKind::Wildcard(_) => {}
            lume_hir::PatternKind::Variant(variant) => {
                self.enqueue_path_unification(tcx, expr, &variant.name)?;

                for field in &variant.fields {
                    self.unify_pattern(tcx, expr, field)?;
                }
            }
        }

        Ok(())
    }

    #[libftrace::traced(level = Trace, err)]
    fn verify_type_name<'tcx>(&self, tcx: &'tcx TyInferCtx, path: &lume_hir::Path) -> Result<()> {
        let mut type_path = path.clone();

        // Strip back the path until we have the actual type of the path.
        //
        // This is mostly for when a method path is passed, which would throw off the
        // method.
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

    #[libftrace::traced(level = Trace, err)]
    fn enqueue_path_unification<'tcx>(
        &mut self,
        tcx: &'tcx TyInferCtx,
        expr: NodeId,
        path: &lume_hir::Path,
    ) -> Result<()> {
        let expected_type_args = path.all_type_arguments().len();

        match &path.name {
            lume_hir::PathSegment::Type { .. } => {
                let Some(matching_type) = tcx.tdb().find_type(path) else {
                    return Err(crate::errors::MissingType {
                        name: path.to_owned(),
                        source: tcx.hir_span_of_node(expr),
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
                        source: tcx.hir_span_of_node(expr),
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
                    if let lume_hir::ExpressionKind::Variant(variant) = &tcx.hir_expect_expr(expr).kind
                        && self.resolve_variant_type_param(tcx, idx, variant, type_param.id)?
                    {
                        continue;
                    }

                    self.resolve_type_param(tcx, idx, expr, type_param.id)?;
                }
            }
            lume_hir::PathSegment::Namespace { .. } => return Ok(()),
        }

        Ok(())
    }

    #[libftrace::traced(level = Trace, err)]
    fn resolve_type_param<'tcx>(
        &mut self,
        tcx: &'tcx TyInferCtx,
        idx: usize,
        expr: NodeId,
        type_param: NodeId,
    ) -> Result<()> {
        let Some(expected_type_of_expr) = tcx.expected_type_of(expr)? else {
            let span = tcx.hir_span_of_node(expr);
            let type_param_name = tcx.tdb().type_parameter(type_param).unwrap().name.clone();

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
            let span = tcx.hir_span_of_node(expr);
            let type_param = tcx.tdb().type_parameter(type_param).unwrap();

            tcx.dcx().emit(
                crate::errors::TypeArgumentInferenceFailed {
                    source: span,
                    type_param_name: type_param.name.clone(),
                }
                .into(),
            );

            return Ok(());
        };

        let work_key = TypeParameterReference {
            entry: expr,
            param: type_param,
        };

        let substitute = TypeArgumentSubstitute {
            idx,
            ty: expected_type_of_param.to_owned(),
        };

        self.substitution_map.insert(work_key, substitute);

        Ok(())
    }

    #[libftrace::traced(level = Trace, err)]
    fn resolve_variant_type_param<'tcx>(
        &mut self,
        tcx: &'tcx TyInferCtx,
        idx: usize,
        variant: &lume_hir::Variant,
        type_param: NodeId,
    ) -> Result<bool> {
        let work_key = TypeParameterReference {
            entry: variant.id,
            param: type_param,
        };

        for arg in &variant.arguments {
            let arg_type = tcx.type_of(*arg)?;

            let Some(expected_type_of_arg) = tcx.expected_type_of(*arg)? else {
                continue;
            };

            if self.resolve_variant_type_param_nested(
                tcx,
                work_key,
                idx,
                &expected_type_of_arg,
                &arg_type,
                type_param,
            )? {
                return Ok(true);
            }
        }

        Ok(false)
    }

    #[libftrace::traced(level = Trace, err)]
    fn resolve_variant_type_param_nested<'tcx>(
        &mut self,
        tcx: &'tcx TyInferCtx,
        work_key: TypeParameterReference,
        idx: usize,
        expected_type: &TypeRef,
        found_type: &TypeRef,
        type_param: NodeId,
    ) -> Result<bool> {
        if let Some(arg_type_param) = tcx.as_type_parameter(expected_type)?
            && arg_type_param.id == type_param
        {
            let substitute = TypeArgumentSubstitute {
                idx,
                ty: found_type.to_owned(),
            };

            self.substitution_map.insert(work_key, substitute);

            return Ok(true);
        }

        for (expected_type_arg, found_type_arg) in expected_type
            .type_arguments
            .iter()
            .zip(found_type.type_arguments.iter())
        {
            if self.resolve_variant_type_param_nested(
                tcx,
                work_key,
                idx,
                expected_type_arg,
                found_type_arg,
                type_param,
            )? {
                return Ok(true);
            }
        }

        Ok(false)
    }
}

impl UnificationPass {
    #[libftrace::traced(level = Debug, err)]
    fn substitute_type_args<'tcx>(&self, tcx: &'tcx mut TyInferCtx) -> Result<()> {
        for (reference, type_arg) in &self.substitution_map {
            let TypeParameterReference { entry, .. } = reference;
            let TypeArgumentSubstitute { idx, ty } = type_arg;

            let hir_ty = tcx.hir_lift_type(ty)?;
            let node = tcx.hir.node_mut(*entry).unwrap();

            match node {
                lume_hir::Node::Expression(expr) => {
                    let segment = match &mut expr.kind {
                        lume_hir::ExpressionKind::Cast(cast) => &mut cast.target.name.name,
                        lume_hir::ExpressionKind::Construct(construct) => &mut construct.path.name,
                        lume_hir::ExpressionKind::StaticCall(call) => call.name.root.last_mut().unwrap(),
                        lume_hir::ExpressionKind::Variant(variant) => variant.name.root.last_mut().unwrap(),
                        kind => unreachable!("bug!: unimplemented expression substitute: {kind:#?}"),
                    };

                    let mut existing_type_args = segment.type_arguments().to_vec();
                    existing_type_args.insert(*idx, hir_ty);

                    segment.place_type_arguments(existing_type_args);
                }
                _ => todo!(),
            }
        }

        tcx.dcx().ensure_untainted()?;

        Ok(())
    }
}

impl UnificationPass {
    #[libftrace::traced(level = Debug, err)]
    fn verify_fulfilled_paths<'tcx>(&self, tcx: &'tcx TyInferCtx) -> Result<()> {
        for stmt in tcx.hir.statements() {
            if let lume_hir::StatementKind::Variable(decl) = &stmt.kind
                && let Some(declared_type) = &decl.declared_type
            {
                self.verify_type_name(tcx, &declared_type.name)?;
            }
        }

        for expr in tcx.hir.expressions() {
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
