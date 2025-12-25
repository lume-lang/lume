pub mod metadata;
mod visitor;

use lume_errors::Result;
use lume_span::{Internable, NodeId};
use lume_tir::{Call, ExpressionKind, Function, Parameter};
use lume_type_metadata::*;
use lume_typech::TyCheckCtx;
use lume_types::TypeRef;

use crate::reify::visitor::Visitor;

pub(crate) struct ReificationPass<'tcx> {
    tcx: &'tcx TyCheckCtx,

    pub(crate) static_metadata: StaticMetadata,
}

impl<'tcx> ReificationPass<'tcx> {
    pub fn new(tcx: &'tcx TyCheckCtx) -> Self {
        Self {
            tcx,
            static_metadata: StaticMetadata::default(),
        }
    }

    /// Executes the pass over the given function.
    pub fn execute(&mut self, func: &mut Function) -> Result<&StaticMetadata> {
        self.add_metadata_params(func);
        self.add_metadata_calls(func)?;

        Ok(&self.static_metadata)
    }

    /// Add new parameters to the given function, which define metadata about
    /// the given type parameters on the function.
    ///
    /// In effect, a function like this:
    /// ```rs
    /// fn identity<T>(value: T) -> T {
    ///     return value;
    /// }
    /// ```
    /// gets turned into:
    /// ```rs
    /// fn identity<T>(value: T, $T: std::Type) -> T {
    ///     return value;
    /// }
    /// ```
    ///
    /// **Note:** the `$T` parameter is not accessible within the function body
    /// and is only shown for presentation purposes.
    fn add_metadata_params(&self, func: &mut Function) {
        for type_param in &func.type_params {
            let name = format!("${}", type_param.name).intern();
            let ty = self.tcx.std_type();

            func.parameters.push(Parameter {
                index: func.parameters.len(),
                var: type_param.var,
                name,
                ty,
                vararg: false,
                location: func.location,
            });
        }

        if self.is_dynamic_dispatch_call(func.id) {
            let name = String::from("$_dyn").intern();
            let ty = self.tcx.std_type();

            func.parameters.push(Parameter {
                index: func.parameters.len(),
                var: lume_tir::VariableId(usize::MAX),
                name,
                ty,
                vararg: false,
                location: func.location,
            });
        }
    }

    fn is_dynamic_dispatch_call(&self, id: NodeId) -> bool {
        if let Some(lume_hir::Node::TraitMethodDef(method_def)) = self.tcx.hir_node(id)
            && method_def.signature().is_instanced()
        {
            true
        } else {
            false
        }
    }

    fn add_metadata_calls(&mut self, func: &mut Function) -> Result<()> {
        self.visit(func)
    }

    fn add_metadata_arguments_on_call(&mut self, call: &mut Call) -> Result<()> {
        let type_param_len = self.tcx.available_type_params_at(call.function).len();
        let type_arg_len = call.type_arguments.len();

        let mut metadata_args = Vec::with_capacity(call.type_arguments.len());

        for (idx, type_arg) in call
            .type_arguments
            .iter()
            .skip(type_arg_len.saturating_sub(type_param_len))
            .enumerate()
        {
            let argument = if self.tcx.is_type_parameter(type_arg) {
                self.add_metadata_argument_inherited(call, idx, type_arg)
            } else {
                self.add_metadata_argument_concrete(type_arg)?
            };

            metadata_args.push(argument);
        }

        if self.is_dynamic_dispatch_call(call.function) {
            let idx = call.type_arguments.len();
            let instance_ty = &call.arguments.first().unwrap().ty;

            let argument = if self.tcx.is_type_parameter(instance_ty) {
                self.add_metadata_argument_inherited(call, idx, instance_ty)
            } else {
                self.add_metadata_argument_concrete(instance_ty)?
            };

            metadata_args.push(argument);
        }

        call.arguments.append(&mut metadata_args);

        Ok(())
    }

    fn add_metadata_argument_inherited(&self, call: &Call, idx: usize, type_arg: &TypeRef) -> lume_tir::Expression {
        let name = format!("${idx}").intern();

        let parent_func_params = self.tcx.available_params_at(call.id);
        let param_idx = parent_func_params.len() + idx;
        let reference = lume_tir::VariableId(param_idx);

        lume_tir::Expression {
            kind: ExpressionKind::Variable(Box::new(lume_tir::VariableReference {
                id: lume_span::NodeId::default(),
                reference,
                source: lume_tir::VariableSource::Parameter,
                name,
                location: type_arg.location,
            })),
            ty: type_arg.clone(),
        }
    }

    fn add_metadata_argument_concrete(&mut self, type_arg: &TypeRef) -> Result<lume_tir::Expression> {
        let id = self.build_type_metadata_of(type_arg)?;
        let ty = self.tcx.std_ref_pointer(self.tcx.std_type());

        Ok(lume_tir::Expression {
            kind: ExpressionKind::IntrinsicCall(Box::new(lume_tir::IntrinsicCall {
                id: lume_span::NodeId::default(),
                kind: lume_tir::IntrinsicKind::Metadata { id },
                arguments: Vec::new(),
                location: type_arg.location,
            })),
            ty,
        })
    }
}

impl visitor::Visitor for ReificationPass<'_> {
    fn visit_expression(&mut self, expr: &mut lume_tir::Expression) -> Result<()> {
        if let ExpressionKind::Call(call) = &mut expr.kind {
            self.add_metadata_arguments_on_call(call)?;
        }

        self.visit_expression_inner(expr)
    }
}
