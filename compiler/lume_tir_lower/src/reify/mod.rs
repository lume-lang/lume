pub mod metadata;
mod visitor;

use lume_errors::Result;
use lume_span::Internable;
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
    /// **Note:** the `$T` parameter is not accessible within the function body and
    /// is only shown for presentation purposes.
    fn add_metadata_params(&self, func: &mut Function) {
        let type_params = std::mem::take(&mut func.type_params.inner);

        for type_param in &type_params {
            let name = format!("${}", type_param.name).intern();
            let ty = self.tcx.std_type();

            func.parameters.push(Parameter {
                index: func.parameters.len(),
                var: type_param.var,
                name,
                ty,
                vararg: false,
            });
        }

        func.type_params.inner = type_params;
    }

    fn add_metadata_calls(&mut self, func: &mut Function) -> Result<()> {
        self.visit(func)
    }

    fn add_metadata_arguments_on_call(&mut self, call: &mut Call) -> Result<()> {
        let mut metadata_args = Vec::with_capacity(call.type_arguments.len());

        for (idx, type_arg) in call.type_arguments.iter().enumerate() {
            let argument = if self.tcx.is_type_parameter(type_arg)? {
                self.add_metadata_argument_inherited(call, idx, type_arg)
            } else {
                self.add_metadata_argument_concrete(type_arg)?
            };

            metadata_args.push(argument);
        }

        call.arguments.append(&mut metadata_args);

        Ok(())
    }

    fn add_metadata_argument_inherited(&self, call: &Call, idx: usize, type_arg: &TypeRef) -> lume_tir::Expression {
        let name = format!("${idx}").intern();

        let parent_func_params = self.tcx.hir_avail_params(lume_span::DefId::Expression(call.id));
        let param_idx = parent_func_params.len() + idx;
        let reference = lume_tir::VariableId(param_idx);

        lume_tir::Expression {
            kind: ExpressionKind::Variable(Box::new(lume_tir::VariableReference {
                id: lume_span::ExpressionId::default(),
                reference,
                source: lume_tir::VariableSource::Parameter,
                name,
            })),
            ty: type_arg.clone(),
        }
    }

    fn add_metadata_argument_concrete(&mut self, type_arg: &TypeRef) -> Result<lume_tir::Expression> {
        let id = self.build_type_metadata_of(type_arg)?;

        Ok(lume_tir::Expression {
            kind: ExpressionKind::IntrinsicCall(Box::new(lume_tir::IntrinsicCall {
                id: lume_span::ExpressionId::default(),
                kind: lume_tir::IntrinsicKind::Metadata { id },
                arguments: Vec::new(),
            })),
            ty: TypeRef::pointer(self.tcx.std_type()),
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
