use indexmap::IndexMap;
use lume_errors::Result;
use lume_span::{Internable, NodeId};
use lume_tir::{Call, ExpressionKind, Function, VariableId};
use lume_type_metadata::*;
use lume_typech::TyCheckCtx;
use lume_typech::dispatch::DispatchTypeSource;
use lume_types::TypeRef;

pub(crate) struct ReificationPass<'tcx> {
    tcx: &'tcx TyCheckCtx,
    dyna: IndexMap<NodeId, FnTypeParameters>,
}

#[derive(Default)]
struct FnTypeParameters {
    /// Parameter indices of the keyed type parameters.
    pub dyn_params: IndexMap<NodeId, usize>,

    /// Optional variable ID of the dynamic anchor parameter.
    pub dyn_anchor: Option<VariableId>,
}

impl<'tcx> ReificationPass<'tcx> {
    pub fn new(tcx: &'tcx TyCheckCtx) -> Self {
        Self {
            tcx,
            dyna: IndexMap::new(),
        }
    }
}

impl ReificationPass<'_> {
    /// Add new parameters to the given function, which define metadata about
    /// the given type parameters on the function.
    ///
    /// In effect, a function like this:
    /// ```lm
    /// fn identity<T>(value: T) -> T {
    ///     return value;
    /// }
    /// ```
    /// gets turned into:
    /// ```lm
    /// fn identity<T>(value: T, $T: std::Type) -> T {
    ///     return value;
    /// }
    /// ```
    ///
    /// **Note:** the `$T` parameter is not accessible within the function body
    /// and is only shown for presentation purposes.
    pub(crate) fn add_metadata_parameters(&mut self, func: &mut Function) {
        let mut dyna = FnTypeParameters::default();

        for type_param in func.type_params.clone() {
            let index = func.add_parameter(
                format!("${}", type_param.name),
                self.tcx.std_type(),
                type_param.var,
                lume_tir::ParameterKind::TypeMetadata {
                    type_parameter_id: type_param.id,
                },
                func.location,
            );

            tracing::info!(
                func = func.name_as_str(),
                type_parameter = type_param.name,
                index,
                "add_metadata_type_parameter",
            );

            dyna.dyn_params.insert(type_param.id, index);
        }

        if self.tcx.is_callable_dynamic(func.id) {
            dyna.dyn_anchor = Some(self.add_dynamic_instance_parameter(func));
        }

        assert!(self.dyna.insert(func.id, dyna).is_none());
    }

    /// Add a new parameter to the given function, which defines the dynamic
    /// instance type on which the dynamic dispatch is performed.
    ///
    /// In effect, a trait method like this:
    /// ```lm
    /// trait Default {
    ///     fn default() -> Self;
    /// }
    /// ```
    /// gets turned into:
    /// ```lm
    /// trait Default {
    ///     fn default($dyn: std::Type) -> Self {
    ///         // use $dyn to find the matching method implementation,
    ///         // which is lowered in the MIR.
    ///     }
    /// }
    /// ```
    ///
    /// **Note:** the `$T` parameter is not accessible within the function body
    /// and is only shown for presentation purposes.
    fn add_dynamic_instance_parameter(&mut self, func: &mut Function) -> VariableId {
        let anchor_variable_id = VariableId(usize::MAX);

        func.add_parameter(
            "$_dyn",
            self.tcx.std_type(),
            anchor_variable_id,
            lume_tir::ParameterKind::DynamicAnchor,
            func.location,
        );

        anchor_variable_id
    }
}

impl ReificationPass<'_> {
    /// For each function call to a function which was altered by
    /// [`Self::add_metadata_parameters()`], add matching metadata arguments
    /// to each call site.
    ///
    /// Transforms call sites such as this:
    /// ```lm
    /// fn foo<T>($T: std::Type) -> T {
    ///     return T::default();
    /// }
    ///
    /// fn foo_i32() -> Int32 {
    ///     return Int32::default();
    /// }
    /// ```
    /// into this:
    /// ```lm
    /// fn foo<T>($T: std::Type) -> T {
    ///     return Default::default($T);
    /// }
    ///
    /// fn foo_i32() -> Int32 {
    ///     return Default::default(std::metadata_of<Int32>());
    /// }
    /// ```
    ///
    /// **Note:** the `$T` parameter is not accessible within the function body
    /// and is only shown for presentation purposes.
    pub fn add_metadata_arguments(&mut self, func: &mut Function) -> Result<()> {
        lume_tir::traverse(func, self)?;

        Ok(())
    }

    fn add_metadata_arguments_on_call(&mut self, call: &mut Call) {
        let type_param_len = self.tcx.available_type_params_at(call.function).len();
        let type_arg_len = call.type_arguments.len();
        let remaining_type_args = type_param_len.saturating_sub(type_arg_len);

        for (idx, type_arg) in call.type_arguments.iter().enumerate().skip(remaining_type_args) {
            let argument = if self.tcx.is_type_parameter(type_arg) {
                self.inherited_metadata_arg(call, format!("${idx}"), lume_tir::VariableId(idx))
            } else {
                self.concrete_metadata_arg(type_arg)
            };

            call.arguments.push(argument);
        }
    }

    /// Creates a new TIR expression which references an existing function
    /// parameter with variable ID `reference`.
    fn inherited_metadata_arg(
        &self,
        call: &Call,
        name: String,
        reference: lume_tir::VariableId,
    ) -> lume_tir::Expression {
        lume_tir::Expression {
            kind: ExpressionKind::Variable(Box::new(lume_tir::VariableReference {
                id: lume_span::NodeId::default(),
                reference,
                source: lume_tir::VariableSource::Parameter,
                name: name.intern(),
                location: call.location,
            })),
            ty: self.tcx.std_type(),
        }
    }

    /// Creates a new TIR intrinsic expression which creates a new metadata
    /// instance for the given type.
    fn concrete_metadata_arg(&mut self, type_arg: &TypeRef) -> lume_tir::Expression {
        let id = TypeMetadataId::from(type_arg);

        lume_tir::Expression {
            kind: ExpressionKind::IntrinsicCall(Box::new(lume_tir::IntrinsicCall {
                id: lume_span::NodeId::default(),
                kind: lume_tir::IntrinsicKind::Metadata { id },
                arguments: Vec::new(),
                location: type_arg.location,
            })),
            ty: self.tcx.std_type(),
        }
    }
}

impl ReificationPass<'_> {
    /// Complement of [`add_dynamic_instance_parameter`], this methods adds the
    /// corresponding argument to the given function call, matching the function
    /// which it calls.
    #[tracing::instrument(level = "DEBUG", skip_all, err)]
    fn add_dynamic_instance_argument(&mut self, call: &mut Call) -> Result<()> {
        let current_fn = self.tcx.hir_parent_callable(call.id).unwrap();
        let fn_dynamic = self.dyna.get(&current_fn.id()).expect("expected dynamic map to exist");

        let argument = match self.tcx.dynamic_argument_source_of_call(call.id, call.function)? {
            DispatchTypeSource::CreateFrom(source_type) => {
                tracing::info!(
                    call = self
                        .tcx
                        .hir_path_of_node(self.tcx.hir_parent_callable(call.id).unwrap().id())
                        .to_wide_string(),
                    target = self.tcx.hir_path_of_node(call.function).to_wide_string(),
                    "create dispatch type {}",
                    self.tcx.new_named_type(&source_type, true).unwrap(),
                );

                self.concrete_metadata_arg(&source_type)
            }

            DispatchTypeSource::InheritDyn => {
                tracing::info!(
                    call = self
                        .tcx
                        .hir_path_of_node(self.tcx.hir_parent_callable(call.id).unwrap().id())
                        .to_wide_string(),
                    target = self.tcx.hir_path_of_node(call.function).to_wide_string(),
                    "inherit dispatch type from parent dynamic parameter",
                );

                let dyn_param_ref = fn_dynamic.dyn_anchor.expect("dynamic anchor parameter set");

                self.inherited_metadata_arg(call, String::from("$_dyn"), dyn_param_ref)
            }

            DispatchTypeSource::InheritTypeParam { type_param } => {
                tracing::info!(
                    call = self
                        .tcx
                        .hir_path_of_node(self.tcx.hir_parent_callable(call.id).unwrap().id())
                        .to_wide_string(),
                    target = self.tcx.hir_path_of_node(call.function).to_wide_string(),
                    "inherit dispatch type from parent type parameter",
                );

                let dyn_param_idx = *fn_dynamic
                    .dyn_params
                    .get(&type_param.instance_of)
                    .expect("dynamic instance parameter set");

                self.inherited_metadata_arg(call, format!("${dyn_param_idx}"), lume_tir::VariableId(dyn_param_idx))
            }
        };

        call.arguments.push(argument);

        Ok(())
    }
}

impl lume_tir::Visitor for ReificationPass<'_> {
    fn visit_expr(&mut self, expr: &mut lume_tir::Expression) -> Result<()> {
        if let ExpressionKind::Call(call) = &mut expr.kind {
            self.add_metadata_arguments_on_call(call);

            if self.tcx.is_callable_dynamic(call.function) {
                self.add_dynamic_instance_argument(call)?;
            }
        }

        Ok(())
    }
}
