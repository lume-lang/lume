use indexmap::IndexMap;
use lume_errors::Result;
use lume_span::Internable;
use lume_tir::{Call, ExpressionKind, Function, Parameter, VariableId};
use lume_type_metadata::*;
use lume_typech::TyCheckCtx;
use lume_typech::dispatch::DispatchTypeSource;
use lume_types::TypeRef;

pub(crate) struct ReificationPass<'tcx> {
    tcx: &'tcx TyCheckCtx,

    dyn_type_params: IndexMap<TypeRef, usize>,
    dyn_instance_param: Option<VariableId>,
}

impl<'tcx> ReificationPass<'tcx> {
    pub fn new(tcx: &'tcx TyCheckCtx) -> Self {
        Self {
            tcx,
            dyn_type_params: IndexMap::new(),
            dyn_instance_param: None,
        }
    }

    /// Executes the pass over the given function.
    pub fn execute(&mut self, func: &mut Function) -> Result<()> {
        self.add_metadata_params(func);

        if self.tcx.is_callable_dynamic(func.id) {
            self.add_dynamic_instance_parameter(func);
        }

        lume_tir::traverse(func, self)?;

        self.dyn_type_params.clear();
        self.dyn_instance_param = None;

        Ok(())
    }
}

impl ReificationPass<'_> {
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
    fn add_metadata_params(&mut self, func: &mut Function) {
        for type_param in &func.type_params {
            let name = format!("${}", type_param.name).intern();
            let ty = self.tcx.std_type();

            let index = func.parameters.len();

            func.parameters.push(Parameter {
                index,
                var: type_param.var,
                name,
                ty,
                vararg: false,
                location: func.location,
            });

            self.dyn_type_params.insert(type_param.type_ref.clone(), index);
        }
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
    fn add_dynamic_instance_parameter(&mut self, func: &mut Function) {
        let name = String::from("$_dyn").intern();
        let ty = self.tcx.std_type();
        let var = VariableId(usize::MAX);
        let index = func.parameters.len();

        func.parameters.push(Parameter {
            index,
            var,
            name,
            ty,
            vararg: false,
            location: func.location,
        });

        self.dyn_instance_param = Some(var);
    }

    /// Complement of [`add_dynamic_instance_parameter`], this methods adds the
    /// corresponding argument to the given function call, matching the function
    /// which it calls.
    #[libftrace::traced(level = Debug, err)]
    fn add_dynamic_instance_argument(&mut self, call: &mut Call) -> Result<()> {
        let argument = match self.tcx.dynamic_argument_source_of_call(call.id, call.function)? {
            DispatchTypeSource::CreateFrom(source_type) => {
                libftrace::info!(
                    "create dispatch type {}",
                    self.tcx.new_named_type(&source_type, true).unwrap(),
                    call = self
                        .tcx
                        .hir_path_of_node(self.tcx.hir_parent_callable(call.id).unwrap().id())
                        .to_wide_string(),
                    target = self.tcx.hir_path_of_node(call.function).to_wide_string()
                );

                self.concrete_metadata_arg(&source_type)
            }

            DispatchTypeSource::InheritDyn => {
                libftrace::info!(
                    "inherit dispatch type from parent dynamic parameter",
                    call = self
                        .tcx
                        .hir_path_of_node(self.tcx.hir_parent_callable(call.id).unwrap().id())
                        .to_wide_string(),
                    target = self.tcx.hir_path_of_node(call.function).to_wide_string()
                );

                let dyn_param_ref = self.dyn_instance_param.expect("dynamic instance parameter set");

                self.inherited_metadata_arg(call, String::from("$_dyn"), dyn_param_ref)
            }

            DispatchTypeSource::InheritTypeParam { type_param } => {
                libftrace::info!(
                    "inherit dispatch type from parent type parameter",
                    call = self
                        .tcx
                        .hir_path_of_node(self.tcx.hir_parent_callable(call.id).unwrap().id())
                        .to_wide_string(),
                    target = self.tcx.hir_path_of_node(call.function).to_wide_string()
                );

                let dyn_param_idx = *self
                    .dyn_type_params
                    .get(&type_param)
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
