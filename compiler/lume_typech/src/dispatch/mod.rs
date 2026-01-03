//! Queries centering around dynamic dispatch.

use lume_architect::cached_query;
use lume_errors::Result;
use lume_hir::CallExpression;
use lume_span::NodeId;
use lume_types::TypeRef;

use crate::TyCheckCtx;

/// Represents the source of a dynamic argument for a call.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DispatchTypeSource {
    /// The dynamic argument is inherited from the current function's dynamic
    /// argument.
    InheritDyn,

    /// The dynamic argument is inherited from one of the current function's
    /// type parameters.
    InheritTypeParam { type_param: TypeRef },

    /// Create a new metadata type for the given type.
    CreateFrom(TypeRef),
}

impl TyCheckCtx {
    /// Gets the implementing type of the given call expression.
    ///
    /// If the given call expression is a static function call, returns
    /// [`None`].
    #[libftrace::traced(level = Debug, fields(call_expr), ret, err)]
    pub fn impl_ty_of_call(&self, call_expr: NodeId) -> Result<Option<TypeRef>> {
        let call_expr = self.hir_call_expr(call_expr).expect("callable with ID to exist");

        match call_expr {
            CallExpression::Instanced(call) => self.type_of(call.callee).map(Some),
            CallExpression::Intrinsic(call) => self.type_of(call.kind.callee()).map(Some),
            CallExpression::Static(call) => {
                let callable = self.probe_callable(call_expr)?;

                // If an instance method is called statically, the implementing type is the
                // "callee" of the call expression.
                if self.is_instanced_method(callable.id()) {
                    let instance_arg = call.arguments.first();

                    return instance_arg.map(|arg| self.type_of(*arg)).transpose();
                }

                let Some(receiver_name) = call.receiving_type() else {
                    return Ok(None);
                };

                self.find_type_ref_from(&receiver_name, call.id)
            }
        }
    }

    /// Determines whether the callable with the given ID is a dynamic method.
    ///
    /// Dynamic methods are those that are not statically dispatched, but rather
    /// dispatched at runtime based on the type of the receiver.
    #[cached_query]
    #[libftrace::traced(level = Debug, fields(callable_id), ret)]
    pub fn is_callable_dynamic(&self, callable_id: NodeId) -> bool {
        let Some(method_definition) = self.tdb().method(callable_id) else {
            libftrace::warning!("could not find method with id {callable_id}");
            return false;
        };

        libftrace::trace!(
            "method kind `{:+}`: {:?}",
            method_definition.name,
            method_definition.kind
        );

        method_definition.kind == lume_types::MethodKind::TraitDefinition
    }

    /// Determines the source of the dynamic argument for a call.
    #[cached_query(result)]
    #[libftrace::traced(level = Debug, fields(call_expr, target), ret, err)]
    pub fn dynamic_argument_source_of_call(&self, call_expr: NodeId, target: NodeId) -> Result<DispatchTypeSource> {
        assert!(
            self.is_callable_dynamic(target),
            "attempted to add dynamic argument to non-dynamic function call"
        );

        let source_callable = self.hir_parent_callable(call_expr).expect("expr with ID to exist");
        let call_expr = self.hir_call_expr(call_expr).expect("callable with ID to exist");

        if !self.is_callable_dynamic(source_callable.id()) {
            let dyn_instance_type = self.impl_ty_of_call(call_expr.id())?.expect("non-function call");

            if self.is_type_parameter(&dyn_instance_type) {
                return Ok(DispatchTypeSource::InheritTypeParam {
                    type_param: dyn_instance_type.clone(),
                });
            }

            return Ok(DispatchTypeSource::CreateFrom(dyn_instance_type));
        }

        Ok(DispatchTypeSource::InheritDyn)
    }
}
