use lume_mir::Instance;
use lume_span::NodeId;
use lume_types::TypeRef;

use crate::MirQueryCtx;

impl MirQueryCtx<'_> {
    #[inline]
    pub fn instance_of(&self, id: NodeId, type_arguments: Vec<TypeRef>) -> lume_mir::Instance {
        let type_parameter_ids = self.tcx().all_type_parameters_of(id);
        // debug_assert_eq!(type_parameter_ids.len(), type_arguments.len());

        if type_arguments.is_empty() {
            return lume_mir::Instance { id, generics: None };
        }

        lume_mir::Instance {
            id,
            generics: Some(lume_mir::Generics {
                ids: type_parameter_ids,
                types: type_arguments,
            }),
        }
    }

    #[inline]
    pub fn instance_of_type(&self, ty: TypeRef) -> lume_mir::Instance {
        self.instance_of(ty.instance_of, ty.bound_types)
    }

    /// Attempts to instantiate a new [`Instance`] from an existing MIR call
    /// site.
    ///
    /// **Arguments**:
    /// - `owner`: function instance in which the call expression is located.
    /// - `func`: ID of the function which the call expression calls.
    /// - `type_arguments`: any type arguments supplied at the call site.
    ///
    /// # Example
    ///
    /// ```lm (ignore,illustration)
    /// fn foo<T>() {
    ///   let _ = bar<T, UInt32>();
    /// }
    /// ```
    /// would result in the arguments of:
    /// - `owner = instance_of(foo<T>)`
    /// - `func = node_id_of(bar<T>)`
    /// - `type_arguments = [T, UInt32])`
    pub fn instantiated_instance(&self, owner: &Instance, func: NodeId, type_arguments: Vec<TypeRef>) -> Instance {
        let mut instance = self.instance_of(func, type_arguments);

        let Some(instance_generics) = &mut instance.generics else {
            return instance;
        };

        let Some(owner_generics) = &owner.generics else {
            return instance;
        };

        for type_argument in &mut instance_generics.types {
            for (type_parameter_id, replacement) in owner_generics.iter() {
                type_argument.replace_contained(type_parameter_id, replacement);
            }
        }

        instance
    }
}
