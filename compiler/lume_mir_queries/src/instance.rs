use lume_mir::{Generics, Instance};
use lume_span::NodeId;
use lume_types::TypeRef;

use crate::MirQueryCtx;

impl MirQueryCtx<'_> {
    #[inline]
    pub fn instance_of(&self, id: NodeId, type_arguments: Vec<TypeRef>) -> Instance {
        let type_parameter_ids = self.tcx().all_type_parameters_of(id);
        // debug_assert_eq!(type_parameter_ids.len(), type_arguments.len());

        if type_arguments.is_empty() {
            return Instance { id, generics: None };
        }

        Instance {
            id,
            generics: Some(Generics {
                ids: type_parameter_ids,
                types: type_arguments,
            }),
        }
    }

    #[inline]
    pub fn instance_of_type(&self, ty: TypeRef) -> Instance {
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

    /// Attempts to instantiate a new [`TypeRef`] from a set of generic
    /// arguments.
    ///
    /// **Arguments**:
    /// - `type_ref`: type reference to instantiate.
    /// - `generics`: any type arguments supplied for the type. Could be the
    ///   generics from a call site or supplied by the user.
    ///
    /// # Example
    ///
    /// Let's say we want to instantiat the type of `value`:
    /// ```lm (ignore,illustration)
    /// fn foo() {
    ///   let value = bar<T, UInt32>();
    /// }
    /// ```
    /// would result in the arguments of:
    /// - `type_ref = type_of(value)`
    /// - `generics = [T, UInt32])`
    pub fn instantiated_type(&self, mut type_ref: TypeRef, generics: &Generics) -> TypeRef {
        // Replace all contained type parameters with their respective canonical type
        // parameter. This helps with mapping the correct type arguments into the
        // type-ref, as they might not use the same type parameter IDs.
        for bound_type in type_ref.walk_mut() {
            if !bound_type.bound_types.is_empty() || !self.tcx().is_type_parameter(bound_type) {
                continue;
            }

            let Some(owner) = self.tcx().hir_parent_node_of(bound_type.instance_of) else {
                continue;
            };

            if let Ok(Some(canonical)) = self.tcx().hir_canonical_type_of(bound_type.instance_of, owner.id()) {
                bound_type.instance_of = canonical.as_node_id();
            }
        }

        for (type_parameter_id, replacement) in generics.iter() {
            type_ref.replace_contained(type_parameter_id, replacement);
        }

        type_ref
    }

    /// Attempts to instantiate a new [`Instance`] from an existing
    /// type-reference and a set of generic arguments. This is shorthand for:
    /// ```ignore (illustrative)
    /// mcx.instance_of_type(mcx.instantiated_type(type_ref, generics))
    /// ```
    ///
    /// **Arguments**:
    /// - `type_ref`: type reference to instantiate.
    /// - `generics`: any type arguments supplied for the type. Could be the
    ///   generics from a call site or supplied by the user.
    ///
    /// # Example
    ///
    /// Let's say we want to instantiat the type of `value`:
    /// ```lm (ignore,illustration)
    /// fn foo() {
    ///   let value = bar<T, UInt32>();
    /// }
    /// ```
    /// would result in the arguments of:
    /// - `type_ref = type_of(value)`
    /// - `generics = [T, UInt32])`
    pub fn instantiated_type_instance(&self, type_ref: TypeRef, generics: &Generics) -> Instance {
        self.instance_of_type(self.instantiated_type(type_ref, generics))
    }
}
