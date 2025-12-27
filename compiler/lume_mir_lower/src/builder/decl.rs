use lume_mir::*;
use lume_span::{Location, NodeId};
use lume_types::TypeRef;

use crate::builder::Builder;

/// Defines how to handle when declaring an operand, which itself is a
/// reference to another register.
///
/// When an operand is declared using [`Builder::declare_operand`], it will
/// attempt to use the "head" register, if the operand itself refers to a
/// register.
///
/// The following MIR:
/// ```mir
/// B0:
///     let #1: u64 = 4_u64
/// ```
/// declares register `#1` to to have some arbitrary declaration. If we were to
/// declare a new operand whose value is a reference to `#1`, `declare_operand`
/// won't create a new register with a reference, but instead just return the
/// register which the operand refers to.
///
/// In practice, this causes any intermediate references to be eliminated.
/// Instead of this:
/// ```mir
/// B0:
///     let #1: u64 = 4_u64
///     let #2: u64 = #1
///     let #3: void = call foo(#2)
/// ```
/// it would prefer this output:
/// ```mir
/// B0:
///     let #1: u64 = 4_u64
///     let #3: void = call foo(#1)
/// ```
///
/// The first output is only used if [`OperandRef::Direct`] is used. The default
/// is [`OperandRef::Explicit`], which causes the operand to resolve like the
/// second output.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum OperandRef {
    /// Skip over any intermediate registers which the operand may refer to, to
    /// prevent register globber.
    #[default]
    Implicit,

    /// Use the value within the operand, regardless of it's variant.
    Explicit,
}

impl Builder<'_, '_> {
    /// Declares a new register with the given declaration value, where the
    /// register type is inferred from the declaration.
    ///
    /// To explicitly set the type of the declaration, use [`Self::declare_as`].
    pub(crate) fn declare(&mut self, decl: Declaration) -> RegisterId {
        let ty = self.type_of_decl(&decl);

        self.declare_as(ty, decl)
    }

    /// Declares a new register with the given declaration value, along with an
    /// explicitly defined type.
    ///
    /// To infer the type of the declaration, use [`Self::declare`].
    pub(crate) fn declare_as(&mut self, ty: Type, decl: Declaration) -> RegisterId {
        self.func.declare(ty, decl)
    }

    /// Declares a new register with the given operand value, where the
    /// register type is inferred from the operand.
    ///
    /// To explicitly set the type of the operand, use
    /// [`Self::declare_operand_as`].
    pub(crate) fn declare_operand(&mut self, value: Operand, op_ref: OperandRef) -> RegisterId {
        let ty = self.type_of_value(&value);

        self.declare_operand_as(ty, value, op_ref)
    }

    /// Declares a new register with the given operand value, along with an
    /// explicitly defined type.
    ///
    /// To infer the type of the operand, use [`Self::declare_operand`].
    pub(crate) fn declare_operand_as(&mut self, ty: Type, value: Operand, op_ref: OperandRef) -> RegisterId {
        match op_ref {
            OperandRef::Implicit => self.func.declare_value(ty, value),
            OperandRef::Explicit => self.func.declare_value_raw(ty, value),
        }
    }
}

impl Builder<'_, '_> {
    /// Creates a new instruction for a heap-allocation type and returns the
    /// register which contains the allocation pointer.
    pub(crate) fn alloca(&mut self, ty: Type, type_ref: &TypeRef, location: Location) -> RegisterId {
        let alloc_ptr = self.func.add_register(Type::pointer(ty.clone()));
        let metadata_reg = self.declare_metadata_of(type_ref, location);

        self.func
            .current_block_mut()
            .allocate(alloc_ptr, ty, metadata_reg, location);

        alloc_ptr
    }

    /// Creates a new instruction for a stack-allocation slot and returns it's
    /// ID.
    pub(crate) fn alloc_slot(&mut self, ty: Type, location: Location) -> SlotId {
        let slot = self.func.add_slot(ty.clone());
        self.func.current_block_mut().create_slot(slot, ty, location);

        slot
    }

    /// Loads the given register into a new register, where the type of the
    /// loaded value is explicitly specified.
    pub(crate) fn load_as(&mut self, loaded_type: Type, id: RegisterId, location: Location) -> RegisterId {
        self.declare_operand_as(
            loaded_type.clone(),
            lume_mir::Operand {
                kind: lume_mir::OperandKind::Load { id, loaded_type },
                location,
            },
            OperandRef::Implicit,
        )
    }

    /// Stores the given value into an existing register.
    pub(crate) fn store(&mut self, target: RegisterId, value: Operand, location: Location) {
        self.func.current_block_mut().store(target, value, location);
    }

    /// Stores the given value into the field of an existing register, which
    /// exists at the offset `offset`.
    pub(crate) fn store_field(&mut self, target: RegisterId, value: Operand, offset: usize, location: Location) {
        self.func
            .current_block_mut()
            .store_field(target, offset, value, location);
    }

    /// Stores the given value into an existing slot.
    pub(crate) fn store_slot(&mut self, target: SlotId, value: Operand, offset: usize, location: Location) {
        self.func
            .current_block_mut()
            .store_slot(target, offset, value, location);
    }

    /// Gets the address of an existing slot, plus some offset.
    pub(crate) fn slot_address(&mut self, id: SlotId, offset: usize, location: Location) -> RegisterId {
        let slot_type = self.func.slots.get(&id).unwrap();

        self.declare_operand_as(
            Type::pointer(slot_type.clone()),
            Operand {
                kind: OperandKind::SlotAddress { id, offset },
                location,
            },
            OperandRef::Implicit,
        )
    }
}

impl Builder<'_, '_> {
    /// Defines a new register with a value of `null` and return an reference to
    /// it.
    pub(crate) fn null_const(&mut self) -> lume_mir::Operand {
        let reg = self.declare_operand(
            lume_mir::Operand {
                kind: lume_mir::OperandKind::Boolean { value: false },
                location: Location::empty(),
            },
            OperandRef::Implicit,
        );

        lume_mir::Operand {
            kind: lume_mir::OperandKind::Reference { id: reg },
            location: Location::empty(),
        }
    }

    /// Declares a new register with the comparison result of the two given
    /// operands, as a boolean value.
    pub(crate) fn ieq_imm(&mut self, val: Operand, imm: impl Into<i64>, bits: u8, signed: bool) -> RegisterId {
        let imm_op = lume_mir::Operand {
            kind: lume_mir::OperandKind::Integer {
                bits,
                signed,
                value: imm.into(),
            },
            location: Location::empty(),
        };

        self.declare(lume_mir::Declaration {
            kind: Box::new(lume_mir::DeclarationKind::Intrinsic {
                name: lume_mir::Intrinsic::IntEq { bits, signed },
                args: vec![val, imm_op],
            }),
            location: Location::empty(),
        })
    }

    /// Declares a new register with the value of the two given operands added
    /// together.
    pub(crate) fn iadd_imm(&mut self, val: Operand, imm: impl Into<i64>, bits: u8, signed: bool) -> RegisterId {
        let imm_op = lume_mir::Operand {
            kind: lume_mir::OperandKind::Integer {
                bits,
                signed,
                value: imm.into(),
            },
            location: Location::empty(),
        };

        self.declare(lume_mir::Declaration {
            kind: Box::new(lume_mir::DeclarationKind::Intrinsic {
                name: lume_mir::Intrinsic::IntAdd { bits, signed },
                args: vec![val, imm_op],
            }),
            location: Location::empty(),
        })
    }

    /// Bitcasts the given register to the given amount of bits.
    pub(crate) fn bitcast(&mut self, operand: RegisterId, bits: u8, location: Location) -> RegisterId {
        self.declare(lume_mir::Declaration {
            kind: Box::new(lume_mir::DeclarationKind::Cast { operand, bits }),
            location,
        })
    }

    /// Declares a new register with the returned value of calling the given
    /// function.
    pub(crate) fn call(&mut self, func_id: NodeId, args: Vec<Operand>, location: Location) -> RegisterId {
        let signature = &self.mcx.mir().function(func_id).signature;

        self.call_with_signature(func_id, signature, args, location)
    }

    /// Declares a new register with the returned value of calling the given
    /// function.
    pub(crate) fn call_with_signature(
        &mut self,
        func_id: NodeId,
        signature: &Signature,
        args: Vec<Operand>,
        location: Location,
    ) -> RegisterId {
        let func_name = self.mcx.mir().function(func_id).name;

        let args = self.normalize_call_argumets(&signature.parameters, &args, signature.vararg);
        let return_type = signature.return_type.clone();

        self.declare_as(return_type, lume_mir::Declaration {
            kind: Box::new(lume_mir::DeclarationKind::Call {
                func_id,
                name: func_name,
                args,
            }),
            location,
        })
    }

    /// Declares a new register with the returned value of indirectly calling
    /// the function pointer which is stored in the register `ptr`.
    pub(crate) fn call_indirect(
        &mut self,
        ptr: RegisterId,
        signature: Signature,
        args: Vec<Operand>,
        location: Location,
    ) -> RegisterId {
        let args = self.normalize_call_argumets(&signature.parameters, &args, signature.vararg);
        let return_type = signature.return_type.clone();

        self.declare_as(return_type, lume_mir::Declaration {
            kind: Box::new(lume_mir::DeclarationKind::IndirectCall { ptr, signature, args }),
            location,
        })
    }

    fn normalize_call_argumets(
        &mut self,
        params: &[lume_mir::Parameter],
        args: &[lume_mir::Operand],
        vararg: bool,
    ) -> Vec<lume_mir::Operand> {
        let mut args = args.to_vec();

        // Generic parameters are lowering into accepting pointer types, so all
        // types of argument can be passed.
        //
        // When passing a non-reference argument into a generic parameter, we then
        // need to pass an address to the argument, so the callee can load it. When
        // lowering these arguments, we create a slot in the stack to store the
        // argument, then we pass the address of the stack slot to the function.
        for (arg, param) in args.iter_mut().zip(params.iter()) {
            *arg = self.box_value_if_needed(arg.clone(), &param.type_ref);
        }

        if vararg && args.len() >= params.len() - 1 {
            return self.merge_vararg_operands(params, args);
        }

        args
    }

    fn merge_vararg_operands(
        &mut self,
        params: &[lume_mir::Parameter],
        mut args: Vec<lume_mir::Operand>,
    ) -> Vec<lume_mir::Operand> {
        let last_arg = args.last().map(|a| a.location);

        let mut new_args = args.drain(..params.len() - 1).collect::<Vec<_>>();
        let vararg_type = &params.last().unwrap().type_ref;

        let vararg_loc = last_arg.unwrap_or(vararg_type.location);
        let metadata_reg = self.declare_metadata_of(vararg_type, vararg_loc);

        let array_alloc_func_id = self.tcx().lang_item(lume_hir::LangItem::ArrayWithCapacity).unwrap();
        let array_alloc_func = self.mcx.function(array_alloc_func_id).unwrap();

        let array_push_func_id = self.tcx().lang_item(lume_hir::LangItem::ArrayPush).unwrap();

        let vararg_arr_reg = self
            .func
            .declare(array_alloc_func.signature.return_type.clone(), lume_mir::Declaration {
                kind: Box::new(lume_mir::DeclarationKind::Call {
                    func_id: array_alloc_func.id,
                    name: array_alloc_func.name,
                    args: vec![
                        lume_mir::Operand::integer(64, false, args.len().cast_signed() as i64),
                        lume_mir::Operand::reference_of(metadata_reg),
                    ],
                }),
                location: vararg_loc,
            });

        for arg in args {
            self.call(
                array_push_func_id,
                vec![
                    lume_mir::Operand::reference_of(vararg_arr_reg),
                    arg,
                    lume_mir::Operand::reference_of(metadata_reg),
                ],
                vararg_loc,
            );
        }

        new_args.push(lume_mir::Operand::reference_of(vararg_arr_reg));
        new_args
    }
}

impl Builder<'_, '_> {
    /// Determines whether the given value needs to be boxed, given the
    /// expected type of the value.
    pub(crate) fn needs_boxing(&self, value: &lume_mir::Operand, expected_type: &TypeRef) -> bool {
        let expected_type = self.lower_type(expected_type);
        let value_type = self.type_of_value(value);

        expected_type.is_reference_type() && value_type.is_scalar_type()
    }

    /// Determines whether the given value needs to be unboxed, given the
    /// expected type of the value.
    pub(crate) fn needs_unboxing(&self, value: &lume_mir::Operand, expected_type: &TypeRef) -> bool {
        let value_type = self.type_of_value(value);

        expected_type.is_scalar_type() && value_type.is_reference_type()
    }

    /// Either references or boxes the given value, depending on the type of the
    /// given value. This method does not check whether the value needs to be
    /// boxed; to only box a value if it's necessary, use
    /// [`Self::box_value_if_needed`].
    ///
    /// Whether the value is allocated on the heap or stack depends on whether
    /// it escapes the current function.
    ///
    /// | Escapes | Result        |
    /// |---------|---------------|
    /// | No      | Boxed (stack) |
    /// | Yes     | Boxed (heap)  |
    pub(crate) fn box_value(&mut self, value: lume_mir::Operand, expected_type: &TypeRef) -> lume_mir::Operand {
        let location = value.location;

        #[allow(clippy::match_same_arms, reason = "explaitory comments differ")]
        let allocate_on_heap = match &value.kind {
            // Scalar types always get stored on the heap, since they are
            // declared inline (i.e. have no backing static memory address).
            lume_mir::OperandKind::Boolean { .. }
            | lume_mir::OperandKind::Integer { .. }
            | lume_mir::OperandKind::Float { .. } => true,

            // Strings are put on the stack, since they always have a backing
            // static memory address - either from an allocation or it's stored in the binary.
            lume_mir::OperandKind::String { .. } => false,

            // Slot operands already exist on the stack, so they don't need to be put on the heap.
            lume_mir::OperandKind::SlotAddress { .. } | lume_mir::OperandKind::LoadSlot { .. } => false,

            lume_mir::OperandKind::Bitcast { source: id, .. }
            | lume_mir::OperandKind::Load { id, .. }
            | lume_mir::OperandKind::LoadField { target: id, .. }
            | lume_mir::OperandKind::Reference { id } => self
                .mcx
                .does_register_escape(&self.func, self.func.current_block().id, *id)
                .escapes(),
        };

        if allocate_on_heap {
            self.store_on_heap(value, expected_type, location)
        } else {
            self.store_on_stack(value, location)
        }
    }

    /// Boxes the given operand value, if needed by the expected type. If the
    /// value does not need to be boxed, it returns the original value.
    ///
    /// This method checks whether the value should be boxed, as opposed to
    /// [`Self::box_value`].
    pub(crate) fn box_value_if_needed(
        &mut self,
        value: lume_mir::Operand,
        expected_type: &TypeRef,
    ) -> lume_mir::Operand {
        if self.needs_boxing(&value, expected_type) {
            self.box_value(value, expected_type)
        } else {
            value
        }
    }

    /// Unboxes the given operand value.
    ///
    /// This method does not check whether the value should be unboxed or not -
    /// it assumes that the value is a boxed value and should be unboxed.
    pub(crate) fn unbox_value(&mut self, value: lume_mir::Operand, expected_type: &TypeRef) -> lume_mir::Operand {
        let location = value.location;
        let return_type = self.lower_type(expected_type);

        let declared_operand = self.declare_operand(value, OperandRef::Implicit);
        let loaded_operand = self.load_as(return_type, declared_operand, location);

        self.use_register(loaded_operand, location)
    }

    /// Unboxes the given operand value, if needed by the expected type. If the
    /// value is not boxed or does not need to be unboxed, it returns the
    /// original value.
    ///
    /// This method checks whether the value should be unboxed, as opposed to
    /// [`Self::unbox_value`].
    pub(crate) fn unbox_value_if_needed(
        &mut self,
        value: lume_mir::Operand,
        expected_type: &TypeRef,
    ) -> lume_mir::Operand {
        if self.needs_unboxing(&value, expected_type) {
            self.unbox_value(value, expected_type)
        } else {
            value
        }
    }
}
