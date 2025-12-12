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
        let alloc_ptr = self.func.add_register(ty.clone());
        let metadata_reg = self.declare_metadata_of(type_ref, location);

        self.func
            .current_block_mut()
            .allocate(alloc_ptr, ty, metadata_reg, location);

        alloc_ptr
    }

    /// Declares a new register with the value of loading the given register
    /// from it's contained memory address.
    pub(crate) fn load(&mut self, ty: Type, source: RegisterId) -> RegisterId {
        self.declare_operand_as(
            ty,
            Operand {
                kind: OperandKind::Load { id: source },
                location: Location::empty(),
            },
            OperandRef::Implicit,
        )
    }

    /// Declares a new register with the value of loading the given register
    /// from it's contained memory address plus the given field offset.
    pub(crate) fn load_field(&mut self, target: RegisterId, offset: usize, field_type: Type) -> RegisterId {
        self.declare_operand_as(
            field_type.clone(),
            Operand {
                kind: OperandKind::LoadField {
                    target,
                    offset,
                    field_type,
                },
                location: Location::empty(),
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

    /// Declares a new register with the value of the given immediate integer
    /// value.
    pub(crate) fn iconst(&mut self, imm: impl Into<i64>, ty: Type) -> RegisterId {
        let bits = u8::try_from(ty.bytesize()).unwrap() * 8;
        let signed = ty.is_signed();

        self.declare_operand_as(
            ty,
            Operand {
                kind: OperandKind::Integer {
                    bits,
                    signed,
                    value: imm.into(),
                },
                location: Location::empty(),
            },
            OperandRef::Implicit,
        )
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
            // If the parameter isn't generic, we let it be.
            if !param.ty.is_generic && !param.ty.kind.is_reference_type() {
                continue;
            }

            // If the passed type is already a reference type, we can pass it without
            // allocating room for it.
            if self.type_of_value(arg).kind.is_reference_type() {
                continue;
            }

            let arg_ty = self.type_of_value(arg);
            let slot = self.func.alloc_slot(arg_ty, arg.location);
            self.func
                .current_block_mut()
                .store_slot(slot, 0, arg.clone(), arg.location);

            *arg = lume_mir::Operand {
                kind: lume_mir::OperandKind::SlotAddress { id: slot, offset: 0 },
                location: arg.location,
            };
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

        let array_alloc_func_id = self.tcx().lang_item("array_with_capacity").unwrap();
        let array_alloc_func = self.mcx.function(array_alloc_func_id).unwrap();

        let array_push_func_id = self.tcx().lang_item("array_push").unwrap();

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
