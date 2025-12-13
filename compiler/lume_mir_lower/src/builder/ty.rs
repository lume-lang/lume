use lume_span::NodeId;
use lume_types::TypeRef;

use crate::builder::Builder;
use crate::ty::lower_type;

impl Builder<'_, '_> {
    #[inline]
    pub(crate) fn lower_type(&self, type_ref: &TypeRef) -> lume_mir::Type {
        lower_type(self.mcx, type_ref)
    }

    pub(crate) fn type_of_value(&self, value: &lume_mir::Operand) -> lume_mir::Type {
        match &value.kind {
            lume_mir::OperandKind::Boolean { .. } => lume_mir::Type::boolean(),
            lume_mir::OperandKind::Integer { bits, signed, .. } => lume_mir::Type::integer(*bits, *signed),
            lume_mir::OperandKind::Float { bits, .. } => lume_mir::Type::float(*bits),
            lume_mir::OperandKind::String { .. } => lume_mir::Type::string(),
            lume_mir::OperandKind::Bitcast { target, .. } => target.to_owned(),
            lume_mir::OperandKind::Load { id } => {
                let register_type = self.func.registers.register_ty(*id).clone();
                let lume_mir::TypeKind::Pointer { elemental } = &register_type.kind else {
                    panic!("bug!: attempted to load non-pointer type: {id}, {register_type}");
                };

                elemental.as_ref().clone()
            }
            lume_mir::OperandKind::LoadField { field_type, .. } => field_type.clone(),
            lume_mir::OperandKind::LoadSlot { loaded_type, .. } => loaded_type.clone(),
            lume_mir::OperandKind::SlotAddress { id, .. } => {
                let slot_ty = self.func.slots.get(id).unwrap();

                lume_mir::Type::pointer(slot_ty.to_owned())
            }
            lume_mir::OperandKind::Reference { id } => self.func.registers.register_ty(*id).clone(),
        }
    }

    pub(crate) fn type_of_decl(&self, decl: &lume_mir::Declaration) -> lume_mir::Type {
        match decl.kind.as_ref() {
            lume_mir::DeclarationKind::Operand(val) => self.type_of_value(val),
            lume_mir::DeclarationKind::Intrinsic { name, args } => match name {
                lume_mir::Intrinsic::IntEq { .. }
                | lume_mir::Intrinsic::IntNe { .. }
                | lume_mir::Intrinsic::IntGt { .. }
                | lume_mir::Intrinsic::IntGe { .. }
                | lume_mir::Intrinsic::IntLt { .. }
                | lume_mir::Intrinsic::IntLe { .. }
                | lume_mir::Intrinsic::FloatEq { .. }
                | lume_mir::Intrinsic::FloatNe { .. }
                | lume_mir::Intrinsic::FloatGt { .. }
                | lume_mir::Intrinsic::FloatGe { .. }
                | lume_mir::Intrinsic::FloatLt { .. }
                | lume_mir::Intrinsic::FloatLe { .. }
                | lume_mir::Intrinsic::BooleanEq
                | lume_mir::Intrinsic::BooleanNe
                | lume_mir::Intrinsic::BooleanAnd
                | lume_mir::Intrinsic::BooleanOr
                | lume_mir::Intrinsic::BooleanNot => lume_mir::Type::boolean(),
                lume_mir::Intrinsic::IntAdd { bits, signed }
                | lume_mir::Intrinsic::IntSub { bits, signed }
                | lume_mir::Intrinsic::IntMul { bits, signed }
                | lume_mir::Intrinsic::IntDiv { bits, signed }
                | lume_mir::Intrinsic::IntAnd { bits, signed }
                | lume_mir::Intrinsic::IntOr { bits, signed }
                | lume_mir::Intrinsic::IntXor { bits, signed }
                | lume_mir::Intrinsic::IntNegate { bits, signed } => match args.first() {
                    Some(first_arg) => self.type_of_value(first_arg),
                    None => lume_mir::Type::integer(*bits, *signed),
                },
                lume_mir::Intrinsic::FloatAdd { bits }
                | lume_mir::Intrinsic::FloatSub { bits }
                | lume_mir::Intrinsic::FloatMul { bits }
                | lume_mir::Intrinsic::FloatDiv { bits }
                | lume_mir::Intrinsic::FloatNegate { bits } => lume_mir::Type::float(*bits),
                lume_mir::Intrinsic::Metadata { metadata } => lume_mir::Type {
                    kind: lume_mir::TypeKind::Metadata {
                        inner: metadata.clone(),
                    },
                    is_generic: false,
                },
            },
            lume_mir::DeclarationKind::Cast { operand, bits } => {
                let operand_ty = self.func.registers.register_ty(*operand);
                let signed = operand_ty.is_signed();

                lume_mir::Type::integer(*bits, signed)
            }
            lume_mir::DeclarationKind::Call { func_id, .. } => self.function_ret_type(*func_id),
            lume_mir::DeclarationKind::IndirectCall { signature, .. } => signature.return_type.clone(),
        }
    }

    /// Gets the MIR signature of the function with the given ID.
    pub(crate) fn signature_of(&self, func_id: NodeId) -> lume_mir::Signature {
        self.function(func_id).signature.clone()
    }

    /// Gets the MIR return type of the function with the given ID.
    pub(crate) fn function_ret_type(&self, func_id: NodeId) -> lume_mir::Type {
        self.signature_of(func_id).return_type.clone()
    }

    /// Creates a MIR union type from the given enum type.
    pub(crate) fn union_of(&self, enum_type: &TypeRef) -> lume_mir::Type {
        let enum_def = self.tcx().enum_definition(enum_type.instance_of).unwrap();
        let enum_def_id = enum_def.id;

        let mut union_cases = Vec::new();

        for variant in &enum_def.cases {
            let mut items = Vec::new();

            let params = &variant.parameters;
            let case_refs = self.tcx().mk_type_refs_from(params, enum_def_id).unwrap();

            union_cases.reserve_exact(case_refs.len());
            for case_ref in case_refs {
                items.push(self.lower_type(&case_ref));
            }

            union_cases.push(lume_mir::Type::tuple(items));
        }

        lume_mir::Type::union(union_cases)
    }
}
