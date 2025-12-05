use lume_mir_queries::MirQueryCtx;
use lume_span::NodeId;
use lume_types::TypeRef;

use crate::FunctionTransformer;

pub(crate) fn lower_type(mcx: &MirQueryCtx, type_ref: &TypeRef) -> lume_mir::Type {
    if mcx.tcx().is_type_never(type_ref) {
        return lume_mir::Type::never();
    }

    let ty = mcx.tcx().db().type_(type_ref.instance_of).unwrap();

    match &ty.kind {
        lume_types::TypeKind::Void => lume_mir::Type::void(),
        lume_types::TypeKind::Bool => lume_mir::Type::boolean(),
        lume_types::TypeKind::Int(n) => lume_mir::Type::integer(*n, true),
        lume_types::TypeKind::UInt(n) => lume_mir::Type::integer(*n, false),
        lume_types::TypeKind::Float(n) => lume_mir::Type::float(*n),
        lume_types::TypeKind::String => lume_mir::Type::string(),
        lume_types::TypeKind::Struct => {
            if mcx.tcx().is_std_pointer(type_ref) {
                let elemental_type = lower_type(mcx, &type_ref.bound_types[0]);

                return lume_mir::Type::pointer(elemental_type);
            }

            let struct_def = mcx.tcx().hir_expect_struct(type_ref.instance_of);
            let name = format!("{:+}", struct_def.name);

            let ty_props = mcx.tcx().fields_on(type_ref.instance_of).unwrap();
            let props = ty_props
                .iter()
                .map(|prop| {
                    let field_type = mcx
                        .tcx()
                        .mk_type_ref_from(&prop.field_type, type_ref.instance_of)
                        .unwrap();

                    lower_type(mcx, &field_type)
                })
                .collect::<Vec<_>>();

            let struct_ty = lume_mir::Type::structure(name, props);

            lume_mir::Type::pointer(struct_ty)
        }
        lume_types::TypeKind::Enum => {
            let enum_ty = lume_mir::Type::union(Vec::new());

            lume_mir::Type::pointer(enum_ty)
        }
        lume_types::TypeKind::Trait => lume_mir::Type::pointer(lume_mir::Type::void()),
        lume_types::TypeKind::TypeParameter => lume_mir::Type::type_param(),
    }
}

impl FunctionTransformer<'_, '_> {
    #[inline]
    pub(crate) fn lower_type(&self, type_ref: &TypeRef) -> lume_mir::Type {
        lower_type(&self.mcx, type_ref)
    }

    #[allow(clippy::unused_self)]
    pub(super) fn type_of_value(&self, value: &lume_mir::Operand) -> lume_mir::Type {
        match &value.kind {
            lume_mir::OperandKind::Boolean { .. } => lume_mir::Type::boolean(),
            lume_mir::OperandKind::Integer { bits, signed, .. } => lume_mir::Type::integer(*bits, *signed),
            lume_mir::OperandKind::Float { bits, .. } => lume_mir::Type::float(*bits),
            lume_mir::OperandKind::String { .. } => lume_mir::Type::string(),
            lume_mir::OperandKind::Bitcast { target, .. } => target.to_owned(),
            lume_mir::OperandKind::Load { id } => {
                let elemental = self.func.registers.register_ty(*id).clone();

                lume_mir::Type::pointer(elemental)
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

    pub(super) fn type_of_decl(&self, decl: &lume_mir::Declaration) -> lume_mir::Type {
        match decl.kind.as_ref() {
            lume_mir::DeclarationKind::Operand(val) => self.type_of_value(val),
            lume_mir::DeclarationKind::Intrinsic { name, .. } => match name {
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
                | lume_mir::Intrinsic::IntNegate { bits, signed } => lume_mir::Type::integer(*bits, *signed),
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
            lume_mir::DeclarationKind::Call { func_id, .. } => self.type_of_function(*func_id),
            lume_mir::DeclarationKind::IndirectCall { signature, .. } => signature.return_type.clone(),
        }
    }

    pub(super) fn type_of_function(&self, func_id: NodeId) -> lume_mir::Type {
        let func = self.function(func_id);

        func.signature.return_type.clone()
    }
}
