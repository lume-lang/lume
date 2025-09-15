use lume_span::DefId;

use crate::FunctionTransformer;

impl FunctionTransformer<'_, '_> {
    pub(super) fn lower_type(&self, type_ref: &lume_types::TypeRef) -> lume_mir::Type {
        let ty = self.tcx().db().type_(type_ref.instance_of).unwrap();

        match &ty.kind {
            lume_types::TypeKind::Void => lume_mir::Type::void(),
            lume_types::TypeKind::Bool => lume_mir::Type::boolean(),
            lume_types::TypeKind::Int(n) => lume_mir::Type::integer(*n, true),
            lume_types::TypeKind::UInt(n) => lume_mir::Type::integer(*n, false),
            lume_types::TypeKind::Float(n) => lume_mir::Type::float(*n),
            lume_types::TypeKind::String => lume_mir::Type::string(),
            lume_types::TypeKind::User(lume_types::UserType::Struct(def)) => {
                let name = format!("{:+}", def.name);
                let ty_props = self.tcx().db().find_fields(type_ref.instance_of);
                let props = ty_props.map(|p| self.lower_type(&p.field_type)).collect::<Vec<_>>();

                let struct_ty = lume_mir::Type::structure(name, props);

                lume_mir::Type::pointer(struct_ty)
            }
            lume_types::TypeKind::User(lume_types::UserType::Enum(_)) => {
                let enum_ty = lume_mir::Type::union(Vec::new());

                lume_mir::Type::pointer(enum_ty)
            }
            lume_types::TypeKind::User(lume_types::UserType::Trait(_)) => {
                lume_mir::Type::pointer(lume_mir::Type::void())
            }
            lume_types::TypeKind::TypeParameter(_) => lume_mir::Type::type_param(),
        }
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
            lume_mir::OperandKind::LoadField { target, index, .. } => {
                let reg_ty = self.func.registers.register_ty(*target).clone();

                if let lume_mir::TypeKind::Union { cases } = &reg_ty.kind {
                    let case = cases
                        .get(*index)
                        .expect("bug!: attempted to load union field out of bounds");

                    return case.clone();
                };

                let lume_mir::TypeKind::Pointer { elemental } = &reg_ty.kind else {
                    panic!("bug!: attempting to load non-pointer register");
                };

                if let lume_mir::TypeKind::Union { cases } = &elemental.kind {
                    let case = cases
                        .get(*index)
                        .expect("bug!: attempted to load union field out of bounds");

                    return case.clone();
                };

                let lume_mir::TypeKind::Struct { fields, .. } = &elemental.kind else {
                    panic!("bug!: attempting to load field from non-struct register");
                };

                fields[*index].clone()
            }
            lume_mir::OperandKind::SlotAddress { id } => {
                let slot_ty = self.func.slots.get(id).unwrap();

                lume_mir::Type::pointer(slot_ty.to_owned())
            }
            lume_mir::OperandKind::Reference { id } => self.func.registers.register_ty(*id).clone(),
        }
    }

    pub(super) fn type_of_decl(&self, decl: &lume_mir::Declaration) -> lume_mir::Type {
        match &decl.kind {
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
                | lume_mir::Intrinsic::BooleanOr => lume_mir::Type::boolean(),
                lume_mir::Intrinsic::IntAdd { bits, signed }
                | lume_mir::Intrinsic::IntSub { bits, signed }
                | lume_mir::Intrinsic::IntMul { bits, signed }
                | lume_mir::Intrinsic::IntDiv { bits, signed }
                | lume_mir::Intrinsic::IntAnd { bits, signed }
                | lume_mir::Intrinsic::IntOr { bits, signed }
                | lume_mir::Intrinsic::IntXor { bits, signed } => lume_mir::Type::integer(*bits, *signed),
                lume_mir::Intrinsic::FloatAdd { bits }
                | lume_mir::Intrinsic::FloatSub { bits }
                | lume_mir::Intrinsic::FloatMul { bits }
                | lume_mir::Intrinsic::FloatDiv { bits } => lume_mir::Type::float(*bits),
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

    pub(super) fn type_of_function(&self, func_id: DefId) -> lume_mir::Type {
        let func = self.function(func_id);

        func.signature.return_type.clone()
    }
}
