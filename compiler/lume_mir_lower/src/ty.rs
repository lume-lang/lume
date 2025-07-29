use crate::FunctionTransformer;

impl FunctionTransformer<'_> {
    pub(super) fn lower_type(&self, type_ref: &lume_types::TypeRef) -> lume_mir::Type {
        let ty = self.tcx().db().type_(type_ref.instance_of).unwrap();

        match &ty.kind {
            lume_types::TypeKind::Void => lume_mir::Type::void(),
            lume_types::TypeKind::Bool => lume_mir::Type::boolean(),
            lume_types::TypeKind::Int(n) => lume_mir::Type::integer(*n, true),
            lume_types::TypeKind::UInt(n) => lume_mir::Type::integer(*n, false),
            lume_types::TypeKind::Float(n) => lume_mir::Type::float(*n),
            lume_types::TypeKind::String => lume_mir::Type::string(),
            lume_types::TypeKind::User(lume_types::UserType::Struct(_)) => {
                let ty_props = self.tcx().db().find_properties(type_ref.instance_of);
                let props = ty_props.map(|p| self.lower_type(&p.property_type)).collect::<Vec<_>>();

                let struct_ty = lume_mir::Type::structure(type_ref.clone(), props);

                lume_mir::Type::pointer(struct_ty)
            }
            lume_types::TypeKind::User(lume_types::UserType::Trait(_) | lume_types::UserType::Enum(_))
            | lume_types::TypeKind::TypeParameter(_) => lume_mir::Type::pointer(lume_mir::Type::void()),
        }
    }

    #[allow(clippy::unused_self)]
    pub(super) fn type_of_value(&self, value: &lume_mir::Operand) -> lume_mir::Type {
        match value {
            lume_mir::Operand::Boolean { .. } => lume_mir::Type::boolean(),
            lume_mir::Operand::Integer { bits, signed, .. } => lume_mir::Type::integer(*bits, *signed),
            lume_mir::Operand::Float { bits, .. } => lume_mir::Type::float(*bits),
            lume_mir::Operand::String { .. } => lume_mir::Type::string(),
            lume_mir::Operand::Load { id } => {
                let elemental = self.func.registers.register_ty(*id).clone();

                lume_mir::Type::pointer(elemental)
            }
            lume_mir::Operand::LoadField { target, index, .. } => {
                let reg_ty = self.func.registers.register_ty(*target).clone();
                let lume_mir::TypeKind::Pointer { elemental } = &reg_ty.kind else {
                    panic!("bug!: attempting to load non-pointer register");
                };

                let lume_mir::TypeKind::Struct { properties } = &elemental.kind else {
                    panic!("bug!: attempting to load field from non-struct register");
                };

                properties[*index].clone()
            }
            lume_mir::Operand::Reference { id } => self.func.registers.register_ty(*id).clone(),
        }
    }

    pub(super) fn type_of_decl(&self, decl: &lume_mir::Declaration) -> lume_mir::Type {
        match decl {
            lume_mir::Declaration::Operand(val) => self.type_of_value(val),
            lume_mir::Declaration::Intrinsic { name, .. } => match name {
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
                lume_mir::Intrinsic::Metadata { metadata } => {
                    let id = self.tcx().std_type();

                    lume_mir::Type {
                        id,
                        kind: lume_mir::TypeKind::Metadata {
                            inner: metadata.clone(),
                        },
                    }
                }
            },
            lume_mir::Declaration::Cast { .. } => todo!(),
            lume_mir::Declaration::Call { func_id, .. } => self.type_of_function(*func_id),
        }
    }

    pub(super) fn type_of_function(&self, func_id: lume_mir::FunctionId) -> lume_mir::Type {
        let func = self.function(func_id);

        func.signature.return_type.clone()
    }
}
