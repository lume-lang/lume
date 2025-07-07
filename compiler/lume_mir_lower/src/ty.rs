use crate::FunctionTransformer;

impl FunctionTransformer<'_> {
    pub(super) fn lower_type(&self, type_ref: &lume_types::TypeRef) -> lume_mir::Type {
        let ty = self.transformer.tcx.db().type_(type_ref.instance_of).unwrap();

        match ty.kind {
            lume_types::TypeKind::Void => lume_mir::Type::Void,
            lume_types::TypeKind::Bool => lume_mir::Type::Boolean,
            lume_types::TypeKind::Int(n) => lume_mir::Type::Integer { bits: n, signed: true },
            lume_types::TypeKind::UInt(n) => lume_mir::Type::Integer { bits: n, signed: false },
            lume_types::TypeKind::Float(n) => lume_mir::Type::Float { bits: n },
            lume_types::TypeKind::String => lume_mir::Type::String,
            _ => lume_mir::Type::Pointer,
        }
    }

    #[allow(clippy::unused_self)]
    pub(super) fn type_of_value(&self, value: &lume_mir::Value) -> lume_mir::Type {
        match value {
            lume_mir::Value::Boolean { .. } => lume_mir::Type::Boolean,
            lume_mir::Value::Integer { bits, signed, .. } => lume_mir::Type::Integer {
                bits: *bits,
                signed: *signed,
            },
            lume_mir::Value::Float { bits, .. } => lume_mir::Type::Float { bits: *bits },
            lume_mir::Value::String { .. } => lume_mir::Type::String,
            lume_mir::Value::Reference { id } => self.func.registers.register_ty(*id).clone(),
            lume_mir::Value::Load { .. } => lume_mir::Type::Pointer,
        }
    }

    pub(super) fn type_of_decl(&self, decl: &lume_mir::Declaration) -> lume_mir::Type {
        match decl {
            lume_mir::Declaration::Value(val) => self.type_of_value(val),
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
                | lume_mir::Intrinsic::BooleanNe => lume_mir::Type::Boolean,
                lume_mir::Intrinsic::IntAdd { bits, signed }
                | lume_mir::Intrinsic::IntSub { bits, signed }
                | lume_mir::Intrinsic::IntMul { bits, signed }
                | lume_mir::Intrinsic::IntDiv { bits, signed } => lume_mir::Type::Integer {
                    bits: *bits,
                    signed: *signed,
                },
                lume_mir::Intrinsic::FloatAdd { bits }
                | lume_mir::Intrinsic::FloatSub { bits }
                | lume_mir::Intrinsic::FloatMul { bits }
                | lume_mir::Intrinsic::FloatDiv { bits } => lume_mir::Type::Float { bits: *bits },
            },
            _ => todo!(),
        }
    }
}
