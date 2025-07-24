use crate::llvm::FunctionLower;

impl FunctionLower<'_, '_> {
    pub(super) fn instruction(&self, inst: &lume_mir::Instruction) {
        match inst {
            lume_mir::Instruction::Let { register, decl } => {
                let (ptr, _) = self.retrieve_var_ptr(*register);
                let val = self.decl_value(decl);

                self.builder.store(ptr, val);
            }
            lume_mir::Instruction::Allocate { ty, .. } => {
                let is_ref_ty = ty.is_reference_type();
                let ty = self.builder.ctx.lower_type(ty);

                if is_ref_ty {
                    self.builder.malloc(ty);
                } else {
                    self.builder.alloca(ty);
                }
            }
            lume_mir::Instruction::Assign { target, value } | lume_mir::Instruction::Store { target, value } => {
                let (ptr, _) = self.retrieve_var_ptr(*target);
                let val = self.operand(value);

                self.builder.store(ptr, val);
            }
            lume_mir::Instruction::StoreField { target, offset, value } => {
                let (ptr, struct_ty) = self.retrieve_var_ptr(*target);
                let val = self.operand(value);

                self.builder
                    .store_field(struct_ty.into_struct_type(), ptr, val, *offset);
            }
        }
    }
}
