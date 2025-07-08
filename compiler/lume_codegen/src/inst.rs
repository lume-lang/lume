use crate::FunctionLower;

impl FunctionLower<'_, '_> {
    pub(super) fn instruction(&self, inst: &lume_mir::Instruction) {
        match inst {
            lume_mir::Instruction::Let { register, decl } => {
                let (ptr, _) = self.load_ptr(*register);
                let val = self.decl_value(decl);

                self.builder.store(ptr, val);
            }
            lume_mir::Instruction::StackAllocate { ty, .. } => {
                let ty = self.builder.ctx.lower_type(ty);

                self.builder.alloca(ty);
            }
            lume_mir::Instruction::HeapAllocate { ty, .. } => {
                let ty = self.builder.ctx.lower_type(ty);

                self.builder.malloc(ty);
            }
            lume_mir::Instruction::Store { target, value } => {
                let (ptr, _) = self.load_ptr(*target);
                let val = self.operand(value);

                self.builder.store(ptr, val);
            }
        }
    }
}
