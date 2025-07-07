use crate::FunctionLower;

impl FunctionLower<'_> {
    pub(super) fn instruction(&self, inst: &lume_mir::Instruction) {
        match inst {
            lume_mir::Instruction::Declare { register, decl } => {
                let (ptr, _) = self.load_ptr(*register);
                let val = self.decl_value(decl);

                self.builder.store(ptr, val);
            }
            lume_mir::Instruction::Store { target, value } => {
                let (ptr, _) = self.load_ptr(*target);
                let val = self.value(value);

                self.builder.store(ptr, val);
            }
        }
    }
}
