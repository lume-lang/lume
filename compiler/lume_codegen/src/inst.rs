use crate::FunctionLower;

impl FunctionLower<'_> {
    pub(super) fn instruction(&self, inst: &lume_mir::Instruction) {
        match inst {
            lume_mir::Instruction::Declare { register, decl } => {
                let ptr = *self.variables.get(register).unwrap();
                let val = self.decl_value(decl);

                self.builder.store(ptr, val);
            }
            lume_mir::Instruction::Store { target, value } => {
                let ptr = *self.variables.get(target).unwrap();
                let val = self.value(value);

                self.builder.store(ptr, val);
            }
        }
    }
}
