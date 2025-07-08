use crate::FunctionLower;

impl FunctionLower<'_, '_> {
    pub(super) fn terminator(&self, term: &lume_mir::Terminator) {
        match term {
            lume_mir::Terminator::Return(ret) => {
                if let Some(val) = &ret {
                    self.builder.return_value(&self.operand(val));
                } else {
                    self.builder.return_void();
                }
            }
            lume_mir::Terminator::Branch(id) => {
                self.builder.branch(self.builder.block(*id));
            }
            lume_mir::Terminator::ConditionalBranch {
                condition,
                then_block,
                else_block,
            } => {
                let (cond_ptr, cond_type) = self.retrieve_var_ptr(*condition);
                let condition = self.builder.load(cond_ptr, cond_type);

                let then_block = self.builder.block(*then_block);
                let else_block = self.builder.block(*else_block);

                self.builder
                    .conditional_branch(condition.into_int_value(), then_block, else_block);
            }
            lume_mir::Terminator::Unreachable => self.builder.unreachable(),
        }
    }
}
