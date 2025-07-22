use std::collections::HashMap;

use lume_mir::{BasicBlock, BlockBranchSite, Declaration, Function, Instruction, Operand, RegisterId, Terminator};

use crate::FunctionTransformer;

impl FunctionTransformer<'_> {
    pub(crate) fn run_passes(&mut self) {
        DefineBlockParameters::execute(&mut self.func);
        PassBlockArguments::execute(&mut self.func);
        RenameSsaVariables::default().execute(&mut self.func);
    }
}

#[derive(Default, Debug)]
struct DefineBlockParameters;

impl DefineBlockParameters {
    pub fn execute(func: &mut Function) {
        for block in &mut func.blocks {
            // Skip blocks which don't have any predecessors, since they cannot
            // have any input parameters.
            if block.predecessors().count() == 0 {
                continue;
            }

            block.parameters = Self::find_required_input_registers(block);
        }
    }

    fn find_required_input_registers(block: &BasicBlock) -> Vec<RegisterId> {
        let mut regs = Vec::new();

        for inst in block.instructions() {
            regs.extend(inst.register_refs());
        }

        if let Some(terminator) = block.terminator() {
            regs.extend(terminator.register_refs());
        }

        regs
    }
}

#[derive(Default, Debug)]
struct PassBlockArguments;

impl PassBlockArguments {
    pub fn execute(func: &mut Function) {
        // TODO:
        // I hate this, but we're not quite at the "we should optimize the compiler more"
        // phase, yet. But, this is certainly not a good solution to a lifetime issue.
        let func_immut = func.clone();

        for block in &mut func.blocks {
            if let Some(terminator) = block.terminator_mut() {
                match terminator {
                    Terminator::Branch(target) => Self::update_branch_terminator(target, &func_immut),
                    Terminator::ConditionalBranch {
                        then_block, else_block, ..
                    } => {
                        Self::update_branch_terminator(then_block, &func_immut);
                        Self::update_branch_terminator(else_block, &func_immut);
                    }
                    Terminator::Return(_) | Terminator::Unreachable => {}
                }
            }
        }
    }

    fn update_branch_terminator(call_site: &mut BlockBranchSite, func: &Function) {
        let target_block = func.block(call_site.block);

        target_block.parameters.clone_into(&mut call_site.arguments);
    }
}

#[derive(Default, Debug)]
struct RenameSsaVariables {
    register_counter: usize,
}

impl RenameSsaVariables {
    pub fn execute(&mut self, func: &mut Function) {
        let mut register_mapping = HashMap::new();

        for block in &mut func.blocks {
            for param_idx in 0..func.signature.parameters.len() {
                let param_reg = func.registers.register_mut(RegisterId::new(param_idx));

                self.rename_register_index(&mut param_reg.id, &mut register_mapping);
            }

            for param in &mut block.parameters {
                self.rename_register_index(param, &mut register_mapping);
            }

            for inst in block.instructions_mut() {
                self.update_regs_inst(inst, &mut register_mapping);
            }

            if let Some(term) = block.terminator_mut() {
                Self::update_regs_term(term, &mut register_mapping);
            }

            register_mapping.clear();
        }
    }

    fn rename_register_index(
        &mut self,
        old: &mut RegisterId,
        mapping: &mut HashMap<RegisterId, RegisterId>,
    ) -> RegisterId {
        let new = RegisterId::new(self.register_counter);

        mapping.insert(*old, new);
        self.register_counter += 1;

        *old = new;

        new
    }

    fn update_regs_inst(&mut self, inst: &mut Instruction, mapping: &mut HashMap<RegisterId, RegisterId>) {
        match inst {
            Instruction::Let { register, decl } => {
                self.rename_register_index(register, mapping);

                Self::update_regs_decl(decl, mapping);
            }
            Instruction::StackAllocate { register, .. } | Instruction::HeapAllocate { register, .. } => {
                self.rename_register_index(register, mapping);
            }
            Instruction::Store { target, value } | Instruction::StoreField { target, value, .. } => {
                *target = *mapping.get(target).unwrap();
                Self::update_regs_op(value, mapping);
            }
        }
    }

    fn update_regs_term(term: &mut Terminator, mapping: &mut HashMap<RegisterId, RegisterId>) {
        match term {
            Terminator::Return(op) => {
                if let Some(value) = op {
                    Self::update_regs_op(value, mapping);
                }
            }
            Terminator::Branch(site) => {
                for arg in &mut site.arguments {
                    *arg = *mapping.get(arg).unwrap();
                }
            }
            Terminator::ConditionalBranch {
                then_block, else_block, ..
            } => {
                for arg in &mut then_block.arguments {
                    *arg = *mapping.get(arg).unwrap();
                }

                for arg in &mut else_block.arguments {
                    *arg = *mapping.get(arg).unwrap();
                }
            }
            Terminator::Unreachable => {}
        }
    }

    fn update_regs_decl(decl: &mut Declaration, mapping: &mut HashMap<RegisterId, RegisterId>) {
        match decl {
            Declaration::Operand(op) => Self::update_regs_op(op, mapping),
            Declaration::Cast { operand, .. } => {
                *operand = *mapping.get(operand).unwrap();
            }
            Declaration::Call { args, .. } | Declaration::Intrinsic { args, .. } => {
                for arg in args {
                    Self::update_regs_op(arg, mapping);
                }
            }
            Declaration::Reference { id } | Declaration::Load { id, .. } => {
                *id = *mapping.get(id).unwrap();
            }
        }
    }

    fn update_regs_op(op: &mut Operand, mapping: &mut HashMap<RegisterId, RegisterId>) {
        match op {
            Operand::Load { id } | Operand::Reference { id } => {
                *id = *mapping.get(id).unwrap();
            }
            Operand::LoadField { target, .. } => {
                *target = *mapping.get(target).unwrap();
            }
            Operand::Boolean { .. } | Operand::Integer { .. } | Operand::Float { .. } | Operand::String { .. } => {}
        }
    }
}
