use indexmap::{IndexMap, IndexSet};
use lume_mir::{
    BasicBlock, BasicBlockId, BlockBranchSite, Declaration, Function, Instruction, Operand, Register, RegisterId,
    Terminator,
};

use crate::FunctionTransformer;

impl FunctionTransformer<'_> {
    pub(crate) fn run_passes(&mut self) {
        DefineBlockParameters::default().execute(&mut self.func);
        PassBlockArguments::execute(&mut self.func);
        ConvertAssignmentExpressions::default().execute(&mut self.func);
        RenameSsaVariables::default().execute(&mut self.func);
    }
}

#[derive(Default, Debug)]
struct DefineBlockParameters {
    params: IndexMap<BasicBlockId, IndexSet<RegisterId>>,
}

impl DefineBlockParameters {
    pub fn execute(&mut self, func: &mut Function) {
        for block in &func.blocks {
            self.find_required_input_registers(func, block);
        }

        for block in &mut func.blocks {
            // Skip blocks which don't have any predecessors, since they cannot
            // have any input parameters. This is mostly for entry blocks, as they
            // get their parameters implictly from the function itself.
            if block.predecessors().count() == 0 {
                continue;
            }

            let Some(params) = self.params.get(&block.id) else {
                continue;
            };

            // TODO: is there maybe a better way to convert an `IndexSet` to a `Vec`?
            block.parameters = params.clone().into_iter().collect();
        }
    }

    fn find_required_input_registers(&mut self, func: &Function, block: &BasicBlock) {
        if self.params.contains_key(&block.id) {
            return;
        }

        // Prevent a stack overflow from self-referencing blocks, such as loops.
        self.params.insert(block.id, IndexSet::new());

        let mut regs = IndexSet::new();

        for successor in block.successors() {
            let successor_block = func.block(successor);
            self.find_required_input_registers(func, successor_block);

            if let Some(successor_params) = self.params.get(&successor) {
                for successor_param in successor_params {
                    if block.is_register_phi_dest(*successor_param) {
                        continue;
                    }

                    regs.insert(*successor_param);
                }
            }
        }

        // Find all referenced registers within the block from all the instructions
        // and the block terminator...
        for inst in block.instructions() {
            regs.extend(inst.register_refs());
        }

        if let Some(terminator) = block.terminator() {
            regs.extend(terminator.register_refs());
        }

        // ...and scoop away all registers which were referenced within the block, but were
        // also defined within the block.
        for inst in block.instructions() {
            if let Some(def) = inst.register_def() {
                regs.shift_remove(&def);
            }
        }

        self.params.insert(block.id, regs);
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
            let block_id = block.id;

            if let Some(terminator) = block.terminator_mut() {
                match terminator {
                    Terminator::Branch(target) => Self::update_branch_terminator(block_id, target, &func_immut),
                    Terminator::ConditionalBranch {
                        then_block, else_block, ..
                    } => {
                        Self::update_branch_terminator(block_id, then_block, &func_immut);
                        Self::update_branch_terminator(block_id, else_block, &func_immut);
                    }
                    Terminator::Return(_) | Terminator::Unreachable => {}
                }
            }
        }
    }

    fn update_branch_terminator(block: BasicBlockId, call_site: &mut BlockBranchSite, func: &Function) {
        let source_block = func.block(block);
        let target_block = func.block(call_site.block);

        call_site.arguments.reserve(target_block.parameters.len());

        for param in &target_block.parameters {
            let arg = if let Some(phi_source) = source_block.resolve_phi_source(*param) {
                phi_source
            } else {
                *param
            };

            if !call_site.arguments.contains(&arg) {
                call_site.arguments.push(arg);
            }
        }
    }
}

#[derive(Default, Debug)]
struct ConvertAssignmentExpressions {
    register_count: usize,
    moved_regs: IndexMap<RegisterId, RegisterId>,
}

impl ConvertAssignmentExpressions {
    pub fn execute(&mut self, func: &mut Function) {
        self.register_count = func.registers.iter().count();

        let mut new_registers = IndexSet::new();

        for block in &mut func.blocks {
            for inst in block.instructions_mut() {
                self.update_regs_inst(inst);
            }

            if let Some(term) = block.terminator_mut() {
                self.update_regs_term(term);
            }

            for (old, new) in &self.moved_regs {
                let old_reg = func.registers.register(*old);

                new_registers.insert(Register {
                    id: *new,
                    ty: old_reg.ty.clone(),
                    block: Some(block.id),
                });
            }

            self.moved_regs.clear();
        }

        for new_reg in new_registers {
            let id = func.registers.allocate(new_reg.ty, new_reg.block.unwrap());

            debug_assert_eq!(new_reg.id, id);
        }
    }

    fn move_register(&mut self, old: &mut RegisterId) -> RegisterId {
        let new = RegisterId::new(self.register_count);

        self.moved_regs.insert(*old, new);
        self.register_count += 1;

        *old = new;
        new
    }

    fn get_moved_register(&self, reg: &mut RegisterId) {
        if let Some(moved) = self.moved_regs.get(reg) {
            *reg = *moved;
        }
    }

    fn update_regs_inst(&mut self, inst: &mut Instruction) {
        match inst {
            Instruction::Let { register, decl } => {
                self.get_moved_register(register);
                self.update_regs_decl(decl);
            }
            Instruction::Assign { target, value } => {
                self.move_register(target);
                self.update_regs_op(value);

                // After the registers of the assignment instruction have been converted
                // to SSA registers, we transform the instruction itself to declare a new register
                // instead of attempting to assign a non-existent register.
                *inst = Instruction::Let {
                    register: *target,
                    decl: Declaration::Operand(value.clone()),
                };
            }
            Instruction::Allocate { register, .. } => {
                self.get_moved_register(register);
            }
            Instruction::Store { target, value } | Instruction::StoreField { target, value, .. } => {
                self.get_moved_register(target);
                self.update_regs_op(value);
            }
            Instruction::CreateSlot { .. } | Instruction::StoreSlot { .. } => {}
        }
    }

    fn update_regs_term(&mut self, term: &mut Terminator) {
        match term {
            Terminator::Return(op) => {
                if let Some(value) = op {
                    self.update_regs_op(value);
                }
            }
            Terminator::Branch(site) => {
                for arg in &mut site.arguments {
                    self.get_moved_register(arg);
                }
            }
            Terminator::ConditionalBranch {
                condition,
                then_block,
                else_block,
            } => {
                self.get_moved_register(condition);

                for arg in &mut then_block.arguments {
                    self.get_moved_register(arg);
                }

                for arg in &mut else_block.arguments {
                    self.get_moved_register(arg);
                }
            }
            Terminator::Unreachable => {}
        }
    }

    fn update_regs_decl(&mut self, decl: &mut Declaration) {
        match decl {
            Declaration::Operand(op) => self.update_regs_op(op),
            Declaration::Cast { operand, .. } => {
                self.get_moved_register(operand);
            }
            Declaration::Call { args, .. } | Declaration::Intrinsic { args, .. } => {
                for arg in args {
                    self.update_regs_op(arg);
                }
            }
        }
    }

    fn update_regs_op(&mut self, op: &mut Operand) {
        match op {
            Operand::Load { id } | Operand::Reference { id } => {
                self.get_moved_register(id);
            }
            Operand::LoadField { target, .. } => {
                self.get_moved_register(target);
            }
            Operand::Boolean { .. }
            | Operand::Integer { .. }
            | Operand::Float { .. }
            | Operand::String { .. }
            | Operand::SlotAddress { .. } => {}
        }
    }
}

type RegisterMapping = IndexMap<(RegisterId, BasicBlockId), RegisterId>;

#[derive(Default, Debug)]
struct RenameSsaVariables {
    register_counter: usize,
}

impl RenameSsaVariables {
    pub fn execute(&mut self, func: &mut Function) {
        let mut new_registers = func
            .registers
            .iter()
            .filter(|reg| reg.block.is_none())
            .cloned()
            .collect::<Vec<Register>>();

        let mut register_mapping = RegisterMapping::new();

        for block in &mut func.blocks {
            let block_id = block.id;

            for param_idx in 0..func.signature.parameters.len() {
                let param_reg = func.registers.register_mut(RegisterId::new(param_idx));

                self.rename_register_index(param_reg.id, block_id, &mut register_mapping);
            }

            for param in &mut block.parameters {
                self.rename_register_index_mut(param, block_id, &mut register_mapping);
            }

            for inst in block.instructions_mut() {
                self.update_regs_inst(inst, block_id, &mut register_mapping);
            }

            if let Some(term) = block.terminator_mut() {
                Self::update_regs_term(term, block_id, &mut register_mapping);
            }

            for (old, new) in &register_mapping {
                let old_reg = func.registers.register(old.0);

                if new_registers.len() <= new.as_usize() {
                    new_registers.resize_with(new.as_usize() + 1, Register::default);
                }

                new_registers[new.as_usize()] = lume_mir::Register {
                    id: *new,
                    ty: old_reg.ty.clone(),
                    block: Some(block.id),
                };
            }

            register_mapping.clear();
        }

        func.registers.replace_all(new_registers.into_iter());
    }

    fn rename_register_index(
        &mut self,
        old: RegisterId,
        block: BasicBlockId,
        mapping: &mut RegisterMapping,
    ) -> RegisterId {
        let new = RegisterId::new(self.register_counter);

        mapping.insert((old, block), new);
        self.register_counter += 1;

        new
    }

    fn rename_register_index_mut(
        &mut self,
        old: &mut RegisterId,
        block: BasicBlockId,
        mapping: &mut RegisterMapping,
    ) -> RegisterId {
        let new = RegisterId::new(self.register_counter);

        mapping.insert((*old, block), new);
        self.register_counter += 1;

        *old = new;

        new
    }

    fn update_regs_inst(&mut self, inst: &mut Instruction, block: BasicBlockId, mapping: &mut RegisterMapping) {
        match inst {
            Instruction::Let { register, decl } => {
                self.rename_register_index_mut(register, block, mapping);

                Self::update_regs_decl(decl, block, mapping);
            }
            Instruction::Assign { .. } => unreachable!("bug!: assignments should be removed in previous SSA pass"),
            Instruction::Allocate { register, .. } => {
                self.rename_register_index_mut(register, block, mapping);
            }
            Instruction::Store { target, value } | Instruction::StoreField { target, value, .. } => {
                *target = *mapping.get(&(*target, block)).unwrap();
                Self::update_regs_op(value, block, mapping);
            }
            Instruction::CreateSlot { .. } | Instruction::StoreSlot { .. } => {}
        }
    }

    fn update_regs_term(term: &mut Terminator, block: BasicBlockId, mapping: &mut RegisterMapping) {
        match term {
            Terminator::Return(op) => {
                if let Some(value) = op {
                    Self::update_regs_op(value, block, mapping);
                }
            }
            Terminator::Branch(site) => {
                for arg in &mut site.arguments {
                    *arg = *mapping.get(&(*arg, block)).unwrap();
                }
            }
            Terminator::ConditionalBranch {
                condition,
                then_block,
                else_block,
            } => {
                *condition = *mapping.get(&(*condition, block)).unwrap();

                for arg in &mut then_block.arguments {
                    *arg = *mapping.get(&(*arg, block)).unwrap();
                }

                for arg in &mut else_block.arguments {
                    *arg = *mapping.get(&(*arg, block)).unwrap();
                }
            }
            Terminator::Unreachable => {}
        }
    }

    fn update_regs_decl(decl: &mut Declaration, block: BasicBlockId, mapping: &mut RegisterMapping) {
        match decl {
            Declaration::Operand(op) => Self::update_regs_op(op, block, mapping),
            Declaration::Cast { operand, .. } => {
                *operand = *mapping.get(&(*operand, block)).unwrap();
            }
            Declaration::Call { args, .. } | Declaration::Intrinsic { args, .. } => {
                for arg in args {
                    Self::update_regs_op(arg, block, mapping);
                }
            }
        }
    }

    fn update_regs_op(op: &mut Operand, block: BasicBlockId, mapping: &mut RegisterMapping) {
        match op {
            Operand::Load { id } | Operand::Reference { id } => {
                *id = *mapping.get(&(*id, block)).unwrap();
            }
            Operand::LoadField { target, .. } => {
                *target = *mapping.get(&(*target, block)).unwrap();
            }
            Operand::Boolean { .. }
            | Operand::Integer { .. }
            | Operand::Float { .. }
            | Operand::String { .. }
            | Operand::SlotAddress { .. } => {}
        }
    }
}
