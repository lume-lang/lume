use super::*;

/// Some backend implementations, specifically Cranelift, does not allow using
/// the same register across block boundaries. Because of the *conservative*
/// nature of Cranelift optimization, we must do this preprocessing ourselves.
///
/// It will attempt to rename registers in an MIR function, so that each block
/// start it's register index at 0, then increments it when a new register is
/// declared. This also include block parameters.
///
/// As an example, take the given MIR:
/// ```mir
/// B0:
///     #0 = 4_i32
///     #1 = 1_i32
///     #2: i32 = -(#0, #1)
///     goto B1(#2)
/// B1(#2):
///     #3 = 7_i32
///     #4: i32 = +(#0, #2)
///     goto B2(#2, #4)
/// B2(#2, #4):
///     #5: i32 = *(#2, #4)
///     return #5
/// ```
///
/// This is not valid in Cranelift, since most registers are used across
/// multiple blocks. Look at `#0` specifically; it is referenced in all of the
/// blocks in the function!
///
/// To circumvent this ~pedantry~ *requirement*, this pass renames all the
/// registers to be local to the block in which they're declared. Using the
/// given MIR from before, we transform it into:
/// ```mir
/// B0:
///     #0 = 4_i32
///     #1 = 1_i32
///     #2: i32 = -(#0, #1)
///     goto B1(#2)
/// B1(#0):
///     #1 = 7_i32
///     #2: i32 = +(#0, #1)
///     goto B2(#2, #0)
/// B2(#0, #1):
///     #2: i32 = *(#0, #1)
///     return #2
/// ```
///
/// <div class="warning">
///
/// **Here be dragons!**
///
/// On a more serious note, this is quite possibly the most error-prone part of
/// the MIR lowering process. The reason for this is mostly because of the
/// awkward implementation of "phi" nodes between blocks in some instances.
///
/// This pass should receive a refactor at some point, but is currently not
/// planned.
///
/// </div>
#[derive(Default, Debug)]
pub(crate) struct RenameSsaVariables {
    register_counter: usize,
}

impl Pass for RenameSsaVariables {
    fn name() -> &'static str {
        "rename_ssa"
    }

    /// Creates a new instance of the pass without default settings.
    fn new() -> Self {
        Self::default()
    }

    /// Executes the pass on the given function.
    fn execute(&mut self, func: &mut Function) {
        let mut new_registers = func
            .registers
            .iter()
            .filter(|reg| reg.block.is_none())
            .cloned()
            .collect::<Vec<Register>>();

        let mut register_mapping = RegisterMapping::new(func.name.clone());

        for block in func.blocks.values_mut() {
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

            for (old, new) in &register_mapping.mapping {
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

            register_mapping.mapping.clear();
        }

        func.registers.replace_all(new_registers.into_iter());
    }
}

impl RenameSsaVariables {
    fn rename_register_index(
        &mut self,
        old: RegisterId,
        block: BasicBlockId,
        mapping: &mut RegisterMapping,
    ) -> RegisterId {
        let new = RegisterId::new(self.register_counter);

        mapping.insert(block, old, new);
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

        mapping.insert(block, *old, new);
        self.register_counter += 1;

        *old = new;

        new
    }

    fn update_regs_inst(&mut self, inst: &mut Instruction, block: BasicBlockId, mapping: &mut RegisterMapping) {
        match &mut inst.kind {
            InstructionKind::Let { register, decl, .. } => {
                self.rename_register_index_mut(register, block, mapping);

                Self::update_regs_decl(decl, block, mapping);
            }
            InstructionKind::Assign { .. } => unreachable!("bug!: assignments should be removed in previous SSA pass"),
            InstructionKind::Allocate { register, metadata, .. } => {
                self.rename_register_index_mut(register, block, mapping);
                *metadata = mapping.get(block, *metadata);
            }
            InstructionKind::Store { target, value } | InstructionKind::StoreField { target, value, .. } => {
                *target = mapping.get(block, *target);
                Self::update_regs_op(value, block, mapping);
            }
            InstructionKind::ObjectRegister { register } => {
                *register = mapping.get(block, *register);
            }
            InstructionKind::CreateSlot { .. } | InstructionKind::StoreSlot { .. } => {}
        }
    }

    fn update_regs_term(term: &mut Terminator, block: BasicBlockId, mapping: &mut RegisterMapping) {
        match &mut term.kind {
            TerminatorKind::Return(op) => {
                if let Some(value) = op {
                    Self::update_regs_op(value, block, mapping);
                }
            }
            TerminatorKind::Branch(site) => {
                for arg in &mut site.arguments {
                    Self::update_regs_op(arg, block, mapping);
                }
            }
            TerminatorKind::ConditionalBranch {
                condition,
                then_block,
                else_block,
            } => {
                *condition = mapping.get(block, *condition);

                for arg in &mut then_block.arguments {
                    Self::update_regs_op(arg, block, mapping);
                }

                for arg in &mut else_block.arguments {
                    Self::update_regs_op(arg, block, mapping);
                }
            }
            TerminatorKind::Switch {
                operand,
                arms,
                fallback,
            } => {
                *operand = mapping.get(block, *operand);

                for arm in arms {
                    for arg in &mut arm.1.arguments {
                        Self::update_regs_op(arg, block, mapping);
                    }
                }

                for arg in &mut fallback.arguments {
                    Self::update_regs_op(arg, block, mapping);
                }
            }
            TerminatorKind::Unreachable => {}
        }
    }

    fn update_regs_decl(decl: &mut Declaration, block: BasicBlockId, mapping: &mut RegisterMapping) {
        match decl.kind.as_mut() {
            DeclarationKind::Operand(op) => Self::update_regs_op(op, block, mapping),
            DeclarationKind::Cast { operand, .. } => {
                *operand = mapping.get(block, *operand);
            }
            DeclarationKind::Call { args, .. } | DeclarationKind::Intrinsic { args, .. } => {
                for arg in args {
                    Self::update_regs_op(arg, block, mapping);
                }
            }
            DeclarationKind::IndirectCall { ptr, args, .. } => {
                *ptr = mapping.get(block, *ptr);

                for arg in args {
                    Self::update_regs_op(arg, block, mapping);
                }
            }
        }
    }

    fn update_regs_op(op: &mut Operand, block: BasicBlockId, mapping: &mut RegisterMapping) {
        match &mut op.kind {
            OperandKind::Load { id } | OperandKind::Reference { id } => {
                *id = mapping.get(block, *id);
            }
            OperandKind::LoadField { target, .. } => {
                *target = mapping.get(block, *target);
            }
            OperandKind::Bitcast { source, .. } => {
                *source = mapping.get(block, *source);
            }
            OperandKind::Boolean { .. }
            | OperandKind::Integer { .. }
            | OperandKind::Float { .. }
            | OperandKind::String { .. }
            | OperandKind::LoadSlot { .. }
            | OperandKind::SlotAddress { .. } => {}
        }
    }
}

struct RegisterMapping {
    func: String,
    pub mapping: IndexMap<(RegisterId, BasicBlockId), RegisterId>,
}

impl RegisterMapping {
    pub fn new(func: String) -> Self {
        Self {
            func,
            mapping: IndexMap::new(),
        }
    }

    pub fn get(&mut self, block: BasicBlockId, id: RegisterId) -> RegisterId {
        *self
            .mapping
            .get(&(id, block))
            .unwrap_or_else(|| panic!("could not find register {id} in {block} in {}", self.func))
    }

    pub fn insert(&mut self, block: BasicBlockId, old: RegisterId, new: RegisterId) {
        self.mapping.insert((old, block), new);
    }
}
