use super::*;

/// This pass converts any found direct assignment expressions, turns them into
/// a new declaration and uses that declaration for any following register references.
/// This is done because SSA, by design, does not support altering existing register values
/// directly.
///
/// For example, MIR like the following:
/// ```mir
///     let #0 = 4_i32
///     call foo(#0)
///     #0 = 6_i32     <-- invalid! SSA forbids reassigning existing registers
///     return #0
/// ```
///
/// is invalid SSA, since `#0` has now been reassigned. This pass attempts to convert the
/// assignment instruction into the following MIR:
/// ```mir
///     let #0 = 4_i32
///     call foo(#0)
///     let #1 = 6_i32
///     return #1
/// ```
#[derive(Default, Debug)]
pub(crate) struct ConvertAssignmentExpressions {
    register_count: usize,
    moved_regs: Vec<(RegisterId, RegisterId)>,
    registers: Registers,
}

impl Pass for ConvertAssignmentExpressions {
    fn name() -> &'static str {
        "ssa_assign"
    }

    /// Creates a new instance of the pass without default settings.
    fn new() -> Self {
        Self::default()
    }

    /// Executes the pass on the given function.
    fn execute(&mut self, func: &mut Function) {
        self.registers = func.registers.clone();
        self.register_count = self.registers.next_id().as_usize();

        let mut new_registers = IndexSet::new();

        for block in func.blocks.values_mut() {
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
}

impl ConvertAssignmentExpressions {
    fn move_register(&mut self, old: &mut RegisterId) -> RegisterId {
        let new = RegisterId::new(self.register_count);

        self.moved_regs.push((*old, new));
        self.register_count += 1;

        *old = new;
        new
    }

    fn get_moved_register(&self, reg: &mut RegisterId) {
        if let Some(moved) = self.moved_regs.iter().rfind(|r| r.0 == *reg) {
            *reg = moved.1;
        }
    }

    fn update_regs_inst(&mut self, inst: &mut Instruction) {
        match &mut inst.kind {
            InstructionKind::Let { register, decl, .. } => {
                self.get_moved_register(register);
                self.update_regs_decl(decl);
            }
            InstructionKind::Assign { target, value } => {
                let ty = self.registers.register_ty(*target).to_owned();

                self.update_regs_op(value);
                self.move_register(target);

                // After the registers of the assignment instruction have been converted
                // to SSA registers, we transform the instruction itself to declare a new register
                // instead of attempting to assign a non-existent register.
                inst.kind = InstructionKind::Let {
                    register: *target,
                    decl: Declaration {
                        kind: DeclarationKind::Operand(value.clone()),
                        location: inst.location,
                    },
                    ty,
                };
            }
            InstructionKind::Allocate { register, metadata, .. } => {
                self.get_moved_register(register);
                self.get_moved_register(metadata);
            }
            InstructionKind::Store { target, value } | InstructionKind::StoreField { target, value, .. } => {
                self.get_moved_register(target);
                self.update_regs_op(value);
            }
            InstructionKind::ObjectRegister { register } => {
                self.get_moved_register(register);
            }
            InstructionKind::CreateSlot { .. } | InstructionKind::StoreSlot { .. } => {}
        }
    }

    fn update_regs_term(&mut self, term: &mut Terminator) {
        match &mut term.kind {
            TerminatorKind::Return(op) => {
                if let Some(value) = op {
                    self.update_regs_op(value);
                }
            }
            TerminatorKind::Branch(site) => {
                for arg in &mut site.arguments {
                    self.get_moved_register(arg);
                }
            }
            TerminatorKind::ConditionalBranch {
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
            TerminatorKind::Switch {
                operand,
                arms,
                fallback,
            } => {
                self.get_moved_register(operand);

                for arm in arms {
                    for arg in &mut arm.1.arguments {
                        self.get_moved_register(arg);
                    }
                }

                for arg in &mut fallback.arguments {
                    self.get_moved_register(arg);
                }
            }
            TerminatorKind::Unreachable => {}
        }
    }

    fn update_regs_decl(&mut self, decl: &mut Declaration) {
        match &mut decl.kind {
            DeclarationKind::Operand(op) => self.update_regs_op(op),
            DeclarationKind::Cast { operand, .. } => {
                self.get_moved_register(operand);
            }
            DeclarationKind::Call { args, .. } | DeclarationKind::Intrinsic { args, .. } => {
                for arg in args {
                    self.update_regs_op(arg);
                }
            }
            DeclarationKind::IndirectCall { ptr, args, .. } => {
                self.get_moved_register(ptr);

                for arg in args {
                    self.update_regs_op(arg);
                }
            }
        }
    }

    fn update_regs_op(&mut self, op: &mut Operand) {
        match &mut op.kind {
            OperandKind::Load { id } | OperandKind::Reference { id } => {
                self.get_moved_register(id);
            }
            OperandKind::LoadField { target, .. } => {
                self.get_moved_register(target);
            }
            OperandKind::Bitcast { source, .. } => {
                self.get_moved_register(source);
            }
            OperandKind::Boolean { .. }
            | OperandKind::Integer { .. }
            | OperandKind::Float { .. }
            | OperandKind::String { .. }
            | OperandKind::SlotAddress { .. } => {}
        }
    }
}
