use super::*;

/// Attempts to mark managed objects as GC references, by adding
/// [`InstructionKind::ObjectRegister`] instructions to the MIR, wherever
/// fitting. These instructions are used when the garbage collector
/// looks for live objects in any given location.
#[derive(Default, Debug)]
pub(crate) struct MarkObjectReferences {
    /// Current offset into the current block where the instruction
    /// should be placed.
    offset: usize,

    /// Defines all the currently marked objects, to prevent multiple
    /// instructions from being inserted, all referencing the same register.
    marked: HashSet<RegisterId>,

    /// List of all instructions which require an object register instruction.
    ///
    /// The tuple in each list entry is:
    ///  - the block in which the instruction was found,
    ///  - the offset of where to place the instruction
    ///  - and the register which needs to marked as an object.
    reference_inst: Vec<(BasicBlockId, usize, RegisterId)>,
}

impl Pass for MarkObjectReferences {
    fn name() -> &'static str {
        "mark_gc_refs"
    }

    /// Creates a new instance of the pass without default settings.
    fn new() -> Self {
        Self::default()
    }

    /// Executes the pass on the given function.
    fn execute(&mut self, func: &mut Function) {
        self.find_object_references(func);

        while let Some((block, offset, register)) = self.reference_inst.pop() {
            let block = func.block_mut(block);
            let inst = lume_mir::Instruction {
                kind: lume_mir::InstructionKind::ObjectRegister { register },
                location: Location::empty(),
            };

            // Even though it's valid for the offset to be the same length
            // as the instruction list, `insert` will panic if that's the case.
            if block.instructions.len() + 1 == offset {
                block.instructions.push(inst);
            } else {
                block.instructions.insert(offset, inst);
            }
        }
    }
}

impl MarkObjectReferences {
    fn find_object_references(&mut self, func: &Function) {
        for block in func.blocks.values() {
            for param in &block.parameters {
                if self.register_gc_object(func, block.id, *param) {
                    self.offset += 1;
                }
            }

            for inst in block.instructions() {
                match &inst.kind {
                    InstructionKind::Let { register, decl, .. } => {
                        match &decl.kind {
                            DeclarationKind::Operand(op) => self.find_object_references_operand(func, block.id, op),
                            DeclarationKind::Call { args, .. } | DeclarationKind::IndirectCall { args, .. } => {
                                for arg in args {
                                    self.find_object_references_operand(func, block.id, arg);
                                }
                            }
                            DeclarationKind::Cast { .. } | DeclarationKind::Intrinsic { .. } => {}
                        }

                        self.offset += 1;
                        self.register_gc_object(func, block.id, *register);
                    }
                    InstructionKind::Allocate { register, .. } => {
                        self.offset += 1;
                        self.register_gc_object(func, block.id, *register);
                    }
                    InstructionKind::Store { target, value } | InstructionKind::StoreField { target, value, .. } => {
                        self.offset += 1;
                        self.register_gc_object(func, block.id, *target);

                        self.find_object_references_operand(func, block.id, value);
                    }
                    InstructionKind::ObjectRegister { .. }
                    | InstructionKind::Assign { .. }
                    | InstructionKind::CreateSlot { .. }
                    | InstructionKind::StoreSlot { .. } => {
                        self.offset += 1;
                    }
                }
            }

            self.marked.clear();
            self.offset = 0;
        }
    }

    fn find_object_references_operand(&mut self, func: &Function, block: BasicBlockId, op: &Operand) {
        match &op.kind {
            OperandKind::Load { id } | OperandKind::Reference { id } => {
                if self.register_gc_object(func, block, *id) {
                    self.offset += 1;
                }
            }
            OperandKind::LoadField { target, .. } => {
                if self.register_gc_object(func, block, *target) {
                    self.offset += 1;
                }
            }
            OperandKind::Bitcast { .. }
            | OperandKind::Boolean { .. }
            | OperandKind::Integer { .. }
            | OperandKind::Float { .. }
            | OperandKind::String { .. }
            | OperandKind::LoadSlot { .. }
            | OperandKind::SlotAddress { .. } => {}
        }
    }

    fn register_gc_object(&mut self, func: &Function, block: BasicBlockId, register: RegisterId) -> bool {
        if self.marked.contains(&register) || !func.registers.register_ty(register).requires_stack_map() {
            return false;
        }

        self.marked.insert(register);
        self.reference_inst.push((block, self.offset, register));

        true
    }
}
