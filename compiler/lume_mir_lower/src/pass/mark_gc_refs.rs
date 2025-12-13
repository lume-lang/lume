use std::collections::HashSet;

use lume_span::Location;

use super::*;

#[derive(Debug, Clone)]
struct ObjectReference {
    /// The block in which the instruction was found.
    pub block: BasicBlockId,

    /// The ID of the target instruction, which referenced the object.
    pub target: InstructionId,

    /// The placement of the GC reference instruction.
    pub placement: Placement,

    /// The register which needs to marked as an object.
    pub register: RegisterId,

    /// The location to use for the GC reference instruction.
    pub location: Location,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Placement {
    /// The reference should be placed before the target instruction.
    Before,

    /// The reference should be placed after the target instruction.
    After,
}

/// Attempts to mark managed objects as GC references, by adding
/// [`InstructionKind::ObjectRegister`] instructions to the MIR, wherever
/// fitting. These instructions are used when the garbage collector
/// looks for live objects in any given location.
#[derive(Default, Debug)]
pub(crate) struct MarkObjectReferences {
    /// Defines all the currently marked objects, to prevent multiple
    /// instructions from being inserted, all referencing the same register.
    marked: HashSet<RegisterId>,

    /// List of all places which require an object register instruction.
    references: Vec<ObjectReference>,
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
    fn execute(&mut self, _mcx: &MirQueryCtx, func: &mut Function) {
        self.find_object_references(func);

        while let Some(reference) = self.references.pop() {
            let block = func.block_mut(reference.block);

            let inst = lume_mir::Instruction {
                kind: lume_mir::InstructionKind::ObjectRegister {
                    register: reference.register,
                },
                location: reference.location,
            };

            let key = InstructionId(block.instructions.len());
            let offset = block.instructions.get_index_of(&reference.target).unwrap_or(0);

            match reference.placement {
                Placement::Before => {
                    block.instructions.insert_before(offset, key, inst);
                }
                Placement::After => {
                    if offset == block.instructions.len() {
                        block.instructions.insert(key, inst);
                    } else {
                        block.instructions.insert_before(offset + 1, key, inst);
                    }
                }
            }
        }
    }
}

impl MarkObjectReferences {
    fn find_object_references(&mut self, func: &Function) {
        for block in func.blocks.values() {
            for param in block.parameters() {
                self.register_gc_object(func, block.id, InstructionId(0), param, Placement::After, func.location);
            }

            for (&inst_id, inst) in &block.instructions {
                let location = inst.location;

                match &inst.kind {
                    InstructionKind::Let { register, decl, .. } => {
                        match decl.kind.as_ref() {
                            DeclarationKind::Operand(op) => self.find_object_references_operand(
                                func,
                                block.id,
                                inst_id,
                                op,
                                Placement::Before,
                                location,
                            ),
                            DeclarationKind::Call { args, .. } | DeclarationKind::IndirectCall { args, .. } => {
                                for arg in args {
                                    self.find_object_references_operand(
                                        func,
                                        block.id,
                                        inst_id,
                                        arg,
                                        Placement::Before,
                                        location,
                                    );
                                }
                            }
                            DeclarationKind::Cast { .. } | DeclarationKind::Intrinsic { .. } => {}
                        }

                        self.register_gc_object(func, block.id, inst_id, *register, Placement::After, location);
                    }
                    InstructionKind::Allocate { register, .. } => {
                        self.register_gc_object(func, block.id, inst_id, *register, Placement::After, location);
                    }
                    InstructionKind::Store { target, value } | InstructionKind::StoreField { target, value, .. } => {
                        self.register_gc_object(func, block.id, inst_id, *target, Placement::Before, location);
                        self.find_object_references_operand(
                            func,
                            block.id,
                            inst_id,
                            value,
                            Placement::Before,
                            location,
                        );
                    }
                    InstructionKind::ObjectRegister { .. }
                    | InstructionKind::CreateSlot { .. }
                    | InstructionKind::StoreSlot { .. } => {}
                }
            }

            self.marked.clear();
        }
    }

    fn find_object_references_operand(
        &mut self,
        func: &Function,
        block: BasicBlockId,
        inst_id: InstructionId,
        op: &Operand,
        placement: Placement,
        location: Location,
    ) {
        match &op.kind {
            OperandKind::Load { id } | OperandKind::Reference { id } => {
                self.register_gc_object(func, block, inst_id, *id, placement, location);
            }
            OperandKind::LoadField { target, .. } => {
                self.register_gc_object(func, block, inst_id, *target, placement, location);
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

    fn register_gc_object(
        &mut self,
        func: &Function,
        block: BasicBlockId,
        inst: InstructionId,
        register: RegisterId,
        placement: Placement,
        location: Location,
    ) -> bool {
        if self.marked.contains(&register) || !func.registers.register_ty(register).requires_stack_map() {
            return false;
        }

        self.marked.insert(register);
        self.references.push(ObjectReference {
            block,
            target: inst,
            placement,
            location,
            register,
        });

        true
    }
}
