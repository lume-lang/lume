use std::collections::HashSet;

use lume_mir_queries::patch::{Placement, RelativePlacement};

use super::*;

#[derive(Debug, Clone)]
struct ObjectReference {
    /// The block in which the instruction was found.
    pub block: BasicBlockId,

    /// The placement of the GC reference instruction.
    pub placement: Placement,

    /// The register which needs to marked as an object.
    pub register: RegisterId,
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
    fn execute(&mut self, mcx: &MirQueryCtx, func: &mut Function) {
        self.find_object_references(func);

        let mut patcher = mcx.patcher(func);

        while let Some(reference) = self.references.pop() {
            let placement = RelativePlacement {
                relative: reference.placement,
                block_id: reference.block,
            };

            patcher.add_instruction(placement, lume_mir::InstructionKind::ObjectRegister {
                register: reference.register,
            });
        }

        patcher.apply(func);
    }
}

impl MarkObjectReferences {
    fn find_object_references(&mut self, func: &Function) {
        for block in func.blocks.values() {
            for param in block.parameters() {
                self.register_gc_object(func, block.id, param, Placement::Beginning);
            }

            for (&inst_id, inst) in &block.instructions {
                match &inst.kind {
                    InstructionKind::Let { register, decl, .. } => {
                        match decl.kind.as_ref() {
                            DeclarationKind::Operand(operand) => {
                                self.find_object_references_operand(func, block.id, operand, Placement::Before {
                                    inst: inst_id,
                                });
                            }
                            DeclarationKind::Call { args, .. } | DeclarationKind::IndirectCall { args, .. } => {
                                for arg in args {
                                    self.find_object_references_operand(func, block.id, arg, Placement::Before {
                                        inst: inst_id,
                                    });
                                }
                            }
                            DeclarationKind::Cast { .. } | DeclarationKind::Intrinsic { .. } => {}
                        }

                        self.register_gc_object(func, block.id, *register, Placement::After { inst: inst_id });
                    }
                    InstructionKind::Allocate { register, .. } => {
                        self.register_gc_object(func, block.id, *register, Placement::After { inst: inst_id });
                    }
                    InstructionKind::Store { target, value } | InstructionKind::StoreField { target, value, .. } => {
                        self.register_gc_object(func, block.id, *target, Placement::Before { inst: inst_id });
                        self.find_object_references_operand(func, block.id, value, Placement::Before { inst: inst_id });
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
        op: &Operand,
        placement: Placement,
    ) {
        match &op.kind {
            OperandKind::Load { id, .. } | OperandKind::Reference { id } | OperandKind::Untagged { id } => {
                self.register_gc_object(func, block, *id, placement);
            }
            OperandKind::LoadField { target, .. } => {
                self.register_gc_object(func, block, *target, placement);
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
        register: RegisterId,
        placement: Placement,
    ) -> bool {
        if self.marked.contains(&register) || !func.registers.local_type(register).requires_stack_map() {
            return false;
        }

        self.marked.insert(register);
        self.references.push(ObjectReference {
            block,
            placement,
            register,
        });

        true
    }
}
