pub use walk_mut::{VisitorMut, walk_mut};
pub use walk_ref::{Visitor, walk};

use crate::*;

pub mod walk_ref {
    use super::*;

    /// Visitor trait for traversing a MIR function.
    pub trait Visitor<'mir> {
        fn visit_function(&mut self, _func: &'mir Function) {}

        fn visit_block(&mut self, _func: &'mir Function, _block: &'mir BasicBlock) {}

        fn visit_instruction(&mut self, _func: &'mir Function, _inst: &'mir Instruction) {}

        fn visit_terminator(&mut self, _func: &'mir Function, _term: &'mir Terminator) {}

        fn visit_declaration(&mut self, _func: &'mir Function, _decl: &'mir Declaration) {}

        fn visit_operand(&mut self, _func: &'mir Function, _op: &'mir Operand) {}
    }

    /// Traverses the given MIR function using the provided visitor.
    pub fn walk<'mir, V: Visitor<'mir>>(func: &'mir Function, visitor: &mut V) {
        visitor.visit_function(func);

        for block in func.blocks.values() {
            traverse_block(func, visitor, block);
        }
    }

    pub fn traverse_block<'mir, V: Visitor<'mir>>(func: &'mir Function, visitor: &mut V, block: &'mir BasicBlock) {
        visitor.visit_block(func, block);

        for inst in block.instructions.values() {
            traverse_instruction(func, visitor, inst);
        }

        if let Some(term) = &block.terminator {
            traverse_terminator(func, visitor, term);
        }
    }

    fn traverse_instruction<'mir, V: Visitor<'mir>>(func: &'mir Function, visitor: &mut V, inst: &'mir Instruction) {
        visitor.visit_instruction(func, inst);

        match &inst.kind {
            InstructionKind::Let { decl, .. } => {
                traverse_declaration(func, visitor, decl);
            }

            InstructionKind::Store { value, .. }
            | InstructionKind::StoreSlot { value, .. }
            | InstructionKind::StoreField { value, .. } => {
                visitor.visit_operand(func, value);
            }

            InstructionKind::Allocate { .. }
            | InstructionKind::CreateSlot { .. }
            | InstructionKind::ObjectRegister { .. } => {}
        }
    }

    fn traverse_terminator<'mir, V: Visitor<'mir>>(func: &'mir Function, visitor: &mut V, term: &'mir Terminator) {
        visitor.visit_terminator(func, term);

        match &term.kind {
            TerminatorKind::Return(operand) => {
                if let Some(operand) = operand {
                    visitor.visit_operand(func, operand);
                }
            }
            TerminatorKind::ConditionalBranch {
                then_block, else_block, ..
            } => {
                for arg in &then_block.arguments {
                    visitor.visit_operand(func, arg);
                }

                for arg in &else_block.arguments {
                    visitor.visit_operand(func, arg);
                }
            }
            TerminatorKind::Switch { arms, fallback, .. } => {
                for (_, arm) in arms {
                    for arg in &arm.arguments {
                        visitor.visit_operand(func, arg);
                    }
                }

                for arg in &fallback.arguments {
                    visitor.visit_operand(func, arg);
                }
            }
            TerminatorKind::Branch(site) => {
                for arg in &site.arguments {
                    visitor.visit_operand(func, arg);
                }
            }
            TerminatorKind::Unreachable => {}
        }
    }

    fn traverse_declaration<'mir, V: Visitor<'mir>>(func: &'mir Function, visitor: &mut V, decl: &'mir Declaration) {
        visitor.visit_declaration(func, decl);

        match decl.kind.as_ref() {
            DeclarationKind::Operand(operand) => visitor.visit_operand(func, operand),
            DeclarationKind::Intrinsic { args, .. }
            | DeclarationKind::Call { args, .. }
            | DeclarationKind::IndirectCall { args, .. } => {
                for arg in args {
                    visitor.visit_operand(func, arg);
                }
            }
            DeclarationKind::Cast { .. } => {}
        }
    }
}

pub mod walk_mut {
    use super::*;

    /// Visitor trait for traversing a MIR function.
    pub trait VisitorMut {
        fn visit_function(&mut self, _func: &mut Function) {}

        fn visit_block(&mut self, _block: &mut BasicBlock) {}

        fn visit_instruction(&mut self, _inst: &mut Instruction) {}

        fn visit_terminator(&mut self, _term: &mut Terminator) {}

        fn visit_declaration(&mut self, _decl: &mut Declaration) {}

        fn visit_operand(&mut self, _op: &mut Operand) {}
    }

    /// Traverses the given MIR function using the provided visitor.
    pub fn walk_mut<V: VisitorMut>(func: &mut Function, visitor: &mut V) {
        visitor.visit_function(func);

        for block in func.blocks.values_mut() {
            traverse_block(visitor, block);
        }
    }

    pub fn traverse_block<V: VisitorMut>(visitor: &mut V, block: &mut BasicBlock) {
        visitor.visit_block(block);

        for inst in block.instructions.values_mut() {
            traverse_instruction(visitor, inst);
        }

        if let Some(term) = &mut block.terminator {
            traverse_terminator(visitor, term);
        }
    }

    fn traverse_instruction<V: VisitorMut>(visitor: &mut V, inst: &mut Instruction) {
        visitor.visit_instruction(inst);

        match &mut inst.kind {
            InstructionKind::Let { decl, .. } => {
                traverse_declaration(visitor, decl);
            }

            InstructionKind::Store { value, .. }
            | InstructionKind::StoreSlot { value, .. }
            | InstructionKind::StoreField { value, .. } => {
                visitor.visit_operand(value);
            }

            InstructionKind::Allocate { .. }
            | InstructionKind::CreateSlot { .. }
            | InstructionKind::ObjectRegister { .. } => {}
        }
    }

    fn traverse_terminator<V: VisitorMut>(visitor: &mut V, term: &mut Terminator) {
        visitor.visit_terminator(term);

        match &mut term.kind {
            TerminatorKind::Return(operand) => {
                if let Some(operand) = operand {
                    visitor.visit_operand(operand);
                }
            }
            TerminatorKind::ConditionalBranch {
                then_block, else_block, ..
            } => {
                for arg in &mut then_block.arguments {
                    visitor.visit_operand(arg);
                }

                for arg in &mut else_block.arguments {
                    visitor.visit_operand(arg);
                }
            }
            TerminatorKind::Switch { arms, fallback, .. } => {
                for (_, arm) in arms {
                    for arg in &mut arm.arguments {
                        visitor.visit_operand(arg);
                    }
                }

                for arg in &mut fallback.arguments {
                    visitor.visit_operand(arg);
                }
            }
            TerminatorKind::Branch(site) => {
                for arg in &mut site.arguments {
                    visitor.visit_operand(arg);
                }
            }
            TerminatorKind::Unreachable => {}
        }
    }

    fn traverse_declaration<V: VisitorMut>(visitor: &mut V, decl: &mut Declaration) {
        visitor.visit_declaration(decl);

        match decl.kind.as_mut() {
            DeclarationKind::Operand(operand) => visitor.visit_operand(operand),
            DeclarationKind::Intrinsic { args, .. }
            | DeclarationKind::Call { args, .. }
            | DeclarationKind::IndirectCall { args, .. } => {
                for arg in args {
                    visitor.visit_operand(arg);
                }
            }
            DeclarationKind::Cast { .. } => {}
        }
    }
}
