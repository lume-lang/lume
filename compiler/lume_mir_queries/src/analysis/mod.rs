use std::ops::ControlFlow;

use indexmap::IndexSet;
use lume_architect::cached_query;
use lume_mir::*;
use lume_span::NodeId;

use crate::MirQueryCtx;

/// Defines the limit of nested calls before aborting an escape analysis.
const ESCAPE_ANALYSIS_CALL_LIMIT: usize = 5;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EscapeResult {
    /// Represents the register as having escaped the function,
    /// either via opaque function call, call-stack became too
    /// long or otherwise.
    Escaped { reason: EscapeReason },

    /// Represents that the register did not escape the function.
    Unescaped,
}

/// Represents the reason why a specific register escaped the containing parent.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EscapeReason {
    /// Represents that analysis was aborted due to the call stack
    /// reaching the limit before returning a valid result.
    CallStackLimit,

    /// The analysis yielded no result, as some functioncall within
    /// the call-stack invoked an external function.
    Indeterminate,

    /// Represents that the register escaped via return statement.
    Returned,
}

impl MirQueryCtx<'_> {
    /// Attempts to determine whether the given register will escape
    /// the containing function or if it is only used locally within
    /// the function.
    ///
    /// This method is used to perform escape analysis on registers,
    /// which can be used in allocation optimization.
    ///
    /// # Returns
    ///
    /// If the method determines with certainty whether the register
    /// escapes the function or not, it is returned as [`Some`]. If
    /// the method cannot determine whether the register escapes, it
    /// returns [`None`].
    #[cached_query(key = (func.id, block_id, reg))]
    #[tracing::instrument(level = "TRACE", skip(self, func), fields(name = %func.name), ret)]
    pub fn does_register_escape(
        &self,
        func: &lume_mir::Function,
        block_id: BasicBlockId,
        reg: RegisterId,
    ) -> EscapeResult {
        let mut call_stack = IndexSet::with_capacity(ESCAPE_ANALYSIS_CALL_LIMIT);

        match self.does_register_escape_inner(func, block_id, reg, &mut call_stack) {
            ControlFlow::Break(reason) => EscapeResult::Escaped { reason },
            ControlFlow::Continue(()) => EscapeResult::Unescaped,
        }
    }

    fn does_register_escape_inner(
        &self,
        func: &lume_mir::Function,
        block_id: BasicBlockId,
        reg: RegisterId,
        call_stack: &mut IndexSet<NodeId>,
    ) -> ControlFlow<EscapeReason, ()> {
        // Limit the analysis to not make this analysis incredibly slow.
        if call_stack.len() > ESCAPE_ANALYSIS_CALL_LIMIT {
            return ControlFlow::Break(EscapeReason::CallStackLimit);
        }

        // If the function is external, we cannot analyse it and must
        // assume it escapes further out.
        if func.signature.external {
            return ControlFlow::Break(EscapeReason::Indeterminate);
        }

        call_stack.insert(func.id);

        let block = func.block(block_id);

        for inst in block.instructions() {
            match &inst.kind {
                InstructionKind::Let { register, decl, .. } => match &decl.kind {
                    DeclarationKind::Operand(op) => {
                        if op.stores_register(reg) {
                            self.does_register_escape_inner(func, block_id, *register, call_stack)?;
                        }
                    }
                    DeclarationKind::Call {
                        func_id: callee_id,
                        args,
                    } => {
                        for (idx, arg) in args.iter().enumerate() {
                            if arg.stores_register(reg) {
                                let param = RegisterId::new(idx);
                                let callee = self.mir().function(*callee_id);

                                self.does_register_escape_inner(callee, BasicBlockId(0), param, call_stack)?;
                            }
                        }
                    }
                    DeclarationKind::IndirectCall { .. } => {
                        return ControlFlow::Break(EscapeReason::Indeterminate);
                    }
                    DeclarationKind::Cast { .. } | DeclarationKind::Intrinsic { .. } => {}
                },
                InstructionKind::Store { target, value } | InstructionKind::StoreField { target, value, .. } => {
                    if value.stores_register(reg) {
                        self.does_register_escape_inner(func, block_id, *target, call_stack)?;
                    }
                }
                InstructionKind::Allocate { .. }
                | InstructionKind::ObjectRegister { .. }
                | InstructionKind::Assign { .. }
                | InstructionKind::CreateSlot { .. }
                | InstructionKind::StoreSlot { .. } => {}
            }
        }

        if let Some(terminator) = block.terminator() {
            match &terminator.kind {
                TerminatorKind::ConditionalBranch {
                    then_block, else_block, ..
                } => {
                    self.does_register_escape_call(func, then_block, reg, call_stack)?;
                    self.does_register_escape_call(func, else_block, reg, call_stack)?;
                }
                TerminatorKind::Branch(call_site) => {
                    self.does_register_escape_call(func, call_site, reg, call_stack)?;
                }
                TerminatorKind::Switch { arms, fallback, .. } => {
                    for (_, arm) in arms {
                        self.does_register_escape_call(func, arm, reg, call_stack)?;
                    }

                    self.does_register_escape_call(func, fallback, reg, call_stack)?;
                }
                TerminatorKind::Return(Some(op)) => {
                    if op.stores_register(reg) {
                        return ControlFlow::Break(EscapeReason::Returned);
                    }
                }
                TerminatorKind::Return(None) | TerminatorKind::Unreachable => {}
            }
        }

        ControlFlow::Continue(())
    }

    /// Attempts to determine whether the given register escapes the function
    /// through the given call site.
    fn does_register_escape_call(
        &self,
        func: &lume_mir::Function,
        call_site: &BlockBranchSite,
        reg: RegisterId,
        call_stack: &mut IndexSet<NodeId>,
    ) -> ControlFlow<EscapeReason, ()> {
        for (idx, arg) in call_site.arguments.iter().enumerate() {
            if arg.stores_register(reg) {
                let block = func.block(call_site.block);
                let param = block.parameters[idx];

                self.does_register_escape_inner(func, call_site.block, param, call_stack)?;
            }
        }

        ControlFlow::Continue(())
    }
}
