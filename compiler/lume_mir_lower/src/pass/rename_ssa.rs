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
#[derive(Default, Debug)]
pub(crate) struct RenameSsaVariables;

impl Pass for RenameSsaVariables {
    fn name() -> &'static str {
        "rename_ssa"
    }

    /// Creates a new instance of the pass without default settings.
    fn new() -> Self {
        Self
    }

    /// Executes the pass on the given function.
    fn execute(&mut self, mcx: &MirQueryCtx, func: &mut Function) {
        let mut patcher = mcx.patcher(func);

        for parameter_idx in 0..func.signature.parameters.len() {
            let renamed_register = patcher.next_local();

            patcher.rename_register(BasicBlockId(0), RegisterId::new(parameter_idx), renamed_register);
        }

        for (&block_id, block) in &func.blocks {
            for &parameter in &block.parameters {
                let renamed_register = patcher.next_local();
                patcher.rename_register(block_id, parameter, renamed_register);
            }

            for inst in block.instructions() {
                match &inst.kind {
                    InstructionKind::Let { register, .. } | InstructionKind::Allocate { register, .. } => {
                        let renamed_register = patcher.next_local();
                        patcher.rename_register(block_id, *register, renamed_register);
                    }
                    InstructionKind::Store { .. }
                    | InstructionKind::StoreField { .. }
                    | InstructionKind::ObjectRegister { .. }
                    | InstructionKind::CreateSlot { .. }
                    | InstructionKind::StoreSlot { .. } => {}
                }
            }
        }

        patcher.apply(func);
    }
}
