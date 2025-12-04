use super::*;

/// Using the result of the previous pass, [`DefineBlockParameters`], traverses
/// through each block and passes the appropriate registers to each call to any
/// successor blocks.
///
/// For example, given MIR like the following:
/// ```mir
/// B0:
///     #0 = 4_i32
///     goto B1
/// B1(#0):
///     #1 = 7_i32
///     #2: i32 = +(#0, #1)
///     goto B2
/// B2(#2, #0):
///     #3: i32 = *(#2, #0)
///     return #3
/// ```
///
/// Each successor block, except for `B2`, is meant to pass one-or-more
/// registers to their successor blocks. This pass will look through each block
/// and update their terminator to pass the required registers. After the pass,
/// the same MIR will look like this:
/// ```mir
/// B0:
///     #0 = 4_i32
///     goto B1(#0)          <-- notice the added block arguments
/// B1(#0):
///     #1 = 7_i32
///     #2: i32 = +(#0, #1)
///     goto B2(#2, #0)
/// B2(#2, #0):
///     #3: i32 = *(#2, #0)  <-- notice the added block arguments
///     return #3
/// ```
#[derive(Default, Debug)]
pub(crate) struct PassBlockArguments;

impl Pass for PassBlockArguments {
    fn name() -> &'static str {
        "pass_block_args"
    }

    /// Creates a new instance of the pass without default settings.
    fn new() -> Self {
        Self
    }

    /// Executes the pass on the given function.
    fn execute(&mut self, func: &mut Function) {
        // TODO:
        // I hate this, but we're not quite at the "we should optimize the compiler
        // more" phase, yet. But, this is certainly not a good solution to a
        // lifetime issue.
        let func_immut = func.clone();

        for block in func.blocks.values_mut() {
            let block_id = block.id;

            if let Some(terminator) = block.terminator_mut() {
                match &mut terminator.kind {
                    TerminatorKind::Branch(target) => update_branch_terminator(block_id, target, &func_immut),
                    TerminatorKind::ConditionalBranch {
                        then_block, else_block, ..
                    } => {
                        update_branch_terminator(block_id, then_block, &func_immut);
                        update_branch_terminator(block_id, else_block, &func_immut);
                    }
                    TerminatorKind::Switch { arms, fallback, .. } => {
                        for arm in arms {
                            update_branch_terminator(block_id, &mut arm.1, &func_immut);
                        }

                        update_branch_terminator(block_id, fallback, &func_immut);
                    }
                    TerminatorKind::Return(_) | TerminatorKind::Unreachable => {}
                }
            }
        }
    }
}

fn update_branch_terminator(block: BasicBlockId, call_site: &mut BlockBranchSite, func: &Function) {
    let source_block = func.block(block);
    let target_block = func.block(call_site.block);

    if target_block.parameters.len() < call_site.arguments.len() {
        call_site.arguments.drain(target_block.parameters.len()..);
    }

    call_site.arguments.reserve(target_block.parameters.len());

    for param in &target_block.parameters {
        let arg = source_block.resolve_phi_source(*param).unwrap_or(*param);

        if !call_site.arguments.iter().any(|a| a.is_reference_of(arg)) {
            call_site.arguments.push(Operand::reference_of(arg));
        }
    }
}
