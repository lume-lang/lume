use indexmap::IndexMap;
use lume_mir_queries::analysis::liveness::BlockLiveness;

use super::*;

/// Determines which parameters are required for a given block, as well
/// as any subsequent successor blocks, so that matching registers can be
/// passed from any successor blocks.
///
/// For example, given MIR like the following:
/// ```mir
/// B0:
///     #0 = 4_i32
///     goto B1
/// B1:
///     #1 = 7_i32
///     #2: i32 = +(#0, #1)
///     goto B2
/// B2:
///     #3: i32 = *(#2, #0)
///     return #3
/// ```
///
/// Each successor block, except for `B0`, requires a register from a previous
/// blocks. This pass will look for all registers required for each block and
/// declare them as parameters of the block, if they cannot be found in the
/// block itself. After the pass, the same MIR will look like this:
/// ```mir
/// B0:
///     #0 = 4_i32
///     goto B1
/// B1(#0):       <-- notice the added block parameters
///     #1 = 7_i32
///     #2: i32 = +(#0, #1)
///     goto B2
/// B2(#2, #0):   <-- notice the added block parameters
///     #3: i32 = *(#2, #0)
///     return #3
/// ```
#[derive(Default)]
pub(crate) struct DefineLiveness;

impl Pass for DefineLiveness {
    /// Returns the unique name of the pass.
    fn name() -> &'static str {
        "liveness"
    }

    /// Creates a new instance of the pass without default settings.
    fn new() -> Self {
        Self
    }

    /// Executes the pass on the given function.
    fn execute(&mut self, mcx: &MirQueryCtx, func: &mut Function) {
        for (block_id, BlockLiveness { live_in, live_out }) in mcx.liveness(func) {
            let block = func.blocks.get_mut(&block_id).unwrap();

            // Entry blocks should not have any parameters, since they get inherited from
            // the function.
            if !block.is_entry() {
                block.add_parameters(
                    live_in
                        .into_iter()
                        .map(|var| func.variables.bound_variable(var).register),
                );
            }

            for (successor, live_out_set) in live_out {
                for output_var in live_out_set {
                    let bound_register = func.variables.bound_variable(output_var).register;
                    let output_register = func.variables.moved_register(block_id, bound_register);

                    let output_op = Operand::reference_of(output_register);

                    block.add_terminator_arg(output_op, successor);
                }
            }
        }
    }
}

/// Dump the given liveness information for a function into `stdout`.
#[cfg(debug_assertions)]
#[allow(dead_code, clippy::disallowed_macros, reason = "only used in debugging")]
fn dump_liveness(func: &Function, liveness: &IndexMap<BasicBlockId, BlockLiveness>) {
    println!("{}:", func.name);
    for (&block_id, BlockLiveness { live_in, live_out }) in liveness {
        println!("  [{block_id}]");

        println!(
            "    live-in:\t\t{}",
            live_in
                .iter()
                .map(|var| {
                    let bound_register = func.variables.bound_variable(*var).register;

                    format!("{var} ({bound_register})")
                })
                .collect::<Vec<_>>()
                .join(", ")
        );

        for (&successor, live_out_set) in live_out {
            println!(
                "    live-out[->{successor}]:\t{}",
                live_out_set
                    .iter()
                    .map(|var| {
                        let bound_register = func.variables.bound_variable(*var).register;
                        let moved_register = func.variables.moved_register(block_id, bound_register);

                        format!("{var} ({moved_register})")
                    })
                    .collect::<Vec<_>>()
                    .join(", ")
            );
        }
    }

    println!();
}
