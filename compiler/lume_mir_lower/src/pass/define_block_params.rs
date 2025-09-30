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
#[derive(Default, Debug)]
pub(crate) struct DefineBlockParameters {
    params: IndexMap<BasicBlockId, IndexSet<RegisterId>>,
}

impl Pass for DefineBlockParameters {
    /// Returns the unique name of the pass.
    fn name() -> &'static str {
        "define_block_params"
    }

    /// Creates a new instance of the pass without default settings.
    fn new() -> Self {
        Self::default()
    }

    /// Executes the pass on the given function.
    fn execute(&mut self, func: &mut Function) {
        for block in func.blocks.values() {
            let mut visited = IndexSet::new();
            self.find_required_input_registers(func, block, &mut visited);
        }

        for block in func.blocks.values_mut() {
            // Skip blocks which don't have any predecessors, since they cannot
            // have any input parameters. This is mostly for entry blocks, as they
            // get their parameters implictly from the function itself.
            if block.predecessors().count() == 0 {
                continue;
            }

            let Some(params) = self.params.swap_remove(&block.id) else {
                continue;
            };

            block.parameters = params.into_iter().collect();
        }
    }
}

impl DefineBlockParameters {
    fn find_required_input_registers(
        &mut self,
        func: &Function,
        block: &BasicBlock,
        visited: &mut IndexSet<BasicBlockId>,
    ) {
        let mut regs = IndexSet::new();

        for successor in block.successors() {
            if visited.contains(&successor) {
                continue;
            }

            visited.insert(successor);

            let successor_block = func.block(successor);
            self.find_required_input_registers(func, successor_block, visited);

            if let Some(successor_params) = self.params.get(&successor) {
                for successor_param in successor_params {
                    if block.is_register_phi_dest(*successor_param) {
                        continue;
                    }

                    regs.insert(*successor_param);
                }
            }
        }

        // Find all referenced registers within the block from all the instructions
        // and the block terminator...
        for inst in block.instructions() {
            regs.extend(inst.register_refs());
        }

        if let Some(terminator) = block.terminator() {
            regs.extend(terminator.register_refs());
        }

        // ...and scoop away all registers which were referenced within the block, but
        // were also defined within the block.
        for inst in block.instructions() {
            if let Some(def) = inst.register_def() {
                regs.shift_remove(&def);
            }
        }

        self.params.entry(block.id).or_default().extend(regs);
    }
}
