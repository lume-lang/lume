use lume_mir::{RegisterId, Signature};
use lume_span::{Location, NodeId};

use crate::builder::Builder;

pub(crate) struct DynamicShimBuilder<'shim, 'mir, 'tcx> {
    builder: &'shim mut Builder<'mir, 'tcx>,

    /// Defines the ID of the method to be called.
    target: NodeId,
    signature: Signature,
    parameters: Vec<RegisterId>,
}

impl<'shim, 'mir, 'tcx> DynamicShimBuilder<'shim, 'mir, 'tcx> {
    pub(crate) fn new(builder: &'shim mut Builder<'mir, 'tcx>, func_id: NodeId) -> Self {
        builder.func.mangled_name = format!("_dyn_{}", builder.func.mangled_name);

        // The last parameter in dynamic function declarations contains the type
        // metadata of the implemented type. Since this parameter shouldn't
        // be passed to the target function, we remove it from the parameters list.

        let mut params = builder.func.parameter_registers().collect::<Vec<_>>();
        params.pop();

        let mut signature = builder.func.signature.clone();
        signature.parameters.pop();

        Self {
            builder,
            signature,
            parameters: params,
            target: func_id,
        }
    }

    /// Returns the signature of the target function.
    fn target_signature(&self) -> Signature {
        let mut signature = self.signature.clone();
        signature.return_type = self.builder.function_ret_type(self.target);

        signature
    }

    /// Returns the non-dynamic parameters of the function as operands.
    fn parameters_as_operands(&self) -> Vec<lume_mir::Operand> {
        self.parameters
            .iter()
            .map(|id| lume_mir::Operand::reference_of(*id))
            .collect()
    }

    /// Creates the shim function around the function with the given ID and
    /// returns the ID if the created shim.
    pub(crate) fn build(self) -> NodeId {
        let entry_block = self.builder.new_block();
        let method_found = self.builder.new_block();
        let method_missing = self.builder.new_block();

        let signature = self.target_signature();
        let method_arguments = self.parameters_as_operands();

        self.builder.with_block(entry_block, |builder, _| {
            let parameters = builder.func.parameters().collect::<Vec<_>>();
            let type_metadata_reg = parameters.last().unwrap().id;

            let lookup_method_id = builder.tcx().lang_item("lookup_method").unwrap();

            let method_id_arg = lume_mir::Operand::integer(64, false, self.target.as_usize().cast_signed() as i64);
            let metadata_arg = lume_mir::Operand::reference_of(type_metadata_reg);

            let method_ptr = builder.call(lookup_method_id, vec![method_id_arg, metadata_arg], Location::empty());
            let null_cmp = builder.ieq_imm(lume_mir::Operand::reference_of(method_ptr), 0, 64, false);

            let found_block_args = [&[method_ptr][..], &self.parameters[..]].concat();

            builder.conditional_branch_with(
                null_cmp,
                method_missing,
                &[],
                method_found,
                found_block_args.as_slice(),
                Location::empty(),
            );
        });

        self.builder.with_block(method_missing, |builder, _| {
            builder.unreachable(Location::empty());
        });

        self.builder.with_block(method_found, |builder, block_id| {
            // Add parameter for the returned method pointer.
            let method_ptr = builder.func.add_block_parameter(block_id, lume_mir::Type::void_ptr());

            for &param in &self.parameters {
                let parameter_type = &builder.func.register(param).ty;
                builder.func.add_block_parameter(block_id, parameter_type.clone());
            }

            let return_value = builder.call_indirect(method_ptr, signature, method_arguments, Location::empty());

            builder.return_(Some(lume_mir::Operand::reference_of(return_value)), Location::empty());
        });

        self.target
    }
}
