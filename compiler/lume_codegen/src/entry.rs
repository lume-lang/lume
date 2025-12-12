use cranelift::prelude::*;
use cranelift_codegen::Context;
use cranelift_module::Module;
use lume_errors::{MapDiagnostic, Result, SimpleDiagnostic};

use crate::CraneliftBackend;

impl CraneliftBackend {
    /// Creates the main entrypoint for all Lume binaries.
    ///
    /// When the Lume compiler sees a function of name `main` without any
    /// namespace, it is designated as the entrypoint of the Lume
    /// application.
    ///
    /// Before calling into the main function, the compiler
    /// needs to setup different parts of the standard library
    /// and the garbage collector. Conversely, after the main function has
    /// returned, the same parts need to be torn down and cleared up. These
    /// lifetime routines are invoked from the "real" entrypoint, which
    /// itself ends up calling the `main` function.
    ///
    /// To prevent linker issues, the `main` function of the Lume binary is
    /// implicitly renamed to `__lume_entry`. The code generator then
    /// creates a new function which handles startup, calls `__lume_entry`
    /// and cleans up.
    #[libftrace::traced(level = Debug)]
    pub(crate) fn create_entry_fn(
        &mut self,
        ctx: &mut Context,
        builder_ctx: &mut FunctionBuilderContext,
    ) -> Result<()> {
        let Some(entry_node_id) = self
            .context
            .functions
            .iter()
            .find_map(|(&id, func)| (func.mangled_name == super::LUME_ENTRY).then_some(id))
        else {
            libftrace::warning!("skipping entrypoint generation: no entry found");

            return Ok(());
        };

        let entry_decl = self.declared_funcs.get(&entry_node_id).unwrap();
        let entry_func_id = entry_decl.id;

        let entry_has_own_return = !entry_decl.sig.returns.is_empty();
        let entry_return_type = if let Some(ret_ty) = entry_decl.sig.returns.first() {
            *ret_ty
        } else {
            AbiParam::new(types::I8)
        };

        ctx.func.signature = self.module().make_signature();
        ctx.func.signature.returns.push(entry_return_type);

        let main_func_id = self
            .module_mut()
            .declare_function(
                super::MAIN_ENTRY,
                cranelift_module::Linkage::Export,
                &ctx.func.signature,
            )
            .map_diagnostic()?;

        let mut builder = FunctionBuilder::new(&mut ctx.func, builder_ctx);

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);

        // Call the `__lume_start` function, initializing the standard library and
        // memory heaps.
        let lm_start_id = self.intrinsics.lume_start;
        let lm_start_ref = self.module_mut().declare_func_in_func(lm_start_id, builder.func);
        builder.ins().call(lm_start_ref, &[]);

        // Call into the actual `main` entrypoint.
        let entry_ref = self.module_mut().declare_func_in_func(entry_func_id, builder.func);
        let entry_call = builder.ins().call(entry_ref, &[]);

        let exit_code = if entry_has_own_return {
            builder.inst_results(entry_call)[0]
        } else {
            builder.ins().iconst(entry_return_type.value_type, 0)
        };

        // Call the `__lume_end` function, cleaning up the allocated objects and memory.
        let lm_end_id = self.intrinsics.lume_end;
        let lm_end_ref = self.module_mut().declare_func_in_func(lm_end_id, builder.func);
        builder.ins().call(lm_end_ref, &[]);

        // Return the exit code from the entry point.
        builder.ins().return_(&[exit_code]);

        builder.seal_all_blocks();
        builder.finalize();

        if let Err(err) = self.module_mut().define_function(main_func_id, ctx) {
            libftrace::error!("error caused by function:\n{}", ctx.func);

            // Displaying verifier errors directly gives a really useless error, so to
            // actually know the issue, we're using the debug output of the error in the
            // error.
            let diagnostic = SimpleDiagnostic::new(format!("function verification failed ({})", super::MAIN_ENTRY))
                .add_cause(SimpleDiagnostic::new(format!("{err:#?}")));

            return Err(diagnostic.into());
        }

        self.module().clear_context(ctx);

        Ok(())
    }
}
