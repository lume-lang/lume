use lume_mir::Function;

use crate::MirQueryCtx;

impl MirQueryCtx<'_> {
    /// Determines whether the given MIR function should be dumped to `stdout`,
    /// according to the current compilation options.
    ///
    /// The `pass` argument is used to determine the current MIR pass to check
    /// against, if any. If the current invocation is not occuring inside of
    /// a MIR pass, provide [`None`].
    #[inline]
    pub fn should_dump_func(&self, func: &Function, pass: Option<&str>) -> bool {
        let opts = &self.gcx().session.options;

        // Defines whether all functions should be dumped.
        let include_all_funcs = opts.dump_mir.is_some() && opts.dump_mir_func.is_empty();

        // Determines whether the function should be dumped - but only if the
        // pass isn't rejected!
        let func_name = func.name.clone_inner();
        let should_dump_function = include_all_funcs || opts.dump_mir_func.contains(&func_name);

        match (opts.dump_mir.as_ref(), pass) {
            // If `--dump-mir` was passed without any specific passes, return
            // whether the specific function should be dumped.
            (Some(required_pass), None) if required_pass.is_empty() => should_dump_function,

            // If the user requested to only dump from specific passes, we should
            // also be calling from a MIR pass.
            (Some(required_pass), Some(current_pass)) => {
                // If the current MIR pass isn't requested, skip.
                if !required_pass.contains(&String::from(current_pass)) {
                    return false;
                }

                // If the current pass is requested, return whether the specific function
                // should be dumped.
                should_dump_function
            }

            // If outside of any MIR pass when `--dump-mir` is passed, don't dump anything
            //
            // This prevents dumping methods multiple times; once for each defined pass and once
            // after all passes have finished.
            (Some(required_pass), None) if !required_pass.is_empty() => false,

            // If outside of any MIR pass when `--dump-mir-func` is passed,
            // only dump the requested functions.
            (_, None) if !include_all_funcs => opts.dump_mir_func.contains(&func_name),

            // `--dump-mir` was not defined - nothing to dump.
            (None, Some(_)) => false,

            // If neither `--dump-mir` nor `--dump-mir-func` was passed, dump nothing.
            (_, _) => false,
        }
    }
}
