pub use error_snippet;

/// Asserts that the given [`DiagCtx`] renders the same output as
/// has been saved and snapshot in a previous iteration.
///
/// # Panics
///
/// Panics if the given [`DiagCtx`] is unbuffered.
#[macro_export]
macro_rules! assert_dcx_snapshot {
    ($dcx:expr) => {
        let mut renderer = $crate::error_snippet::GraphicalRenderer::new();
        renderer.use_colors = false;

        let buffer = $dcx.render_buffer(&mut renderer).unwrap_or_default();

        insta::with_settings!({ omit_expression => true }, {
            insta::assert_snapshot!(buffer);
        });
    };
    ($input:expr, $dcx:expr) => {
        let mut renderer = $crate::error_snippet::GraphicalRenderer::new();
        renderer.use_colors = false;

        let buffer = $dcx.render_buffer(&mut renderer).unwrap_or_default();

        insta::with_settings!({
            description => $input,
            omit_expression => true,
        }, {
            insta::assert_snapshot!(buffer);
        });
    };
}
