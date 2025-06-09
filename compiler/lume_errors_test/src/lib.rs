/// Asserts that the given [`DiagCtx`] renders the same output as
/// has been saved and snapshot in a previous iteration.
///
/// # Panics
///
/// Panics if the given [`DiagCtx`] is unbuffered.
#[macro_export]
macro_rules! assert_dcx_snapshot {
    ($input:expr, $dcx:expr) => {
        let Some(buffer) = $dcx.buffer() else {
            panic!("bug!: snapshot test uses non-buffered DiagCtx")
        };

        insta::with_settings!({
            description => $input,
            omit_expression => true,
        }, {
            insta::assert_snapshot!(buffer);
        });
    };
}
