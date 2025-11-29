/// Main entrypoint of all Lume applications.
#[unsafe(export_name = "main")]
pub extern "C" fn entry() -> u8 {
    unsafe extern "C" {
        pub fn __lume_entry() -> u8;
    }

    let exit = unsafe { __lume_entry() };

    // Drop all GC allocations and call their disposing methods.
    lume_gc::drop_allocations();

    exit
}
