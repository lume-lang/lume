//! This file holds the two lifetime functions of Lume, which are invoked before
//! and after the main function has been called.
//!
//! The two lifetime functions are:
//! - `__lume_start`: starts setting up the standard library, including the
//!   garbage collector and memory heaps.
//!
//! - `__lume_end`: ensures that all objects are disposed correctly and frees
//!   allocated memory.

use std::sync::LazyLock;

use lume_options::RuntimeOptions;

#[unsafe(export_name = "__lume_start")]
pub extern "C" fn lm_start() {
    // Initialize the GC with the runtime options, so we have a heap we can allocate
    // into.
    lume_gc::initialize(&RUNTIME_OPTIONS);
}

#[unsafe(export_name = "__lume_end")]
pub extern "C" fn lm_end() {
    // Drop all GC allocations and call their disposing methods.
    lume_gc::drop_allocations();
}

unsafe extern "C" {
    /// Static reference to the `__lume_options` symbol.
    #[link_name = "__lume_options"]
    static __LUME_OPTIONS: u8;
}

static RUNTIME_OPTIONS: LazyLock<RuntimeOptions> = LazyLock::new(parse_runtime_options);

/// Reads and parses the runtime options, which are embedded in the binary.
///
/// This function should only ever be executed *once* at startup, since it's
/// somewhat slow.
pub fn parse_runtime_options() -> RuntimeOptions {
    let ptr = &raw const __LUME_OPTIONS;

    lume_options::from_ptr(ptr).unwrap()
}
