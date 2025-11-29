//! This file holds the two lifetime functions of Lume, which are invoked before
//! and after the main function has been called.
//!
//! The two lifetime functions are:
//! - `__lume_start`: starts setting up the standard library, including the
//!   garbage collector and memory heaps.
//!
//! - `__lume_end`: ensures that all objects are disposed correctly and frees
//!   allocated memory.

#[unsafe(export_name = "__lume_start")]
pub extern "C" fn lm_start() {}

#[unsafe(export_name = "__lume_end")]
pub extern "C" fn lm_end() {
    // Drop all GC allocations and call their disposing methods.
    lume_gc::drop_allocations();
}
