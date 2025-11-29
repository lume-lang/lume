pub mod array;
pub mod io;
pub mod lifetime;
pub mod mem;
pub mod process;
pub mod string;

pub use lume_gc::*;

/// Retrieves of the type metadata of the first type parameter.
///
/// Since Lume passes the metadata of type arguments after all other parameters,
/// we can safely return the first input pointer.
#[unsafe(export_name = "std::type_of")]
pub extern "C" fn type_of(metadata: *const ()) -> *const () {
    metadata
}
