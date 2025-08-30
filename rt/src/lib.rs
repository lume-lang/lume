pub mod array;
pub mod mem;
pub mod metadata;
pub mod pointer;
pub mod print;
pub mod string;

pub use string::*;

/// Retrieves of the type metadata of the first type parameter.
///
/// Since Lume passes the metadata of type arguments after all other parameters,
/// we can safely return the first input pointer.
#[unsafe(export_name = "std::type_of")]
pub extern "C" fn type_of(metadata: *const ()) -> *const () {
    metadata
}
