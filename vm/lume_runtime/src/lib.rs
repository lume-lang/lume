pub mod array;
pub mod io;
pub mod mem;
pub mod string;

/// Retrieves of the type metadata of the first type parameter.
///
/// Since Lume passes the metadata of type arguments after all other parameters,
/// we can safely return the first input pointer.
pub extern "C" fn type_of(metadata: *const ()) -> *const () {
    metadata
}
