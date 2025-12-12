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

/// Finds the method with the given ID on the given type metadata.
///
/// If the method is not found, returns `null` pointer.
#[unsafe(export_name = "std::find_method_on")]
pub unsafe extern "C" fn find_method_on(method_id: lume_rt_metadata::FunctionId, metadata: *const ()) -> *const () {
    let metadata = unsafe { metadata.cast::<lume_rt_metadata::TypeMetadata>().read() };

    for method_ptr in metadata.methods.items() {
        let method = unsafe { method_ptr.read() };

        if method.func_id == method_id {
            return method.func_ptr as *const _;
        }
    }

    std::ptr::null()
}
