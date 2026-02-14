pub mod array;
pub mod io;
pub mod lifetime;
pub mod mem;
pub mod process;
pub mod string;

pub use lume_gc::*;

pub const POINTER_SIZE: usize = size_of::<*const u8>();

/// Retrieves of the type metadata of the first type parameter.
///
/// Since Lume passes the metadata of type arguments after all other parameters,
/// the first parameter would be the pointer to the type metadata.
///
/// However, since the type parameter metadata is retrieved from an alias
/// symbol, the pointer would point to the metadata of the metadata. While this
/// is expected in the Rust runtime, Lume programs expect the pointer to point
/// to the first field of the type parameter metadata.
#[unsafe(export_name = "std::type_of")]
pub unsafe extern "C" fn type_of(metadata: *const ()) -> *const () {
    unsafe { metadata.byte_add(POINTER_SIZE) }
}

/// Finds the method with the given ID on the given type metadata.
///
/// If the method is not found, returns `null` pointer.
#[unsafe(export_name = "std::find_method_on")]
pub unsafe extern "C" fn find_method_on(method_id: lume_rt_metadata::FunctionId, metadata: *const ()) -> *const () {
    let metadata = unsafe { metadata.cast::<lume_rt_metadata::TypeMetadata>().read() };

    for method in metadata.methods.items() {
        if method.func_id == method_id {
            return method.func_ptr.cast();
        }
    }

    std::ptr::null()
}
