use lume_rt_metadata::TypeMetadata;

/// Creates a null pointer of type `T`.
#[unsafe(export_name = "std::mem::ptr_null")]
#[expect(clippy::missing_safety_doc, reason = "early alpha stage")]
pub unsafe extern "C" fn lumert_ptr_null(_metadata: *const TypeMetadata) -> *mut () {
    std::ptr::null_mut()
}

/// Gets the given pointer as a reference, to the value which the pointer is
/// pointing to.
#[unsafe(export_name = "std::mem::ptr_ref")]
#[expect(clippy::missing_safety_doc, reason = "early alpha stage")]
pub unsafe extern "C" fn lumert_ptr_ref(ptr: *mut (), _metadata: *const TypeMetadata) -> *mut () {
    ptr
}

/// Reads an existing value from the memory location the current pointer is
/// pointing to, offset by the given amount of bytes.
#[unsafe(export_name = "std::mem::ptr_read")]
#[expect(clippy::missing_safety_doc, reason = "early alpha stage")]
pub unsafe extern "C" fn lumert_ptr_read(ptr: *mut (), offset: usize, _metadata: *const TypeMetadata) -> *mut () {
    unsafe { ptr.byte_add(offset).cast::<*mut ()>().read() }
}

/// Writes a new value to the memory location the current pointer is pointing
/// to, offset by the given amount of bytes.
#[unsafe(export_name = "std::mem::ptr_write")]
#[expect(clippy::missing_safety_doc, reason = "early alpha stage")]
pub unsafe extern "C" fn lumert_ptr_write(
    ptr: *mut (),
    value: *const (),
    offset: usize,
    _metadata: *const TypeMetadata,
) {
    unsafe {
        let dest: *mut *const () = ptr.byte_add(offset).cast();

        *dest = value;
    }
}
