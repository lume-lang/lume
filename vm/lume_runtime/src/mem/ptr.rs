use lume_rt_metadata::TypeMetadata;

unsafe extern "C" {
    pub fn memcpy(dest: *mut (), src: *const (), n: usize) -> *mut ();
}

/// Gets the given pointer as a reference, to the value which the pointer is
/// pointing to.
#[expect(clippy::missing_safety_doc, reason = "early alpha stage")]
pub unsafe extern "C" fn lumert_ptr_ref(ptr: *mut (), _metadata: *const TypeMetadata) -> *mut () {
    ptr
}

/// Reads an existing value from the memory location the current pointer is
/// pointing to, offset by the given amount of bytes.
#[expect(clippy::missing_safety_doc, reason = "early alpha stage")]
pub unsafe extern "C" fn lumert_ptr_read(ptr: *mut (), offset: usize, _metadata: *const TypeMetadata) -> *mut () {
    unsafe { ptr.byte_add(offset).cast::<*mut ()>().read() }
}

/// Writes a new value to the memory location the current pointer is pointing
/// to, offset by the given amount of bytes.
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
