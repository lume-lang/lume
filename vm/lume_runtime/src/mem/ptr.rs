use lume_rt_metadata::TypeMetadata;

unsafe extern "C" {
    pub fn memcpy(dest: *mut u8, src: *const u8, n: usize) -> *mut u8;
}

/// Reads an existing value from the memory location the current pointer is
/// pointing to, offset by the given amount of bytes.
#[expect(clippy::missing_safety_doc, reason = "early alpha stage")]
pub unsafe extern "C" fn lumert_ptr_read(ptr: *mut (), offset: usize, _metadata: *const TypeMetadata) -> *mut () {
    unsafe { ptr.byte_add(offset) }
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
        let ptr: *mut *const () = ptr.add(offset).cast();

        *ptr = value;
    }
}
