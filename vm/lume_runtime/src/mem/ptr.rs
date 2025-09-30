use lume_rt_metadata::TypeMetadata;

unsafe extern "C" {
    pub fn memcpy(dest: *mut u8, src: *const u8, n: usize) -> *mut u8;
}

/// Reads an existing value from the memory location the current pointer is
/// pointing to, offset by the given amount of bytes.
#[expect(clippy::missing_safety_doc, reason = "early alpha stage")]
pub unsafe extern "C" fn lumert_ptr_read(ptr: *mut u8, offset: usize, metadata: *const TypeMetadata) -> *mut u8 {
    let value_size = unsafe { metadata.read().size };
    let dest = unsafe { crate::mem::lumert_alloc(value_size, metadata) };

    unsafe { memcpy(dest, ptr.byte_add(offset), value_size) };

    dest
}

/// Writes a new value to the memory location the current pointer is pointing
/// to, offset by the given amount of bytes.
#[expect(clippy::missing_safety_doc, reason = "early alpha stage")]
pub unsafe extern "C" fn lumert_ptr_write(
    ptr: *mut u8,
    value: *const u8,
    offset: usize,
    metadata: *const TypeMetadata,
) {
    let value_size = unsafe { metadata.read().size };

    unsafe { memcpy(ptr.byte_add(offset), value, value_size) };
}
