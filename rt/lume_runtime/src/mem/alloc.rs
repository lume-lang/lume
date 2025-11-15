use lume_rt_metadata::TypeMetadata;

unsafe extern "C" {
    pub fn malloc(size: usize) -> *mut u8;
    pub fn realloc(ptr: *mut u8, new_size: usize) -> *mut u8;
    pub fn free(ptr: *mut u8);
}

/// Allocates the given amount of bytes using the global allocator.
///
/// The returned memory block may be larger than the requested size and
/// it may not have it's content initialized and/or zeroed.
#[unsafe(export_name = "std::mem::alloc")]
#[expect(clippy::missing_safety_doc, reason = "early alpha stage")]
pub unsafe extern "C" fn lumert_alloc(size: usize, _: *const TypeMetadata) -> *mut u8 {
    unsafe { malloc(size) }
}

/// Reallocates an existing pointer, so that the new memory block is at
/// least the given length in bytes.
///
/// The returned memory block may be larger than the requested size. The content
/// is guaranteed to be identical to the original memory block, up until the
/// original memory block size.
#[unsafe(export_name = "std::mem::realloc")]
#[expect(clippy::missing_safety_doc, reason = "early alpha stage")]
pub unsafe extern "C" fn lumert_realloc(ptr: *mut u8, new_size: usize, _: *const TypeMetadata) -> *mut u8 {
    unsafe { realloc(ptr, new_size) }
}

/// Deallocates the memory block pointed to by `ptr`, so that it can be used
/// for future allocations.
#[unsafe(export_name = "std::mem::dealloc")]
#[expect(clippy::missing_safety_doc, reason = "early alpha stage")]
pub unsafe extern "C" fn lumert_dealloc(ptr: *mut u8, _: *const TypeMetadata) {
    unsafe { free(ptr) }
}
