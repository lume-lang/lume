unsafe extern "C" {
    pub fn malloc(size: usize) -> *mut u8;
    pub fn aligned_alloc(align: usize, size: usize) -> *mut u8;
    pub fn realloc(ptr: *mut u8, new_size: usize) -> *mut u8;
}

/// Allocates the given amount of bytes using the global allocator.
///
/// The returned memory block may be larger than the requested size and
/// it may not have it's content initialized and/or zeroed.
#[expect(clippy::missing_safety_doc, reason = "early alpha stage")]
pub unsafe extern "C" fn lumert_alloc(size: usize) -> *mut u8 {
    unsafe { malloc(size) }
}

/// Allocates the given amount of bytes using the global allocator, aligned
/// to the defined alignment.
///
/// The returned memory block may be larger than the requested size and
/// it may not have it's content initialized and/or zeroed.
#[expect(clippy::missing_safety_doc, reason = "early alpha stage")]
pub unsafe extern "C" fn lumert_alloca(size: usize, align: usize) -> *mut u8 {
    unsafe { aligned_alloc(align, size) }
}

/// Reallocates an existing pointer, so that the new memory block is at
/// least the given length in bytes.
///
/// The returned memory block may be larger than the requested size. The content
/// is guaranteed to be identical to the original memory block, up until the original
/// memory block size.
#[expect(clippy::missing_safety_doc, reason = "early alpha stage")]
pub unsafe extern "C" fn lumert_realloc(ptr: *mut u8, new_size: usize) -> *mut u8 {
    unsafe { realloc(ptr, new_size) }
}
