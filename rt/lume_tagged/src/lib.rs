#![allow(clippy::cast_possible_truncation)]

/// Mask for the tag bits.
#[cfg(target_pointer_width = "32")]
pub const TAG_MASK: usize = 0b0011;

/// Mask for the tag bits.
#[cfg(target_pointer_width = "64")]
pub const TAG_MASK: usize = 0b0111;

/* Flags for tagged pointers. */
/// Pointer is an allocated object pointer.
pub const TF_OBJECT: u8 = 1 << 1;
/// Object has a drop method attached. Only set if [`TF_OBJECT`] is also set.
pub const TF_DROP: u8 = 1 << 0;

/// Gets the tags from the given pointer.
#[inline]
pub fn tags_of<T>(ptr: *const T) -> u8 {
    (ptr.addr() & TAG_MASK) as u8
}

/// Strips the tags from the given pointer.
#[inline]
pub fn strip_tags<T>(ptr: *mut T) -> *mut T {
    ptr.map_addr(|a| a & !TAG_MASK)
}

/// Creates a tagged object pointer with the given tags.
#[inline]
pub fn tagged_ptr<T>(ptr: *mut T, has_dropper: bool) -> *mut T {
    debug_assert_eq!(tags_of(ptr), 0);

    ptr.map_addr(|mut addr| {
        addr |= TF_OBJECT as usize;

        if has_dropper {
            addr |= TF_DROP as usize;
        }

        addr
    })
}
