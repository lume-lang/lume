pub mod gc;

pub use gc::*;
use serde::{Deserialize, Serialize};

/// Options for configuring the runtime environment.
#[derive(Serialize, Deserialize, Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct RuntimeOptions {
    /// Determines the options for the GC
    pub gc: GarbageCollectorOptions,
}

/// Converts the given [`RuntimeOptions`] instance into a vector of encoded
/// bytes.
pub fn to_vec(options: &RuntimeOptions) -> postcard::Result<Vec<u8>> {
    postcard::to_allocvec(options)
}

/// Converts the given sequence of bytes into an instance of [`RuntimeOptions`].
pub fn from_bytes<B: AsRef<[u8]>>(s: B) -> postcard::Result<RuntimeOptions> {
    postcard::from_bytes(s.as_ref())
}

/// Reads an encoded sequence of bytes from the given pointer into an instance
/// of [`RuntimeOptions`].
pub fn from_ptr(ptr: *const u8) -> postcard::Result<RuntimeOptions> {
    let encoded_len = unsafe { ptr.cast::<u64>().read() } as usize;
    let encoded_ptr = unsafe { ptr.byte_add(size_of::<u64>()) };

    assert!(!encoded_ptr.is_null());
    assert!(encoded_ptr.is_aligned());

    let encoded_slice = unsafe { std::slice::from_raw_parts(encoded_ptr, encoded_len) };

    from_bytes(encoded_slice)
}
