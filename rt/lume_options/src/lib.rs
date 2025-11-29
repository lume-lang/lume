use serde::{Deserialize, Serialize};

/// Options for configuring the runtime environment.
#[derive(Serialize, Deserialize, Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct RuntimeOptions {
    /// Determines the initial size of the heap within the GC.
    pub gc_size: GarbageCollectorSize,
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq)]
pub enum GarbageCollectorSize {
    /// Defines that the GC heap should have a fixed size, set to the given
    /// value (in bytes). This is the preferred otion for user-defined sizes,
    /// since the is explicitly defined without modification.
    Static(usize),

    /// Defines that the GC heap should have a size relative to the total amount
    /// of system memory.
    ///
    /// When using the `Rooted` option, the total size of the GC heap will be
    /// equal to the total amount of system memory, bit-shifted right by the
    /// given amount.
    ///
    /// For example, passing `6` on a system with 16GB of total system memory,
    /// the heap would allocate 256MB of memory, since
    /// `16GB >> 6 = 256MB` (`17179869184 >> 6 = 268435456`).
    Rooted(u8),
}

impl Default for GarbageCollectorSize {
    fn default() -> Self {
        Self::Rooted(6)
    }
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
