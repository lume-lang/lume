//! Lume FFI arrays.
//!
//! None of these structs are meant to be exported - they exist purely to
//! better read arguments passed to Rust from Lume.

use lume_tagged::strip_tags;

#[repr(C)]
pub struct Array<T> {
    /// Defines the amount of items within the list.
    pub length: std::os::raw::c_ulonglong,

    /// Defines all the items in the list.
    pub items: *const Block<T>,
}

impl<T> Array<T> {
    pub fn iter(&self) -> ArrayIterator<T> {
        let base = unsafe { strip_tags(self.items.cast_mut()).read() };

        ArrayIterator {
            idx: 0,
            length: self.length,
            items: strip_tags(base.ptr.cast_mut()),
        }
    }
}

#[repr(C)]
pub struct Block<T> {
    /// Defines the amount of space allocated for the block, measure in bytes.
    pub size: std::os::raw::c_ulonglong,

    /// Pointer to the start of the memory block, which exists within the heap.
    pub ptr: *const T,
}

impl<T> IntoIterator for &Array<T> {
    type IntoIter = ArrayIterator<T>;
    type Item = *const T;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

pub struct ArrayIterator<T> {
    idx: u64,
    length: u64,
    items: *const T,
}

impl<T> Iterator for ArrayIterator<T> {
    type Item = *const T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.idx >= self.length {
            None
        } else {
            #[allow(clippy::cast_possible_truncation)]
            let item: *const T = unsafe { self.items.add(self.idx as usize) };

            self.idx += 1;

            Some(item)
        }
    }
}
