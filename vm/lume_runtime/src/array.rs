//! Lume FFI arrays.
//!
//! None of these structs are meant to be exported - they exist purely to
//! better read arguments passed to Rust from Lume.

use lume_rt_metadata::TypeMetadata;

#[repr(C)]
pub struct Array<T> {
    /// Defines all the items in the list.
    pub metadata: *const TypeMetadata,

    /// Defines the amount of items within the list.
    pub length: std::os::raw::c_ulonglong,

    /// Defines the amount of capacity within the list.
    pub capacity: std::os::raw::c_ulonglong,

    /// Defines all the items in the list.
    pub items: *const T,
}

impl<T> Array<T> {
    pub fn iter(&self) -> ArrayIterator<T> {
        ArrayIterator {
            idx: 0,
            length: self.length,
            items: self.items,
        }
    }
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
