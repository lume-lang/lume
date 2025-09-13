extern crate alloc;

use alloc::alloc::{Layout, alloc, dealloc};
use indexmap::IndexMap;

use std::sync::{LazyLock, RwLock};

use crate::FrameStackMap;

unsafe extern "C" {
    pub fn memcpy(dst: *mut u8, src: *mut u8, len: usize);
}

const PAGE_SIZE: usize = 0x1000;

pub(crate) struct BumpAllocator {
    /// Defines the layouf of the memory block which backs the allocator.
    layout: Layout,

    /// Defines the base of the allocation block.
    base: *const u8,

    /// Defines the address where the allocator will place the next
    /// allocation within the block.
    current: *const u8,
}

impl BumpAllocator {
    pub(crate) fn new(total_size: usize) -> Self {
        let layout = Layout::from_size_align(total_size, PAGE_SIZE).unwrap();
        let base = unsafe { alloc(layout) };

        Self {
            layout,
            base,
            current: base,
        }
    }

    /// Allocates a new memory block with the given size and returns the pointer.
    pub(crate) fn alloc(&mut self, size: usize) -> Option<*mut u8> {
        if self.current_size() + size >= self.total_size() {
            return None;
        }

        let ptr = self.current.cast_mut();
        self.current = unsafe { self.current.byte_add(size) };

        Some(ptr)
    }

    /// Resets the bump allocator pointer, allowing for new allocations to
    /// use the memory space.
    pub(crate) fn clear(&mut self) {
        self.current = self.base;

        #[cfg(feature = "paranoid")]
        {
            unsafe extern "C" {
                pub fn memset(ptr: *mut u8, c: i32, n: usize);
            }

            unsafe {
                memset(self.base.cast_mut(), 0, self.layout.size());
            }
        }
    }

    /// Gets the current mount of space being used by the allocator for
    /// object allocations.
    #[inline]
    pub(crate) fn current_size(&self) -> usize {
        self.current.addr() - self.base.addr()
    }

    /// Gets the total size allocated for the allocator.
    #[inline]
    pub(crate) fn total_size(&self) -> usize {
        self.layout.size()
    }
}

impl Drop for BumpAllocator {
    fn drop(&mut self) {
        unsafe { dealloc(self.base.cast_mut(), self.layout) };
    }
}

pub(crate) struct YoungGeneration {
    allocator: BumpAllocator,
    allocations: IndexMap<*mut u8, usize>,
}

impl YoungGeneration {
    pub(crate) fn new(initial_size: usize) -> Self {
        let alloc_size = initial_size.div_ceil(32);

        Self {
            allocator: BumpAllocator::new(initial_size),
            allocations: IndexMap::with_capacity(alloc_size),
        }
    }

    pub(crate) fn alloc(&mut self, size: usize) -> Option<*mut u8> {
        if let Some(ptr) = self.allocator.alloc(size) {
            println!("[G1] allocated {size} bytes ({ptr:p})");
            self.allocations.insert(ptr, size);

            Some(ptr)
        } else {
            None
        }
    }

    /// "Clears" the generation by resetting the bump pointer, as well
    /// as clearing the set of allocations made by the allocator.
    pub(crate) fn clear(&mut self) {
        self.allocations.clear();
        self.allocator.clear();
    }

    /// Attempts to find all GC references found inside of the given stack map. The
    /// returned iterator will iterate over a list of pointers, which point to an item
    /// inside the current stack frame.
    ///
    /// To get the address of the underlying allocation, simply read the pointer. This
    /// is to facilitate the GC moving the underlying allocation to a different address,
    /// whereafter it can write the new address to the pointer in the stack frame.
    pub(crate) fn living_gc_objects(&self, frame: &FrameStackMap) -> impl Iterator<Item = *const *const u8> {
        self.allocations.iter().filter_map(move |(alloc, _)| {
            frame
                .iter_stack_value_locations()
                .find(|probe| probe.1 == alloc.cast_const())
                .map(|obj| obj.0)
        })
    }
}

pub(crate) struct OldGeneration {
    allocator: BumpAllocator,
}

impl OldGeneration {
    pub(crate) fn new(initial_size: usize) -> Self {
        Self {
            allocator: BumpAllocator::new(initial_size),
        }
    }

    pub(crate) fn alloc(&mut self, size: usize) -> *mut u8 {
        self.allocator.alloc(size).unwrap()
    }
}

pub struct GenerationalAllocator {
    young: YoungGeneration,
    old: OldGeneration,
}

unsafe impl Send for GenerationalAllocator {}
unsafe impl Sync for GenerationalAllocator {}

impl GenerationalAllocator {
    pub fn new() -> Self {
        Self {
            young: YoungGeneration::new(1 * 1024 * 1024),
            old: OldGeneration::new(64 * 1024 * 1024),
        }
    }

    pub fn alloc(&mut self, size: usize, frame: &FrameStackMap) -> *mut u8 {
        // If the allocation request is large enough, we try to allocate
        // it directly into the 2nd generation, since it is likely going
        // to live for much longer than most smaller allocations.
        if size >= self.young.allocator.total_size() {
            return self.old.alloc(size);
        }

        // Attempt to allocate the memory inside of the 1st generation, since
        // that's where most new allocations go. If it fails, it means we're out of
        // space in the 1st generation and must trigger a collection within it.
        if let Some(ptr) = self.young.alloc(size) {
            return ptr;
        }

        println!("  [GA] Collection triggered, 1st generation exhausted");

        // Promote all living allocations to the 2nd generation, effectively clearing
        // the entire 1st generation for new allocations.
        self.promote_allocations(frame);

        // After promotion, attempt to allocate in the 1st generation again.
        if let Some(ptr) = self.young.alloc(size) {
            return ptr;
        }

        // While it is completely expected to allocate successfully, the fallback
        // of use the 2nd generation allocator exists, in case of allocator changes.
        eprintln!("warning: expected allocation to G1 after promotion, but it failed");

        self.old.alloc(size)
    }

    /// Determines whether the current allocator needs to be collected or not.
    pub(crate) fn is_collection_required(&self) -> bool {
        let mem_in_use = self.young.allocator.current_size();
        let mem_available = self.young.allocator.total_size();

        if (mem_in_use as f32) / (mem_available as f32) >= 0.95 {
            return true;
        }

        false
    }

    /// Promote all living allocations from the 1st generation to the 2nd
    /// generation, moving all allocations to that generation. All the objects
    /// who where not alive in the 1st generation are deallocated. After
    /// all the allocations have been handled, the 1st generation is cleared.
    pub(crate) fn promote_allocations(&mut self, frame: &FrameStackMap) {
        for stack_ptr in self.young.living_gc_objects(frame).collect::<Vec<_>>() {
            let live_obj = unsafe { stack_ptr.read() }.cast_mut();
            let obj_size = self.young.allocations.swap_remove(&live_obj).unwrap();

            // Copy the 1st generation object to the 2nd generation.
            let new_live_ptr = self.old.alloc(obj_size);
            unsafe { memcpy(new_live_ptr, live_obj, obj_size) };

            println!("[G1->G2] Promoted {live_obj:p} (now {new_live_ptr:p})");

            // Replace the pointer on the stack with newly moved object pointer,
            // so when the function reloads the object register from the stack,
            // it'll be to the new object.
            unsafe {
                *stack_ptr.cast_mut() = new_live_ptr.cast_const();
            }
        }

        self.young.clear();
    }
}

pub static GA: LazyLock<RwLock<GenerationalAllocator>> = LazyLock::new(|| RwLock::new(GenerationalAllocator::new()));
