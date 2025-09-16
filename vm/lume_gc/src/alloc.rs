extern crate alloc;

use alloc::alloc::{GlobalAlloc, Layout, alloc, dealloc};
use indexmap::IndexMap;
use mimalloc::MiMalloc;

use std::sync::{LazyLock, RwLock};

use crate::FrameStackMap;

unsafe extern "C" {
    pub fn memcpy(dst: *mut u8, src: *mut u8, len: usize);
}

const PAGE_SIZE: usize = 0x1000;
const POINTER_ALIGNMENT: usize = std::mem::align_of::<*const ()>();

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
        debug_assert_ne!(total_size, 0);

        let layout = Layout::from_size_align(total_size, PAGE_SIZE).unwrap();
        let base = unsafe { alloc(layout) };

        if base == std::ptr::null_mut() {
            std::alloc::handle_alloc_error(layout);
        }

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
        // This is a very rough estimate, but we're assuming most allocations
        // will average out to be ~32 bytes in size. So we're just reserving
        // enough room for that amount of items in the `allocations` map.
        let alloc_size = initial_size.div_ceil(32);

        Self {
            allocator: BumpAllocator::new(initial_size),
            allocations: IndexMap::with_capacity(alloc_size),
        }
    }

    pub(crate) fn alloc(&mut self, size: usize) -> Option<*mut u8> {
        if let Some(ptr) = self.allocator.alloc(size) {
            lume_trace::trace!("[G1] allocated {size} bytes ({ptr:p})");
            self.allocations.insert(ptr, size);

            Some(ptr)
        } else {
            None
        }
    }

    /// "Clears" the generation by resetting the bump pointer, as well
    /// as clearing the set of allocations made by the allocator.
    pub(crate) fn clear(&mut self) {
        if lume_trace::enabled!(lume_trace::Level::TRACE) {
            for (alloc, size) in &self.allocations {
                lume_trace::trace!("[G1] deallocated {size} bytes ({:p})", *alloc);
            }
        }

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
        let stack_values = frame.iter_stack_value_locations().collect::<Vec<_>>();

        self.allocations.iter().filter_map(move |(alloc, _)| {
            stack_values
                .iter()
                .find(|probe| probe.1 == alloc.cast_const())
                .map(|obj| obj.0)
        })
    }
}

pub(crate) struct OldGeneration {
    allocator: MiMalloc,
    allocations: IndexMap<*mut u8, usize>,
}

impl OldGeneration {
    pub(crate) fn new() -> Self {
        Self {
            allocator: MiMalloc,
            allocations: IndexMap::new(),
        }
    }

    pub(crate) fn alloc(&mut self, size: usize) -> *mut u8 {
        let layout = Layout::from_size_align(size, POINTER_ALIGNMENT).unwrap();
        let ptr = unsafe { self.allocator.alloc(layout) };

        if ptr == std::ptr::null_mut() {
            std::alloc::handle_alloc_error(layout);
        }

        self.allocations.insert(ptr, size);

        ptr
    }

    /// Clears the generation deallocating all allocations made by the allocator.
    pub(crate) fn clear(&mut self) {
        if lume_trace::enabled!(lume_trace::Level::TRACE) {
            for (alloc, size) in &self.allocations {
                lume_trace::trace!("[G2] deallocated {size} bytes ({:p})", *alloc);
            }
        }

        for (ptr, size) in self.allocations.drain(..) {
            let layout = Layout::from_size_align(size, POINTER_ALIGNMENT).unwrap();

            unsafe { self.allocator.dealloc(ptr, layout) }
        }
    }
}

pub struct GenerationalAllocator {
    young: YoungGeneration,
    old: OldGeneration,
}

unsafe impl Send for GenerationalAllocator {}
unsafe impl Sync for GenerationalAllocator {}

impl GenerationalAllocator {
    /// Creates a new [`GenerationalAllocator`], where the initial size of the
    /// 1st generation is the given value.
    pub fn new(gen1_size: usize) -> Self {
        Self {
            young: YoungGeneration::new(gen1_size),
            old: OldGeneration::new(),
        }
    }

    /// Creates a new [`GenerationalAllocator`], where the initial size of the
    /// 1st generation is a *n*th root of the total amount of memory installed on
    /// the host system. In practice, the total amount of memory, in bytes, will
    /// be bit-shifted right *n* times.
    ///
    /// For example, passing `6` on a system with 16GB of total system memory,
    /// the 1st generation would allocate 256MB of memory, since
    /// `16GB >> 6 = 256MB` (`17179869184 >> 6 = 268435456`).
    pub fn with_root(root: u8) -> Self {
        assert!(root != 0, "root must not be 0");
        assert!(root <= 24, "dividend must be between 0 and 24");

        let total_memory = get_total_memory();
        let g1_size = total_memory.unbounded_shr(root as u32);

        lume_trace::trace!("[G1] initial memory block of {g1_size}B ({} MB)", g1_size / 1024 / 1024);

        Self::new(g1_size)
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

        lume_trace::trace!("collection triggered, 1st generation exhausted");

        // Promote all living allocations to the 2nd generation, effectively clearing
        // the entire 1st generation for new allocations.
        self.promote_allocations(frame);

        // After promotion, attempt to allocate in the 1st generation again.
        if let Some(ptr) = self.young.alloc(size) {
            return ptr;
        }

        // While it is completely expected to allocate successfully, the fallback
        // of use the 2nd generation allocator exists, in case of allocator changes.
        lume_trace::error!("warning: expected allocation to G1 after promotion, but it failed");

        self.old.alloc(size)
    }

    /// Determines whether the current allocator needs to be collected or not.
    pub(crate) fn is_collection_required(&self) -> bool {
        let mem_in_use = self.young.allocator.current_size();
        let mem_available = self.young.allocator.total_size();
        let ratio = (mem_in_use as f32) / (mem_available as f32);

        if ratio >= 0.95 {
            lume_trace::trace!("collection required, memory pressure at {}%", ratio * 100.0);

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

            lume_trace::trace!("[G1->G2] promoted {live_obj:p} (now {new_live_ptr:p})");

            // Replace the pointer on the stack with newly moved object pointer,
            // so when the function reloads the object register from the stack,
            // it'll be to the new object.
            unsafe {
                *stack_ptr.cast_mut() = new_live_ptr.cast_const();
            }
        }

        self.young.clear();
    }

    /// Drops all allocations made with the allocator, effectively resetting
    /// all the state within the allocator.
    pub(crate) fn drop_allocations(&mut self) {
        self.young.clear();
        self.old.clear();
    }
}

fn get_total_memory() -> usize {
    let mut sys = sysinfo::System::new();
    sys.refresh_memory_specifics(sysinfo::MemoryRefreshKind::nothing().with_ram());

    sys.total_memory() as usize
}

pub static GA: LazyLock<RwLock<GenerationalAllocator>> =
    LazyLock::new(|| RwLock::new(GenerationalAllocator::with_root(6)));
