extern crate alloc;

use alloc::alloc::{GlobalAlloc, Layout, alloc, dealloc};
use indexmap::IndexMap;
use lume_rt_metadata::TypeMetadata;
use mimalloc::MiMalloc;

use std::sync::{LazyLock, RwLock};

use crate::FrameStackMap;

unsafe extern "C" {
    pub fn memcpy(dst: *mut u8, src: *mut u8, len: usize);
}

pub(crate) type DropPointer = extern "C" fn(*mut u8);

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
    drop_list: Vec<(*mut u8, DropPointer)>,
}

impl YoungGeneration {
    pub(crate) fn new(initial_size: usize) -> Self {
        Self {
            allocator: BumpAllocator::new(initial_size),
            drop_list: Vec::new(),
        }
    }

    pub(crate) fn alloc(&mut self, size: usize, metadata: *const TypeMetadata) -> Option<*mut u8> {
        if let Some(ptr) = self.allocator.alloc(size) {
            lume_trace::trace!("[G1] allocated {size} bytes ({ptr:p})");

            if !metadata.is_null() {
                let drop_ptr = unsafe { metadata.read() }.drop_ptr;

                if !drop_ptr.is_null() {
                    self.drop_list.push((ptr, unsafe { std::mem::transmute(drop_ptr) }));
                }
            }

            Some(ptr)
        } else {
            None
        }
    }

    /// "Clears" the generation by resetting the bump pointer, as well
    /// as clearing the set of allocations made by the allocator.
    pub(crate) fn clear(&mut self) {
        for (ptr, drop_ptr) in self.drop_list.drain(..) {
            drop_ptr(ptr);
        }

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
        frame.iter_stack_value_locations().filter_map(|(stack_ptr, obj_ptr)| {
            if obj_ptr >= self.allocator.base && obj_ptr <= self.allocator.current {
                Some(stack_ptr)
            } else {
                None
            }
        })
    }
}

pub(crate) struct OldGeneration {
    allocator: MiMalloc,
    allocations: IndexMap<*mut u8, (usize, *const u8)>,
}

impl OldGeneration {
    pub(crate) fn new() -> Self {
        Self {
            allocator: MiMalloc,
            allocations: IndexMap::new(),
        }
    }

    pub(crate) fn alloc(&mut self, size: usize, metadata: *const TypeMetadata) -> *mut u8 {
        let layout = Layout::from_size_align(size, POINTER_ALIGNMENT).unwrap();
        let ptr = unsafe { self.allocator.alloc(layout) };

        if ptr == std::ptr::null_mut() {
            std::alloc::handle_alloc_error(layout);
        }

        let drop_ptr = if !metadata.is_null() {
            unsafe { metadata.read() }.drop_ptr as *const u8
        } else {
            std::ptr::null()
        };

        self.allocations.insert(ptr, (size, drop_ptr));

        ptr
    }

    /// Clears the generation deallocating all allocations made by the allocator.
    pub(crate) fn clear(&mut self) {
        if lume_trace::enabled!(lume_trace::Level::TRACE) {
            for (alloc, (size, _)) in &self.allocations {
                lume_trace::trace!("[G2] deallocated {size} bytes ({:p})", *alloc);
            }
        }

        for (ptr, (size, drop_ptr)) in self.allocations.drain(..) {
            if !drop_ptr.is_null() {
                let drop_ptr: DropPointer = unsafe { std::mem::transmute(drop_ptr) };
                drop_ptr(ptr);
            }

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

    pub fn alloc(&mut self, size: usize, metadata: *const TypeMetadata, frame: &FrameStackMap) -> *mut u8 {
        // If the allocation request is large enough, we try to allocate
        // it directly into the 2nd generation, since it is likely going
        // to live for much longer than most smaller allocations.
        if size >= self.young.allocator.total_size() {
            return self.old.alloc(size, metadata);
        }

        // Attempt to allocate the memory inside of the 1st generation, since
        // that's where most new allocations go. If it fails, it means we're out of
        // space in the 1st generation and must trigger a collection within it.
        if let Some(ptr) = self.young.alloc(size, metadata) {
            return ptr;
        }

        lume_trace::trace!("collection triggered, 1st generation exhausted");

        // Promote all living allocations to the 2nd generation, effectively clearing
        // the entire 1st generation for new allocations.
        self.promote_allocations(frame);

        // After promotion, attempt to allocate in the 1st generation again.
        if let Some(ptr) = self.young.alloc(size, metadata) {
            return ptr;
        }

        // While it is completely expected to allocate successfully, the fallback
        // of use the 2nd generation allocator exists, in case of allocator changes.
        lume_trace::error!("warning: expected allocation to G1 after promotion, but it failed");

        self.old.alloc(size, metadata)
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
            let metadata_ptr =
                unsafe { (live_obj.cast_const() as *const *const lume_rt_metadata::TypeMetadata).read() };
            let obj_size = unsafe { metadata_ptr.read() }.size;

            // Copy the 1st generation object to the 2nd generation.
            let new_live_ptr = self.old.alloc(obj_size, metadata_ptr);
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
