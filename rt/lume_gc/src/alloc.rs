extern crate alloc;

use alloc::alloc::{GlobalAlloc, Layout, alloc, dealloc};
use std::cell::UnsafeCell;
use std::sync::OnceLock;

use indexmap::{IndexMap, IndexSet};
use lume_rt_metadata::TypeMetadata;
use mimalloc::MiMalloc;

use crate::FrameStackMap;

unsafe extern "C" {
    pub fn memcpy(dst: *mut u8, src: *mut u8, len: usize);

    #[cfg(feature = "paranoid")]
    pub fn memset(ptr: *mut u8, c: i32, n: usize);
}

pub(crate) type DropPointer = extern "C" fn(*mut u8);

const PAGE_SIZE: usize = 0x1000;
const POINTER_SIZE: usize = std::mem::size_of::<*const ()>();
const POINTER_ALIGNMENT: usize = std::mem::align_of::<*const ()>();

const BLOCK_TYPE_ID: usize = 0xB356_997D_6F32_8F57;

/// Gets the metadata pointer of the given managed object pointer.
#[inline]
fn metadata_of(obj_ptr: *const u8) -> *const TypeMetadata {
    // The metadata of the object is stored at [-0x08], so the actual start of
    // the allocation is there.
    let alloc_start = unsafe { obj_ptr.byte_sub(POINTER_SIZE) }.cast_mut();

    unsafe { alloc_start.cast::<*const TypeMetadata>().read_unaligned() }
}

/// Record of a single object reference, along with all pointers which reference
/// it.
pub(crate) struct ObjectReference {
    /// The object pointer itself.
    pub object: *const u8,

    /// Set of all pointers pointing to `object`.
    ///
    /// Pointers within this set can be from various different locations
    /// in memory, such as stack locations, fields within other objects, etc.
    pub references: IndexSet<*const *const u8>,
}

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

        if base.is_null() {
            std::alloc::handle_alloc_error(layout);
        }

        Self {
            layout,
            base,
            current: base,
        }
    }

    /// Allocates a new memory block with the given size and returns the
    /// pointer.
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
        unsafe {
            memset(self.base.cast_mut(), 0, self.layout.size());
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
    drop_list: IndexMap<*mut u8, DropPointer>,
}

impl YoungGeneration {
    pub(crate) fn new(initial_size: usize) -> Self {
        Self {
            allocator: BumpAllocator::new(initial_size),
            drop_list: IndexMap::new(),
        }
    }

    pub(crate) fn alloc(&mut self, size: usize, metadata: *const TypeMetadata) -> Option<*mut u8> {
        if let Some(ptr) = self.allocator.alloc(size) {
            libftrace::trace!("[G1] allocated {size} bytes ({ptr:p})");

            if !metadata.is_null() {
                let drop_ptr = unsafe { metadata.read() }.drop_ptr;

                if !drop_ptr.is_null() {
                    self.drop_list.insert(ptr, unsafe {
                        std::mem::transmute::<*const std::ffi::c_void, DropPointer>(drop_ptr)
                    });
                }
            }

            Some(ptr)
        } else {
            None
        }
    }

    /// Removes the object, referenced by the given pointer, from the allocator
    /// without invoking the associated drop pointer, if any.
    pub(crate) fn unregister_object(&mut self, ptr: *mut u8) {
        self.drop_list.shift_remove(&ptr);
    }

    /// "Clears" the generation by resetting the bump pointer, as well
    /// as clearing the set of allocations made by the allocator.
    pub(crate) fn clear(&mut self) {
        for (ptr, drop_ptr) in self.drop_list.drain(..) {
            drop_ptr(unsafe { ptr.byte_add(POINTER_SIZE) });
        }

        self.allocator.clear();
    }

    /// Attempts to find all GC roots found inside of the given stack map.
    /// The returned iterator will iterate over a list of pointers, which
    /// point to an item inside the current stack frame.
    ///
    /// To get the address of the underlying allocation, simply read the
    /// pointer. This is to facilitate the GC moving the underlying
    /// allocation to a different address, whereafter it can write the new
    /// address to the pointer in the stack frame.
    pub(crate) fn living_gc_roots(&self, frame: &FrameStackMap) -> impl Iterator<Item = *const *const u8> {
        frame.iter_stack_value_locations().filter_map(|(stack_ptr, obj_ptr)| {
            // Since the pointer is pointer to an object, the allocation will actually start
            // just before the given pointer, since the metadata is defined at [-0x8].
            let obj_ptr = unsafe { obj_ptr.byte_sub(POINTER_SIZE) };

            if self.is_ptr_object(obj_ptr) {
                Some(stack_ptr)
            } else {
                None
            }
        })
    }

    /// Attempts to find all GC references found inside of the given stack map.
    /// The returned iterator will iterate over a list of pointers, which
    /// point to an item inside the current stack frame.
    ///
    /// To get the address of the underlying allocation, simply read the
    /// pointer. This is to facilitate the GC moving the underlying
    /// allocation to a different address, whereafter it can write the new
    /// address to the pointer in the stack frame.
    ///
    /// This method is an expansion of [`living_gc_roots`]. It expands the
    /// returned GC roots into a tree of all GC references, which can
    /// be referenced by any existing GC reference.
    ///
    /// Objects are pushed to the iterator in the order that they're found. This
    /// means that objects closer to the roots appear earlier in the
    /// iterator, than any child objects.
    pub(crate) fn living_gc_objects(
        &self,
        frame: &FrameStackMap,
    ) -> impl DoubleEndedIterator<Item = ObjectReference> + use<> {
        let mut object_refs = IndexMap::<*const u8, ObjectReference>::new();
        let mut worklist = self.living_gc_roots(frame).collect::<IndexSet<_>>();

        while let Some(root_ptr) = worklist.pop() {
            let obj_ptr = unsafe { root_ptr.read() };

            if let Some(obj_ref) = object_refs.get_mut(&obj_ptr) {
                // We've already visited the pointer - skip it and all it's descendants.
                if !obj_ref.references.insert(root_ptr) {
                    continue;
                }
            } else {
                object_refs.insert(obj_ptr, ObjectReference {
                    object: obj_ptr,
                    references: indexmap::indexset! {
                        root_ptr
                    },
                });
            }

            let metadata = unsafe { metadata_of(obj_ptr).read() };

            // Niche handling for `std::mem::Block` types.
            //
            // Since they only represent a block of contiguous memory, a
            // pointer could theoretically exist anywhere within
            // it. Instead of checking each byte within the block, we check all long words
            // in the block, chunked by poitner size.
            if metadata.type_id.0 == BLOCK_TYPE_ID {
                let block_len = unsafe { obj_ptr.cast::<u64>().read() } as usize;
                let block_ptr = unsafe { obj_ptr.byte_add(POINTER_SIZE).cast::<*const u8>().read() };

                let mut offset = 0;

                // If the block size isn't divisible by the pointer size, we skip any
                // remaining bytes, since they couldn't fit a pointer anyway.
                while offset + POINTER_SIZE <= block_len {
                    let block_item_ptr = unsafe { block_ptr.byte_add(offset).cast::<*const u8>() };
                    let block_item = unsafe { block_item_ptr.read().byte_sub(POINTER_SIZE) };

                    if self.is_ptr_object(block_item) {
                        worklist.insert(block_item_ptr);
                    }

                    offset += POINTER_SIZE;
                }

                continue;
            }

            let mut offset = 0;

            for field_metadata_ptr in metadata.fields.items() {
                let field_ptr = unsafe { obj_ptr.byte_add(offset).cast::<*const u8>() };
                let field = unsafe { field_ptr.read() };

                if self.is_ptr_object(field) {
                    worklist.insert(field_ptr);
                }

                let field_metadata = unsafe { field_metadata_ptr.read() };
                offset += unsafe { field_metadata.ty.read() }.size;
            }
        }

        object_refs.into_values()
    }

    /// Discerns whether the given pointer is a reference to an
    /// object within the current allocator generation.
    #[inline]
    fn is_ptr_object(&self, obj_ptr: *const u8) -> bool {
        obj_ptr >= self.allocator.base && obj_ptr <= self.allocator.current
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

        if ptr.is_null() {
            std::alloc::handle_alloc_error(layout);
        }

        let drop_ptr = if metadata.is_null() {
            std::ptr::null()
        } else {
            unsafe { metadata.read() }.drop_ptr.cast::<u8>()
        };

        self.allocations.insert(ptr, (size, drop_ptr));

        ptr
    }

    /// Clears the generation deallocating all allocations made by the
    /// allocator.
    pub(crate) fn clear(&mut self) {
        #[cfg(debug_assertions)]
        #[allow(unused, reason = "iterator values will be unused when tracing is disabled")]
        for (alloc, (size, _)) in &self.allocations {
            libftrace::trace!("[G2] deallocated {size} bytes ({:p})", *alloc);
        }

        for (ptr, (size, drop_ptr)) in self.allocations.drain(..) {
            if !drop_ptr.is_null() {
                let drop_ptr: DropPointer = unsafe { std::mem::transmute(drop_ptr) };
                drop_ptr(unsafe { ptr.byte_add(POINTER_SIZE) });
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
    /// 1st generation is a *n*th root of the total amount of memory installed
    /// on the host system. In practice, the total amount of memory, in
    /// bytes, will be bit-shifted right *n* times.
    ///
    /// For example, passing `6` on a system with 16GB of total system memory,
    /// the 1st generation would allocate 256MB of memory, since
    /// `16GB >> 6 = 256MB` (`17179869184 >> 6 = 268435456`).
    pub fn with_root(root: u8) -> Self {
        assert!(root != 0, "root must not be 0");
        assert!(root <= 24, "dividend must be between 0 and 24");

        let total_memory = get_total_memory();
        let g1_size = total_memory.unbounded_shr(u32::from(root));

        libftrace::trace!("[G1] initial memory block of {g1_size}B ({} MB)", g1_size / 1024 / 1024);

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

        libftrace::trace!("collection triggered, 1st generation exhausted");

        // Promote all living allocations to the 2nd generation, effectively clearing
        // the entire 1st generation for new allocations.
        self.promote_allocations(frame);

        // After promotion, attempt to allocate in the 1st generation again.
        if let Some(ptr) = self.young.alloc(size, metadata) {
            return ptr;
        }

        // While it is completely expected to allocate successfully, the fallback
        // of use the 2nd generation allocator exists, in case of allocator changes.
        libftrace::error!("warning: expected allocation to G1 after promotion, but it failed");

        self.old.alloc(size, metadata)
    }

    /// Determines whether the current allocator needs to be collected or not.
    pub fn is_collection_required(&self) -> bool {
        let mem_in_use = self.young.allocator.current_size();
        let mem_available = self.young.allocator.total_size();

        #[allow(clippy::cast_precision_loss)]
        let ratio = (mem_in_use as f64) / (mem_available as f64);

        if ratio >= 0.95 {
            libftrace::trace!("collection required, memory pressure at {}%", ratio * 100.0);

            return true;
        }

        false
    }

    /// Promote all living allocations from the 1st generation to the 2nd
    /// generation, moving all allocations to that generation. All the objects
    /// who where not alive in the 1st generation are deallocated. After
    /// all the allocations have been handled, the 1st generation is cleared.
    pub fn promote_allocations(&mut self, frame: &FrameStackMap) {
        // Reverse the list of all objects. Object references are added in the order
        // that they're found, but we must promote child objects first, so that
        // any parent object doesn't copy the old location of the child.
        for ObjectReference { object, references } in self.young.living_gc_objects(frame).rev() {
            // The metadata of the object is stored at [-0x08], so the actual start of
            // the allocation is there.
            let alloc_start = unsafe { object.byte_sub(POINTER_SIZE) }.cast_mut();

            let metadata_ptr = metadata_of(object);
            let obj_size = unsafe { metadata_ptr.read() }.size;

            // Copy the 1st generation object to the 2nd generation.
            let new_live_ptr = self.old.alloc(obj_size, metadata_ptr);
            unsafe { memcpy(new_live_ptr, alloc_start, obj_size) };

            libftrace::trace!("[G1->G2] promoted {alloc_start:p} (now {new_live_ptr:p})");

            // Replace the pointer on the stack with newly moved object pointer,
            // so when the function reloads the object register from the stack,
            // it'll be to the new object.
            let new_object_ptr = unsafe { new_live_ptr.byte_add(POINTER_SIZE).cast_const() };

            for reference in references {
                unsafe {
                    std::ptr::write_unaligned(reference.cast_mut(), new_object_ptr);
                }
            }

            // Unregister the allocation from the 1st generation, so the value isn't
            // disposed, when calling `clear`.
            self.young.unregister_object(alloc_start);
        }

        self.young.clear();
    }

    /// Drops all allocations made with the allocator, effectively resetting
    /// all the state within the allocator.
    pub fn drop_allocations(&mut self) {
        self.young.clear();
        self.old.clear();
    }
}

#[allow(clippy::cast_possible_truncation)]
fn get_total_memory() -> usize {
    let mut sys = sysinfo::System::new();
    sys.refresh_memory_specifics(sysinfo::MemoryRefreshKind::nothing().with_ram());

    sys.total_memory() as usize
}

struct Global<T> {
    inner: UnsafeCell<T>,
}

unsafe impl<T> Sync for Global<T> where T: Send {}

static GA: OnceLock<Global<GenerationalAllocator>> = OnceLock::new();

pub fn initialize_gc(opts: &lume_options::RuntimeOptions) {
    let allocator = match opts.gc.heap_size {
        lume_options::GarbageCollectorSize::Static(size) => GenerationalAllocator::new(size),
        lume_options::GarbageCollectorSize::Rooted(root) => GenerationalAllocator::with_root(root),
    };

    let _ = GA.set(Global {
        inner: UnsafeCell::new(allocator),
    });
}

pub fn with_allocator<R, F: FnOnce(&mut GenerationalAllocator) -> R>(f: F) -> R {
    let global = GA.get().expect("expected GC to be initialized!");

    unsafe { f(&mut *global.inner.get()) }
}
