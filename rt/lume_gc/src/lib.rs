#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::cast_ptr_alignment)]

pub mod alloc;
pub(crate) mod arch;

use alloc::{initialize_gc, with_allocator};
use std::cmp::Ordering;
use std::fmt::Display;
use std::sync::LazyLock;

use indexmap::{IndexMap, IndexSet};
use lume_rt_macros::link_section_data;
use lume_rt_metadata::TypeMetadata;
use lume_tagged::strip_tags;

const POINTER_SIZE: usize = std::mem::size_of::<*const ()>();
const POINTER_ALIGNMENT: usize = std::mem::align_of::<*const ()>();

unsafe extern "C" {
    #[link_name = "_L1_SP3stdN3std3mem5Block"]
    static __STD_MEM_BLOCK: u8;
}

static BLOCK_TYPE_ID: LazyLock<usize> = LazyLock::new(|| {
    let block_metadata_ptr = unsafe { (&raw const __STD_MEM_BLOCK).cast::<*const TypeMetadata>().read() };
    let block_metadata = unsafe { block_metadata_ptr.read() };

    block_metadata.type_id.0
});

/// Stack-map for a given function.
///
/// The vector defines a list of tuples containing the offset
/// of the stack map relative to the start of the function, as well
/// as all spilled GC references at that specific address.
///
/// The spilled GC references are defined as a list of offsets,
/// relative to the stack pointer which contain a reference to a living
/// GC reference.
pub type FunctionStackMap = Vec<(u64, u64, Vec<u64>)>;

/// Metadata entry for a single compiled function.
#[derive(Debug)]
pub struct CompiledFunctionMetadata {
    /// Defines the address of the first instruction in the function.
    pub start: *const u8,

    /// Defines the address of the last instruction in the function.
    pub end: *const u8,

    /// Defines a list of all stack maps found within the function,
    /// keyed by offset from [`CompiledFunctionMetadata::start`].
    pub stack_locations: FunctionStackMap,
}

impl CompiledFunctionMetadata {
    /// Gets the length of the function machine code, in bytes.
    #[inline]
    #[expect(clippy::len_without_is_empty, reason = "useless addition")]
    pub fn len(&self) -> usize {
        self.end.addr() - self.start.addr()
    }

    /// Gets the offset of the given address inside the function
    /// address interval as [`Some`], if the address exists within it.
    ///
    /// Otherwise, returns [`None`].
    #[inline]
    #[must_use]
    pub fn offset_of(&self, addr: *const u8) -> Option<usize> {
        if addr >= self.start && addr < self.end {
            let offset = addr.addr() - self.start.addr();

            return Some(offset);
        }

        None
    }

    /// Gets the [`Ordering`] of the given address, in reference to the interval
    /// of the current metadata entry. This method is used for iterating
    /// over a list of metadata entries using a binary search.
    ///
    /// The truth table for the method is as such[^note]:
    ///
    /// | Input                      | Output ([`Ordering`]) |
    /// |----------------------------|-----------------------|
    /// | `start` > `addr`           | [`Ordering::Greater`] |
    /// | `end` < `addr`             | [`Ordering::Less`]    |
    /// | `start` <= `addr` <= `end` | [`Ordering::Equal`]   |
    ///
    /// [^note]: `start` and `end` denotes the `start` and `end` field in
    /// [`CompiledFunctionMetadata`], respectively.
    #[inline]
    pub fn ordering_of(&self, addr: *const u8) -> Ordering {
        if self.start > addr {
            Ordering::Greater
        } else if addr > self.end {
            Ordering::Less
        } else {
            Ordering::Equal
        }
    }
}

unsafe impl Send for CompiledFunctionMetadata {}
unsafe impl Sync for CompiledFunctionMetadata {}

static FUNC_STACK_MAPS: LazyLock<Vec<CompiledFunctionMetadata>> = LazyLock::new(declare_stack_maps);

/// Static reference to the stack map section.
///
/// See `CraneliftBackend::declare_stack_maps`.
#[link_section_data(section = "smaps")]
pub static __LUME_STACK_MAPS: u8;

/// Declares the stack maps for all generated functions in the runtime.
///
/// This function should only ever be executed *once* at startup, since it's
/// somewhat slow.
pub fn declare_stack_maps() -> Vec<CompiledFunctionMetadata> {
    let len: usize = __LUME_STACK_MAPS_END as usize - __LUME_STACK_MAPS_START as usize;
    let mut offset = 0;

    let read_u64 = |offset: &mut usize| -> u64 {
        let val = unsafe { __LUME_STACK_MAPS_START.byte_add(*offset).cast::<u64>().read() };
        *offset += 8;

        val
    };

    let mut metadata = Vec::new();

    while len > offset {
        let addr = read_u64(&mut offset) as *const u8;
        let size = read_u64(&mut offset) as usize;
        let end = unsafe { addr.byte_add(size) };

        let nloc = read_u64(&mut offset) as usize;
        let mut stack_locations = Vec::with_capacity(nloc);

        for _ in 0..nloc {
            let start = read_u64(&mut offset);
            let size = read_u64(&mut offset);

            let noffset = read_u64(&mut offset) as usize;
            let mut stack_offsets = Vec::with_capacity(noffset);

            for _ in 0..noffset {
                stack_offsets.push(read_u64(&mut offset));
            }

            stack_locations.push((start, size, stack_offsets));
        }

        metadata.push(CompiledFunctionMetadata {
            start: addr,
            end,
            stack_locations,
        });
    }

    metadata
}

/// Attempts to find the stack map for the function, which contains the given
/// program counter address. If no function is found for the given address or if
/// no stack map is attached to the found function, returns [`None`].
///
/// # Panics
///
/// This function will panic if the stack maps have not yet been declared. To
/// declare them, use [`declare_stack_maps`].
fn find_current_stack_map_of_addr(pc: *const u8) -> Option<&'static CompiledFunctionMetadata> {
    if let Ok(idx) = FUNC_STACK_MAPS.binary_search_by(|probe| probe.ordering_of(pc)) {
        return FUNC_STACK_MAPS.get(idx);
    }

    None
}

/// Gets the metadata pointer of the given managed object pointer.
#[inline]
pub(crate) fn metadata_of(obj_ptr: *const u8) -> *const TypeMetadata {
    // The metadata of the object is stored at [-0x08], so the actual start of
    // the allocation is there.
    let alloc_start = unsafe { obj_ptr.byte_sub(POINTER_SIZE) }.cast_mut();

    unsafe { alloc_start.cast::<*const TypeMetadata>().read_unaligned() }
}

/// Represents a single stack map, corresponding to a specific
/// safepoint location within a compiled Lume function.
#[derive(Debug)]
pub struct FrameStackMap {
    pub map: &'static CompiledFunctionMetadata,
    pub frame_pointer: *const u8,
    pub program_counter: *const u8,
}

impl FrameStackMap {
    /// Creates a list of frame stack maps in ascending order.
    ///
    /// This means the first element is the first frame stack map, followed
    /// by it's parent map and so on. The iteration will stop when no parent
    /// frame can be found.
    #[inline]
    pub(crate) fn create_frame_hierarchy(&self) -> Vec<FrameStackMap> {
        let mut maps = Vec::new();
        let mut fp = self.frame_pointer.addr();

        while fp != 0 {
            let pc = unsafe { arch::return_addr_of_frame(fp) };

            if let Some(map) = find_current_stack_map_of_addr(pc as *const u8) {
                maps.push(FrameStackMap {
                    map,
                    frame_pointer: fp as *const u8,
                    program_counter: pc as *const u8,
                });
            }

            fp = unsafe { arch::parent_frame_pointer(fp) };
        }

        maps
    }

    /// Gets the offset of the stack frame from the first
    /// instruction in the associated function.
    #[inline]
    pub(crate) fn offset(&self) -> usize {
        self.program_counter.addr() - self.map.start.addr()
    }

    /// Gets the stack pointer which is associated with the frame.
    #[inline]
    pub(crate) fn stack_pointer(&self) -> *const u8 {
        unsafe { self.frame_pointer.byte_add(arch::PARENT_SP_FROM_FP_OFFSET) }
    }

    /// Gets all the stack location offsets of the current frame stack map.
    ///
    /// The returned slice will be a list of offsets relative to the stack
    /// pointer of the frame, which will contain a pointer to a GC
    /// reference.
    ///
    /// For more information, see [`stack_locations`] which will get the
    /// absolute addresses of the GC references.
    #[inline]
    pub(crate) fn stack_offsets(&self) -> &[u64] {
        let offset = self.offset();

        self.map
            .stack_locations
            .iter()
            .find_map(|(start, len, stack_offsets)| {
                if *start as usize <= offset && *start as usize + *len as usize >= offset {
                    Some(stack_offsets.as_slice())
                } else {
                    None
                }
            })
            .unwrap_or(&[])
    }

    /// Attempts to find all GC references found inside of the stack map for the
    /// current program counter. The returned iterator will iterate over a
    /// list of pointers, which point to an item inside the current stack
    /// frame.
    ///
    /// To get the address of the underlying allocation, simply read the
    /// pointer. This is to facilitate the GC moving the underlying
    /// allocation to a different address, whereafter it can write the new
    /// address to the pointer in the stack frame.
    #[inline]
    #[allow(clippy::cast_ptr_alignment, reason = "stack pointer and offsets are always aligned")]
    pub(crate) fn stack_locations(&self) -> impl Iterator<Item = *const *const u8> {
        self.stack_offsets()
            .iter()
            .map(|offset| unsafe { self.stack_pointer().byte_add(*offset as usize).cast::<*const u8>() })
    }

    /// Attempts to find all GC references found inside of the stack map for the
    /// current program counter.
    ///
    /// The returned iterator will iterate over a list of tuples. The first
    /// element in the tuple is an entry in the current stack frame
    /// containing the GC reference and the second element is a pointer to
    /// the GC reference itself.
    #[inline]
    pub(crate) fn stack_value_locations(&self) -> impl Iterator<Item = (*const *const u8, *const u8)> {
        self.stack_locations().map(|ptr| {
            let gc_ref = unsafe { ptr.read() };

            (ptr, gc_ref)
        })
    }

    /// Attempts to find all GC references found inside of the stack map for the
    /// current program counter, as well as any parent stack maps from
    /// predecessor frames.
    ///
    /// The returned iterator will iterate over a list of tuples. The first
    /// element in the tuple is an entry in the current stack frame
    /// containing the GC reference and the second element is a pointer to
    /// the GC reference itself.
    #[inline]
    pub(crate) fn iter_stack_value_locations(&self) -> impl Iterator<Item = (*const *const u8, *const u8)> {
        self.create_frame_hierarchy().into_iter().flat_map(|frame| {
            // TODO: can we rewrite this, so we don't need to collect the iterator first?
            frame.stack_value_locations().collect::<Vec<_>>()
        })
    }

    /// Attempts to find all GC references found inside of the given stack map.
    ///
    /// The returned iterator will iterate over a list of tuples. The first
    /// element in the tuple is an entry in the current stack frame
    /// containing the GC reference and the second element is a pointer to
    /// the GC reference itself.
    ///
    /// This method is an expansion of
    /// [`FrameStackMap::iter_stack_value_locations`]. It expands the
    /// returned GC roots into a tree of all GC references, which can
    /// be referenced by any existing GC reference.
    ///
    /// Objects are pushed to the iterator in the order that they're found. This
    /// means that objects closer to the roots appear earlier in the
    /// iterator, than any child objects.
    ///
    /// All object pointers within the [`ObjectReference::object`] are tagged.
    pub(crate) fn living_gc_objects(&self) -> impl DoubleEndedIterator<Item = ObjectReference> + use<> {
        let stack_pointer = self.stack_pointer();

        let mut object_refs = IndexMap::<*const u8, ObjectReference>::with_capacity(1000);
        let mut worklist = self.iter_stack_value_locations().collect::<IndexSet<_>>();

        while let Some((root_ptr, tagged_obj_ptr)) = worklist.pop() {
            if tagged_obj_ptr.is_null() {
                continue;
            }

            let untagged_obj_ptr = strip_tags(tagged_obj_ptr.cast_mut());

            // Ignore stack-allocated objects since they'll be automatically collected when
            // the frame pops.
            if untagged_obj_ptr.cast_const() >= stack_pointer {
                continue;
            }

            if let Some(obj_ref) = object_refs.get_mut(&tagged_obj_ptr) {
                // We've already visited the pointer - skip it and all it's descendants.
                if !obj_ref.references.insert(root_ptr) {
                    continue;
                }
            } else {
                object_refs.insert(tagged_obj_ptr, ObjectReference {
                    object: tagged_obj_ptr,
                    references: indexmap::indexset! {
                        root_ptr
                    },
                });
            }

            let metadata = unsafe { metadata_of(untagged_obj_ptr).read() };

            // Niche handling for `std::mem::Block` types.
            //
            // While they only represent a block of contiguous memory, they do have an
            // associated type. And since each object within the block has
            // attached type information, we can read the metadata of the first
            // item in the block to get the type of all items within the block.
            if metadata.type_id.0 == *BLOCK_TYPE_ID {
                let block_len = unsafe { untagged_obj_ptr.cast::<u64>().read() } as usize;
                if block_len == 0 {
                    continue;
                }

                let block_ptr = unsafe { untagged_obj_ptr.byte_add(POINTER_SIZE).cast::<*const u8>().read() };

                libftrace::trace!(
                    "found heap block, ptr = {untagged_obj_ptr:p}, size = 0x{block_len:X}, elemental = {:?}",
                    elemental_metadata.full_name()
                );

                let mut offset = 0;

                // Since all `Block`s hold poiners to objects, we check all long words in the
                // block, chunked by pointer size.
                while offset + POINTER_SIZE <= block_len {
                    let block_item_ptr = unsafe { block_ptr.byte_add(offset).cast::<*const u8>() };
                    let block_item = unsafe { block_item_ptr.read() };

                    offset += POINTER_SIZE;

                    if block_item.is_null() {
                        continue;
                    }

                    worklist.insert((block_item_ptr, block_item));
                }

                continue;
            }

            let mut offset = 0;

            for field_metadata in metadata.fields.items() {
                let field_type_metadata = unsafe { field_metadata.ty.read() };
                let field_ptr = unsafe { untagged_obj_ptr.byte_add(offset).cast::<*const u8>() };

                offset += field_type_metadata.size;

                // Ignore scalar fields, since scalars cannot be traced.
                if field_type_metadata.kind != lume_rt_metadata::TypeKind::Object {
                    continue;
                }

                let field_value = unsafe { field_ptr.read() };
                if field_value.is_null() {
                    continue;
                }

                worklist.insert((field_ptr, field_value));
            }
        }

        object_refs.into_values()
    }
}

impl Display for FrameStackMap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "Frame: PC={:p}, FP={:p}, SP={:p}",
            self.program_counter,
            self.frame_pointer,
            self.stack_pointer()
        ))
    }
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

/// Attempts to find a frame stack map which corresponds to the current frame
/// pointer.
///
/// If no frame stack map can be found for the current frame pointer, the
/// function iterates through all parent frames, until a frame stack map is
/// found.
///
/// If no frame stack maps are found in any parent frames, the functions returns
/// [`None`].
fn find_current_stack_map() -> Option<FrameStackMap> {
    let mut fp = arch::read_frame_pointer();

    // NOTE: We're reasonably sure that the frame pointer will be 0 when no
    //       more frames are actually present, but it might be better
    //       to compare to the frame pointer of the entry function.
    while fp != 0 {
        let pc = unsafe { arch::return_addr_of_frame(fp) };

        if let Some(map) = find_current_stack_map_of_addr(pc as *const u8) {
            return Some(FrameStackMap {
                map,
                frame_pointer: fp as *const u8,
                program_counter: pc as *const u8,
            });
        }

        fp = unsafe { arch::parent_frame_pointer(fp) };
    }

    None
}

/// Initializes the allocator with the given options.
pub fn initialize(opts: &lume_options::RuntimeOptions) {
    libftrace::debug!("initializing allocator");
    libftrace::trace!("allocator options: {opts:#?}");

    initialize_gc(opts);
}

/// Sets the collection condition for the allocator and returns the previous
/// condition.
///
/// # Examples
///
/// ```rust
/// use lume_gc::{CollectCondition, set_collection_condition};
/// use lume_options::RuntimeOptions;
///
/// // Initialize the garbage collector
/// lume_gc::initialize(&RuntimeOptions::default());
///
/// set_collection_condition(|_collector_info| false);
/// ```
pub fn set_collection_condition(predicate: CollectCondition) -> CollectCondition {
    libftrace::debug!("setting collection condition");

    with_allocator(|alloc| alloc.condition.replace(predicate))
}

/// Static version of [`alloc::GenerationalAllocator::promote_allocations`], so
/// it can be used as a function pointer in Cranelift.
///
/// This function *might* trigger a collection, depending on the current state
/// of the allocator. To force a collection, use [`trigger_collection_force`].
///
/// To see whether a collection is required, use
/// [`alloc::GenerationalAllocator::is_collection_required`].
#[unsafe(export_name = "std::mem::GC::step")]
pub fn trigger_collection() {
    if with_allocator(|alloc| alloc.is_collection_required()) {
        let Some(frame) = find_current_stack_map() else {
            panic!("bug!: could not find stack map for allocation call");
        };

        libftrace::trace!("collection triggered");

        with_allocator(|alloc| alloc.promote_allocations(&frame, alloc::PromotionReason::ConditionMet));
    }
}

/// Static version of [`alloc::GenerationalAllocator::promote_allocations`], so
/// it can be used as a function pointer in Cranelift.
///
/// This function *will* trigger a collection. To only trigger a collection if
/// necessary, use [`trigger_collection`].
///
/// To see whether a collection is required, use
/// [`alloc::GenerationalAllocator::is_collection_required`].
#[unsafe(export_name = "std::mem::GC::invoke")]
pub fn trigger_collection_force() {
    let Some(frame) = find_current_stack_map() else {
        panic!("bug!: could not find stack map for allocation call");
    };

    libftrace::trace!("collection triggered");

    with_allocator(|alloc| alloc.promote_allocations(&frame, alloc::PromotionReason::Explicit));
}

/// Static version of [`alloc::GenerationalAllocator::alloc`], so it can be
/// used as a function pointer in Cranelift.
#[unsafe(export_name = "std::mem::GC::alloc")]
pub unsafe fn allocate_object(size: usize, metadata: *const TypeMetadata) -> *mut u8 {
    let Some(frame) = find_current_stack_map() else {
        panic!("bug!: could not find stack map for allocation call");
    };

    let alloc_ptr = with_allocator(|alloc| alloc.alloc(size, metadata, &frame));
    let offset_ptr = unsafe { alloc_ptr.byte_add(POINTER_SIZE) };

    if metadata.is_null() {
        return offset_ptr;
    }

    // Write the metadata pointer into the first word of the allocation.
    //
    // Previously, this was done at the MIR level, which added alot of unnecessary
    // noise to the IR.
    unsafe { *strip_tags(alloc_ptr.cast::<*const TypeMetadata>()) = metadata };

    offset_ptr
}

/// Static version of [`alloc::GenerationalAllocator::drop_allocations`], so it
/// can be used as a function pointer in Cranelift.
///
/// This function will drop all allocations which have been made with the global
/// allocator.
#[unsafe(export_name = "std::mem::GC::finish")]
pub fn drop_allocations() {
    libftrace::trace!("dropping all allocations");

    with_allocator(|alloc| alloc.drop_allocations());
}

/// A function predicate to determine whether a collection is required.
pub type CollectCondition = fn(&CollectorInfo) -> bool;

/// Limit of memory pressure before a collection is triggered, when using the
/// default collection condition.
pub const DEFAULT_MEMORY_PRESSURE_LIMIT: f64 = 0.95;

/// Default collection condition, which will trigger a collection if the number
/// of allocations exceeds a certain threshold.
pub fn default_collect_condition(info: &CollectorInfo) -> bool {
    let mem_in_use = info.g1_current_size();
    let mem_available = info.g1_total_size();

    #[allow(clippy::cast_precision_loss)]
    let ratio = (mem_in_use as f64) / (mem_available as f64);

    if ratio >= DEFAULT_MEMORY_PRESSURE_LIMIT {
        libftrace::trace!("collection required, memory pressure at {}%", ratio * 100.0);

        return true;
    }

    false
}

/// Properties and statistics about the current garbage collector state, which
/// can be used to determine whether a collection is required.
pub struct CollectorInfo {
    _private: (),
}

impl CollectorInfo {
    /// Gets the current size of allocated memory in the young generation (G1),
    /// in bytes.
    pub fn g1_current_size(&self) -> usize {
        with_allocator(|alloc| alloc.g1_current_size())
    }

    /// Gets the total size of the memory arena in the young generation (G1), in
    /// bytes.
    pub fn g1_total_size(&self) -> usize {
        with_allocator(|alloc| alloc.g1_total_size())
    }

    /// Gets the amount of currently allocated objects in the collector.
    ///
    /// Returns a tuple containing the number of objects in the young generation
    /// (G1) and old generation (G2).
    pub fn current_object_count(&self) -> (usize, usize) {
        with_allocator(|alloc| (alloc.g1().info.object_count, alloc.g2().info.object_count))
    }
}
