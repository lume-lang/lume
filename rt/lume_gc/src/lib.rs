#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::cast_ptr_alignment)]

pub mod alloc;
pub(crate) mod arch;

use alloc::with_allocator;
use std::cmp::Ordering;
use std::fmt::Display;
use std::sync::LazyLock;

use lume_rt_metadata::TypeMetadata;

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

unsafe extern "C" {
    /// Static reference to the `__STACK_MAPS` symbol.
    ///
    /// See `CraneliftBackend::declare_stack_maps`.
    #[link_name = "__STACK_MAPS"]
    static __STACK_MAPS: u8;
}

/// Declares the stack maps for all generated functions in the runtime.
///
/// This function should only ever be executed *once* at startup, since it's
/// somewhat slow.
pub fn declare_stack_maps() -> Vec<CompiledFunctionMetadata> {
    let ptr = &raw const __STACK_MAPS;
    let mut offset = 0;

    let read_u64 = |offset: &mut usize| -> u64 {
        let val = unsafe { ptr.byte_add(*offset).cast::<u64>().read() };
        *offset += 8;

        val
    };

    let nfuncs = read_u64(&mut offset) as usize;
    let mut metadata = Vec::with_capacity(nfuncs);

    for _ in 0..nfuncs {
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
        trigger_collection_force();
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

    with_allocator(|alloc| alloc.promote_allocations(&frame));
}

/// Static version of [`alloc::GenerationalAllocator::alloc`], so it can be
/// used as a function pointer in Cranelift.
#[unsafe(export_name = "std::mem::GC::alloc")]
pub fn allocate_object(size: usize, metadata: *const TypeMetadata) -> *mut u8 {
    let Some(frame) = find_current_stack_map() else {
        panic!("bug!: could not find stack map for allocation call");
    };

    with_allocator(|alloc| alloc.alloc(size, metadata, &frame))
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
