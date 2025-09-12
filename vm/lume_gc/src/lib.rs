pub(crate) mod alloc;
pub(crate) mod arch;

use std::fmt::Display;
use std::sync::{LazyLock, OnceLock};

use alloc::GA;

/// Immutable, thread-transportable pointer type.
#[derive(Debug, Clone, Copy)]
pub struct FunctionPtr(*const u8);

impl FunctionPtr {
    pub fn new(ptr: *const u8) -> Self {
        FunctionPtr(ptr)
    }

    pub fn ptr(self) -> *const u8 {
        self.0
    }

    pub fn as_usize(self) -> usize {
        self.0.addr()
    }
}

unsafe impl Send for FunctionPtr {}
unsafe impl Sync for FunctionPtr {}

/// Stack-map for a given function.
///
/// The vector defines a list of tuples containing the offset
/// of the stack map relative to the start of the function, as well
/// as all spilled GC references at that specific address.
///
/// The spilled GC references are defined as a list of offsets,
/// relative to the stack pointer which contain a reference to a living
/// GC reference.
pub type FunctionStackMap = Vec<(usize, Vec<usize>)>;

#[derive(Debug)]
pub struct CompiledFunctionMetadata {
    pub ptr: FunctionPtr,
    pub len: usize,
    pub stack_locations: FunctionStackMap,
}

impl CompiledFunctionMetadata {
    #[inline]
    #[must_use]
    pub fn end(&self) -> *const u8 {
        unsafe { self.ptr.0.byte_add(self.len) }
    }

    #[inline]
    #[must_use]
    pub fn offset_of(&self, addr: *const u8) -> Option<usize> {
        if addr >= self.ptr.0 && addr < self.end() {
            let offset = addr.addr() - self.ptr.0.addr();

            return Some(offset);
        }

        None
    }
}

static FUNC_STACK_MAPS: OnceLock<Vec<CompiledFunctionMetadata>> = OnceLock::new();

/// Declares the stack maps for all generated functions in the runtime.
///
/// # Panics
///
/// This function **will** panic if the stack maps are declared more than once.
pub fn declare_stack_maps(stack_maps: Vec<CompiledFunctionMetadata>) {
    FUNC_STACK_MAPS
        .set(stack_maps)
        .expect("function stack maps should only be assigned once");
}

fn find_current_stack_map_of_addr(pc: *const u8) -> Option<&'static CompiledFunctionMetadata> {
    let stack_maps = FUNC_STACK_MAPS.get().expect("expected function stack map to be set");

    for stack_map in stack_maps {
        if stack_map.offset_of(pc).is_some() {
            return Some(stack_map);
        }
    }

    None
}

#[derive(Debug)]
pub(crate) struct FrameStackMap {
    pub map: &'static CompiledFunctionMetadata,
    pub frame_pointer: *const u8,
    pub program_counter: *const u8,
}

impl FrameStackMap {
    #[inline]
    pub(crate) fn offset(&self) -> usize {
        self.program_counter.addr() - self.map.ptr.as_usize()
    }

    #[inline]
    pub(crate) fn stack_pointer(&self) -> *const u8 {
        unsafe { self.frame_pointer.byte_add(arch::PARENT_SP_FROM_FP_OFFSET) }
    }

    #[inline]
    pub(crate) fn stack_offsets(&self) -> Option<&[usize]> {
        let offset = self.offset();

        self.map
            .stack_locations
            .iter()
            .find_map(|loc| if loc.0 == offset { Some(loc.1.as_slice()) } else { None })
    }

    /// Attempts to find all GC references found inside of the stack map for the current
    /// program counter. The returned iterator will iterate over a list of pointers,
    /// which point to an item inside the current stack frame.
    ///
    /// To get the address of the underlying allocation, simply read the pointer. This
    /// is to facilitate the GC moving the underlying allocation to a different address,
    /// whereafter it can write the new address to the pointer in the stack frame.
    #[inline]
    pub(crate) fn stack_locations(&self) -> Option<impl Iterator<Item = *const *const u8>> {
        self.stack_offsets().map(|offsets| {
            offsets
                .iter()
                .map(|offset| unsafe { self.stack_pointer().byte_add(*offset) } as *const *const u8)
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

fn find_current_stack_map() -> Option<FrameStackMap> {
    let mut fp = arch::read_frame_pointer();

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

/// Static version of [`GarbageCollector::trigger_collection`], so it can be
/// used as a function pointer in Cranelift.
pub fn trigger_collection() {
    GC.trigger_collection();
}

/// Static version of [`alloc::GenerationalAllocator::alloc`], so it can be
/// used as a function pointer in Cranelift.
pub fn allocate_object(size: usize) -> *mut u8 {
    let Some(frame) = find_current_stack_map() else {
        panic!("bug!: could not find stack map for allocation call");
    };

    GA.try_write().unwrap().alloc(size, &frame)
}

pub static GC: LazyLock<GarbageCollector> = LazyLock::new(|| GarbageCollector::default());

#[derive(Default)]
pub struct GarbageCollector {}

impl GarbageCollector {
    pub fn trigger_collection(&self) {}
}
