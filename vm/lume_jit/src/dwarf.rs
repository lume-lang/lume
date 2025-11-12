use std::collections::HashMap;
use std::ops::Deref;
use std::sync::Mutex;

use cranelift::codegen::ir::Endianness;
use cranelift::prelude::isa::TargetIsa;
use cranelift_codegen::{Final, MachSrcLoc};
use gimli::write::*;
use gimli::{DwLang, Encoding, LineEncoding, Register, RunTimeEndian};
use indexmap::IndexMap;
use lume_errors::{MapDiagnostic, Result};
use lume_mir::{Function, ModuleMap};
use lume_span::source::Location;
use lume_span::{NodeId, SourceFileId};
use object::write::{Object, StandardSection, StandardSegment, Symbol, SymbolSection};
use object::{BinaryFormat, NativeEndian, SectionKind, SymbolKind, SymbolScope};

use crate::{CraneliftBackend, FunctionMetadata};

/// DWARF identifier for the Lume language
pub const DW_LANG_LUME: DwLang = DwLang(0xA8D8_u16);

/// Returns the content of the "producer" tag (`DW_AT_producer`) in the
/// resulting DWARF debug info unit.
pub(crate) fn producer() -> String {
    format!(
        "lumec v{}, cranelift v{}",
        env!("CARGO_PKG_VERSION"),
        cranelift::VERSION
    )
}

/// Context for creating DWARF debug info, which is defined
/// on the compilation unit itself, i.e. related to the package as
/// a whole.
pub(crate) struct RootDebugContext<'ctx> {
    ctx: &'ctx ModuleMap,
    dwarf: Dwarf,
    encoding: Encoding,
    endianess: RunTimeEndian,
    stack_register: Register,

    file_units: IndexMap<SourceFileId, UnitId>,
    func_entries: IndexMap<NodeId, UnitEntryId>,
    func_mach_src: IndexMap<NodeId, Vec<MachSrcLoc<Final>>>,
    source_locations: IndexMap<SourceFileId, FileId>,
}

impl<'ctx> RootDebugContext<'ctx> {
    pub(crate) fn new(ctx: &'ctx ModuleMap, isa: &dyn TargetIsa) -> Self {
        let encoding = Encoding {
            format: gimli::Format::Dwarf32,
            version: 5,
            address_size: isa.frontend_config().pointer_bytes(),
        };

        let dwarf = Dwarf::new();

        let endianess = match isa.endianness() {
            Endianness::Big => RunTimeEndian::Big,
            Endianness::Little => RunTimeEndian::Little,
        };

        let stack_register = match isa.triple().architecture {
            target_lexicon::Architecture::Aarch64(_) => gimli::AArch64::SP,
            target_lexicon::Architecture::Riscv64(_) => gimli::RiscV::SP,
            target_lexicon::Architecture::X86_64 | target_lexicon::Architecture::X86_64h => gimli::X86_64::RSP,
            arch => panic!("unsupported ISA archicture: {arch}"),
        };

        let mut debug_ctx = Self {
            ctx,
            dwarf,
            encoding,
            endianess,
            stack_register,
            file_units: IndexMap::new(),
            func_entries: IndexMap::new(),
            func_mach_src: IndexMap::new(),
            source_locations: IndexMap::new(),
        };

        debug_ctx.create_compile_units();

        debug_ctx
    }

    /// Creates compile units for all files within the compilation context.
    fn create_compile_units(&mut self) {
        for (file, _) in self.ctx.group_by_file() {
            // Define line program
            let line_strings = &mut self.dwarf.line_strings;
            let file_name = file.name.to_pathbuf().file_name().unwrap();

            let working_dir = LineString::new(self.ctx.package.path.display().to_string(), self.encoding, line_strings);
            let source_file = LineString::new(file_name.to_str().unwrap(), self.encoding, line_strings);

            let line_program = LineProgram::new(
                self.encoding,
                LineEncoding::default(),
                working_dir,
                None,
                source_file,
                None,
            );

            let unit_id = self.dwarf.units.add(Unit::new(self.encoding, line_program));
            let unit = self.dwarf.units.get_mut(unit_id);
            let entry = unit.get_mut(unit.root());

            let file_name = file.name.to_pathbuf().file_name().unwrap();

            // DW_AT_language
            entry.set(gimli::DW_AT_language, AttributeValue::Language(DW_LANG_LUME));

            // DW_AT_producer
            let producter_str = self.dwarf.strings.add(producer());
            entry.set(gimli::DW_AT_producer, AttributeValue::StringRef(producter_str));

            // DW_AT_name
            let name = self.dwarf.strings.add(file_name.to_str().unwrap());
            entry.set(gimli::DW_AT_name, AttributeValue::StringRef(name));

            // DW_AT_comp_dir
            let comp_dir = self.dwarf.strings.add(self.ctx.package.path.display().to_string());
            entry.set(gimli::DW_AT_comp_dir, AttributeValue::StringRef(comp_dir));

            // DW_AT_stmt_list
            entry.set(gimli::DW_AT_stmt_list, AttributeValue::LineProgramRef);

            self.file_units.insert(file.id, unit_id);
        }
    }

    /// Declares the initial debug information for the given function, so the
    /// layout of the DWARF tag is laid out. Some fields may be unset.
    pub(crate) fn declare_function(&mut self, func: &Function) {
        let compile_unit_id = *self.file_units.get(&func.location.file.id).unwrap();
        let compile_unit = self.dwarf.units.get_mut(compile_unit_id);

        let entry_id = compile_unit.add(compile_unit.root(), gimli::DW_TAG_subprogram);
        let entry = compile_unit.get_mut(entry_id);

        // DW_AT_name
        let name = self.dwarf.strings.add(func.name.clone());
        entry.set(gimli::DW_AT_name, AttributeValue::StringRef(name));

        // DW_AT_external
        entry.set(gimli::DW_AT_external, AttributeValue::Flag(func.signature.external));

        // DW_AT_calling_convention
        entry.set(
            gimli::DW_AT_calling_convention,
            AttributeValue::CallingConvention(gimli::DW_CC_normal),
        );

        // DW_AT_frame_base
        let mut frame_base_expr = Expression::new();
        frame_base_expr.op_reg(self.stack_register);
        entry.set(gimli::DW_AT_frame_base, AttributeValue::Exprloc(frame_base_expr));

        // Will be replaced after the function has been defined.
        entry.set(gimli::DW_AT_low_pc, AttributeValue::Udata(0));
        entry.set(gimli::DW_AT_high_pc, AttributeValue::Udata(0));

        self.func_entries.insert(func.id, entry_id);
    }

    /// Retrieves the source locations from the given function and places them
    /// into the DWARF unit.
    pub(crate) fn define_function(&mut self, func_id: NodeId, ctx: &cranelift::codegen::Context) {
        let mcr = ctx.compiled_code().unwrap();
        let mach_loc = mcr.buffer.get_srclocs_sorted().to_vec();

        self.func_mach_src.insert(func_id, mach_loc);
    }

    /// Populates all the function units in the DWARF unit with correct function
    /// addresses, as well as building a valid line program.
    fn populate_function_units(
        &mut self,
        backend: &CraneliftBackend,
        function_metadata: &HashMap<NodeId, FunctionMetadata>,
    ) -> Result<()> {
        for (file, functions) in self.ctx.group_by_file() {
            let compile_unit_id = *self.file_units.get(&file.id).unwrap();

            let mut ranges = Vec::new();

            for func in functions {
                let Some(entry_id) = self.func_entries.get(&func.id).copied() else {
                    continue;
                };

                let func_decl = backend.declared_funcs.get(&func.id).unwrap();
                let metadata = function_metadata.get(&func.id).unwrap();
                let func_size = metadata.total_size;

                let func_start = backend.module().get_finalized_function(func_decl.id);
                let func_end = unsafe { func_start.byte_add(func_size) };

                let func_start_addr = Address::Constant(func_start.addr() as u64);

                let compile_unit = self.dwarf.units.get_mut(compile_unit_id);
                let entry = compile_unit.get_mut(entry_id);
                entry.set(gimli::DW_AT_low_pc, AttributeValue::Address(func_start_addr));
                entry.set(gimli::DW_AT_high_pc, AttributeValue::Udata(func_size as u64));

                ranges.push(Range::StartLength {
                    begin: func_start_addr,
                    length: func_size as u64,
                });

                compile_unit.line_program.begin_sequence(Some(func_start_addr));

                for MachSrcLoc { start, loc, .. } in self.func_mach_src.swap_remove(&func.id).unwrap() {
                    let compile_unit = self.dwarf.units.get_mut(compile_unit_id);
                    compile_unit.line_program.row().address_offset = u64::from(start);

                    let location = if !loc.is_default() {
                        backend.lookup_source_loc(loc)
                    } else {
                        self.ctx.function(func.id).location.clone()
                    };

                    let (file_id, line, _) = self.get_source_span(location, compile_unit_id);

                    let compile_unit = self.dwarf.units.get_mut(compile_unit_id);
                    compile_unit.line_program.row().file = file_id;
                    compile_unit.line_program.row().line = line as u64;
                    compile_unit.line_program.row().column = 1;
                    compile_unit.line_program.generate_row();
                }

                let compile_unit = self.dwarf.units.get_mut(compile_unit_id);
                compile_unit.line_program.end_sequence(func_end.addr() as u64);

                // DW_AT_decl_*
                let (file_id, line, column) = self.get_source_span(func.location.clone(), compile_unit_id);

                let compile_unit = self.dwarf.units.get_mut(compile_unit_id);
                let entry = compile_unit.get_mut(entry_id);
                entry.set(gimli::DW_AT_decl_file, AttributeValue::FileIndex(Some(file_id)));
                entry.set(gimli::DW_AT_decl_line, AttributeValue::Udata(line as u64));
                entry.set(gimli::DW_AT_decl_column, AttributeValue::Udata(column as u64));
            }

            let compile_unit = self.dwarf.units.get_mut(compile_unit_id);
            let range_list_id = compile_unit.ranges.add(RangeList(ranges));

            let root = compile_unit.get_mut(compile_unit.root());
            root.set(gimli::DW_AT_ranges, AttributeValue::RangeListRef(range_list_id));
        }

        Ok(())
    }

    /// Finish building the final DWARF debug binary and registers it via the
    /// GDB/LLDB JIT interface descriptor, making it available when debugging
    /// the binary.
    pub fn finish(
        mut self,
        backend: &CraneliftBackend,
        function_metadata: &HashMap<NodeId, FunctionMetadata>,
    ) -> Result<()> {
        self.populate_function_units(backend, function_metadata)?;

        let endian = match self.endianess {
            RunTimeEndian::Big => object::Endianness::Big,
            RunTimeEndian::Little => object::Endianness::Little,
        };

        let (bytes_ptr, bytes_len) = self.get_compiled_region(backend, function_metadata);

        let mut object = Object::new(BinaryFormat::Elf, object::Architecture::Aarch64, endian);
        let text_id = object.section_id(StandardSection::Text);

        for node_id in self.func_entries.keys() {
            let func_decl = backend.declared_funcs.get(node_id).unwrap();
            let func_start = backend.module().get_finalized_function(func_decl.id);

            let offset = func_start.addr() - bytes_ptr.addr();
            let size = function_metadata.get(node_id).unwrap().total_size;

            object.add_symbol(Symbol {
                name: func_decl.name.as_bytes().to_vec(),
                value: offset as u64,
                size: size as u64,
                kind: SymbolKind::Text,
                scope: SymbolScope::Dynamic,
                weak: false,
                section: SymbolSection::Section(text_id),
                flags: object::SymbolFlags::None,
            });
        }

        let mut sections = Sections::new(EndianVec::<RunTimeEndian>::new(self.endianess));
        self.dwarf.write(&mut sections).unwrap();

        sections
            .for_each_mut(|id, section| {
                let name = id.name().as_bytes().to_vec();
                let debug_id = object.segment_name(StandardSegment::Debug);

                if !section.slice().is_empty() {
                    let data = section.take().to_vec();

                    let section_id = object.add_section(debug_id.to_vec(), name, SectionKind::Debug);
                    object.append_section_data(section_id, &data, 8);
                }

                gimli::write::Result::Ok(())
            })
            .map_diagnostic()?;

        let bytes = object.write().unwrap();
        let symfile_addr = Box::leak(bytes.into_boxed_slice());

        patch_binary_file(symfile_addr, bytes_ptr, bytes_len)?;
        register_jit_code(symfile_addr);

        Ok(())
    }

    /// Gets the source span of the given [`Location`], from within the given
    /// compilation unit.
    fn get_source_span(&mut self, loc: Location, unit: UnitId) -> (FileId, usize, usize) {
        let (line, column) = loc.coordinates();
        let file_id = self.add_source_file(loc, unit);

        (file_id, line + 1, column + 1)
    }

    /// Gets the [`FileId`] which corresponds to the file associated with the
    /// given [`Location`]. If no [`FileId`] exists for the given [`Location`],
    /// a new one is created and returned.
    fn add_source_file(&mut self, loc: Location, unit: UnitId) -> FileId {
        *self.source_locations.entry(loc.file.id).or_insert_with(|| {
            let line_program: &mut LineProgram = &mut self.dwarf.units.get_mut(unit).line_program;
            let line_strings: &mut LineStringTable = &mut self.dwarf.line_strings;

            let encoding = line_program.encoding();

            let file_info = if self.ctx.options.debug_info.embed_sources() {
                let mut file_info = FileInfo::default();
                file_info.source = Some(LineString::String(loc.file.content.as_bytes().to_vec()));

                Some(file_info)
            } else {
                None
            };

            match &loc.file.name {
                lume_span::FileName::Real(path) => {
                    let absolute_path = self.ctx.package.root().join(path);

                    let dir_name = absolute_path
                        .parent()
                        .map(|p| p.as_os_str().to_string_lossy().as_bytes().to_vec())
                        .unwrap_or_default();

                    let file_name = absolute_path
                        .file_name()
                        .map(|p| p.to_string_lossy().as_bytes().to_vec())
                        .unwrap_or_default();

                    let dir_id = if !dir_name.is_empty() {
                        line_program.add_directory(LineString::new(dir_name, encoding, line_strings))
                    } else {
                        line_program.default_directory()
                    };

                    let file_name = LineString::new(file_name, encoding, line_strings);
                    line_program.add_file(file_name, dir_id, file_info)
                }
                lume_span::FileName::StandardLibrary(path) => {
                    let file_name = path.to_string_lossy().as_bytes().to_vec();

                    let dir_id = line_program.add_directory(LineString::new(
                        "/<stddir>/",
                        line_program.encoding(),
                        line_strings,
                    ));

                    let file_name = LineString::new(file_name, line_program.encoding(), line_strings);
                    line_program.add_file(file_name, dir_id, file_info)
                }
                lume_span::FileName::Internal => {
                    let dir_id = line_program.default_directory();
                    let dummy_file_name = LineString::new("<internal>", line_program.encoding(), line_strings);

                    line_program.add_file(dummy_file_name, dir_id, file_info)
                }
            }
        })
    }

    /// Gets the pointer to the span which contains all the JIT-compiled
    /// functions from Cranelift.
    fn get_compiled_region(
        &self,
        backend: &CraneliftBackend,
        function_metadata: &HashMap<NodeId, FunctionMetadata>,
    ) -> (*const u8, usize) {
        let mut func_spans = HashMap::new();

        for node_id in self.func_entries.keys() {
            let func_decl = backend.declared_funcs.get(node_id).unwrap();
            let metadata = function_metadata.get(node_id).unwrap();

            let func_start = backend.module().get_finalized_function(func_decl.id);
            let func_end = unsafe { func_start.byte_add(metadata.total_size) };

            func_spans.insert(*node_id, func_start..func_end);
        }

        let code_start = func_spans.values().map(|r| r.start).min().unwrap_or_default();
        let code_end = func_spans.values().map(|r| r.end).max().unwrap_or_default();
        let code_size = unsafe { code_end.byte_offset_from(code_start).cast_unsigned() };

        (code_start, code_size)
    }
}

/// `object` restricts which attributes can be defined as a custom value, so we
/// manually patch the ELF binary.
///
/// This operation MUST be done in-memory and without copying the file content.
fn patch_binary_file(bytes: &mut [u8], code_start: *const u8, code_size: usize) -> Result<()> {
    use object::elf::FileHeader64;
    use object::read::elf::ElfFile;

    const SECTION_HEADER_SIZE: usize = 64;

    const TEXT_SECTION_TYPE: u32 = object::elf::SHT_PROGBITS;
    const TEXT_SECTION_FLAGS: u64 = (object::elf::SHF_ALLOC | object::elf::SHF_EXECINSTR) as u64;

    fn read_u32(bytes: &[u8], offset: usize) -> u32 {
        let slice = &bytes[offset..offset + 4];
        let arr: &[u8; 4] = slice.try_into().unwrap();

        u32::from_ne_bytes(*arr)
    }

    fn read_u64(bytes: &[u8], offset: usize) -> u64 {
        let slice = &bytes[offset..offset + 8];
        let arr: &[u8; 8] = slice.try_into().unwrap();

        u64::from_ne_bytes(*arr)
    }

    fn write_u64(bytes: &mut [u8], offset: usize, value: u64) {
        bytes[offset..offset + 8].copy_from_slice(&value.to_le_bytes());
    }

    let file = ElfFile::<FileHeader64<NativeEndian>>::parse(bytes as &[u8]).map_diagnostic()?;
    let section_num = file.elf_header().e_shnum.get(NativeEndian);
    let section_off = file.elf_header().e_shoff.get(NativeEndian);

    let mut offset = section_off as usize;

    for _ in 0..section_num {
        let sh_type = read_u32(bytes, offset + 4);
        let sh_flag = read_u64(bytes, offset + 8);

        // Attempt to determine whether this is the `.text` section without having to
        // lookup the string table.
        let is_text_section = sh_type == TEXT_SECTION_TYPE && sh_flag == TEXT_SECTION_FLAGS;

        // For the `.text` section:
        //   - set `sh_addr` to the in-memory location of the compiled functions,
        //   - set `sh_size` to the size of the compiled region in bytes.
        if is_text_section {
            // sh_addr
            write_u64(bytes, offset + 16, code_start.addr() as u64);

            // sh_size
            write_u64(bytes, offset + 32, code_size as u64);
        }
        // For all non-`.text` sections:
        //   - set `sh_addr` to the in-memory location of the ELF binary,
        //   - set `sh_flag` to `SHF_ALLOC` so debuggers will load them into memory.
        //
        // Skip the NULL section
        else if sh_type != 0 {
            let sh_offset = read_u64(bytes, offset + 24);
            let sh_abs_addr = unsafe { bytes.as_ptr().byte_add(sh_offset as usize) };

            // sh_flag
            write_u64(bytes, offset + 8, object::elf::SHF_ALLOC as u64);

            // sh_addr
            write_u64(bytes, offset + 16, sh_abs_addr.addr() as u64);
        }

        offset += SECTION_HEADER_SIZE;
    }

    Ok(())
}

#[repr(C)]
pub struct JITCodeEntry {
    pub next_entry: *mut JITCodeEntry,
    pub prev_entry: *mut JITCodeEntry,
    pub symfile_addr: *const u8,
    pub symfile_size: u64,
}

#[derive(Debug, Clone, Copy)]
pub struct GlobalPointer<T>(*mut T);

impl<T> Deref for GlobalPointer<T> {
    type Target = *mut T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

unsafe impl<T> Send for GlobalPointer<T> {}
unsafe impl<T> Sync for GlobalPointer<T> {}

#[repr(C)]
pub struct JITDescriptor {
    pub version: u32,
    pub action_flag: i32,
    pub relevant_entry: *mut JITCodeEntry,
    pub first_entry: *mut JITCodeEntry,
}

#[unsafe(no_mangle)]
#[allow(non_upper_case_globals, reason = "external object reference")]
pub static mut __jit_debug_descriptor: JITDescriptor = JITDescriptor {
    version: 1,
    action_flag: 0,
    relevant_entry: std::ptr::null_mut(),
    first_entry: std::ptr::null_mut(),
};

#[unsafe(no_mangle)]
pub extern "C" fn __jit_debug_register_code() {
    // Empty function — the debugger sets a breakpoint here.
}

static LIST: Mutex<Vec<GlobalPointer<JITCodeEntry>>> = Mutex::new(Vec::new());

fn register_jit_code(symfile_addr: &[u8]) {
    unsafe {
        let entry = Box::into_raw(Box::new(JITCodeEntry {
            next_entry: std::ptr::null_mut(),
            prev_entry: std::ptr::null_mut(),
            symfile_addr: symfile_addr.as_ptr(),
            symfile_size: symfile_addr.len() as u64,
        }));

        {
            let mut list = LIST.lock().unwrap();

            if let Some(head) = list.last() {
                (*head.0).next_entry = entry;
                (*entry).prev_entry = head.0;
            } else {
                __jit_debug_descriptor.first_entry = entry;
            }

            list.push(GlobalPointer(entry));
        }

        __jit_debug_descriptor.relevant_entry = entry;
        __jit_debug_descriptor.action_flag = 1; // JIT_REGISTER
        __jit_debug_register_code();
        __jit_debug_descriptor.action_flag = 0;
    }
}
