use std::collections::HashMap;

use cranelift::codegen::ir::Endianness;
use cranelift::prelude::isa::TargetIsa;
use cranelift_codegen::{Final, MachSrcLoc};
use cranelift_object::ObjectProduct;
use gimli::write::*;
use gimli::{DwLang, Encoding, LineEncoding, Register, RunTimeEndian, SectionId};
use indexmap::IndexMap;
use lume_errors::Result;
use lume_mir::{Function, ModuleMap};
use lume_span::source::Location;
use lume_span::{NodeId, SourceFileId};
use object::SectionKind;
use object::write::StandardSegment;

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
            format: gimli::Format::Dwarf64,
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

            let Some(file_name) = file.name.to_pathbuf().file_name() else {
                libftrace::warning!("skipping source file {}; no file name", file.name);
                continue;
            };

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
        let Some(compile_unit_id) = self.file_units.get(&func.location.file.id).copied() else {
            return;
        };

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
    pub(crate) fn populate_function_units(
        &mut self,
        backend: &CraneliftBackend,
        function_metadata: &HashMap<NodeId, FunctionMetadata>,
    ) {
        for (file, functions) in self.ctx.group_by_file() {
            let Some(compile_unit_id) = self.file_units.get(&file.id).copied() else {
                continue;
            };

            for func in functions {
                let Some(entry_id) = self.func_entries.get(&func.id).copied() else {
                    continue;
                };

                let func_decl = backend.declared_funcs.get(&func.id).unwrap();
                let metadata = function_metadata.get(&func.id).unwrap();

                let func_addr = crate::address_for_func(func_decl.id);
                let func_size = metadata.total_size;

                let compile_unit = self.dwarf.units.get_mut(compile_unit_id);
                let entry = compile_unit.get_mut(entry_id);
                entry.set(gimli::DW_AT_low_pc, AttributeValue::Address(func_addr));
                entry.set(gimli::DW_AT_high_pc, AttributeValue::Udata(func_size as u64));

                compile_unit.line_program.begin_sequence(Some(func_addr));

                let mut range_end = 0;

                for MachSrcLoc { start, end, loc } in self.func_mach_src.swap_remove(&func.id).unwrap() {
                    let compile_unit = self.dwarf.units.get_mut(compile_unit_id);
                    compile_unit.line_program.row().address_offset = u64::from(start);

                    let location = if loc.is_default() {
                        self.ctx.function(func.id).location.clone()
                    } else {
                        backend.lookup_source_loc(loc)
                    };

                    let (file_id, line, _) = self.get_source_span(location, compile_unit_id);

                    let compile_unit = self.dwarf.units.get_mut(compile_unit_id);
                    compile_unit.line_program.row().file = file_id;
                    compile_unit.line_program.row().line = line as u64;
                    compile_unit.line_program.row().column = 1;
                    compile_unit.line_program.generate_row();

                    range_end = end;
                }

                let compile_unit = self.dwarf.units.get_mut(compile_unit_id);
                compile_unit.line_program.end_sequence(u64::from(range_end));

                // DW_AT_decl_*
                let (file_id, line, column) = self.get_source_span(func.location.clone(), compile_unit_id);

                let compile_unit = self.dwarf.units.get_mut(compile_unit_id);
                let entry = compile_unit.get_mut(entry_id);
                entry.set(gimli::DW_AT_decl_file, AttributeValue::FileIndex(Some(file_id)));
                entry.set(gimli::DW_AT_decl_line, AttributeValue::Udata(line as u64));
                entry.set(gimli::DW_AT_decl_column, AttributeValue::Udata(column as u64));
            }
        }
    }

    /// Finish building the final DWARF debugging sections in given object file,
    /// as well as adding unwind frames.
    pub fn finish(mut self, product: &mut ObjectProduct) -> Result<()> {
        let mut sections = Sections::new(WriterRelocate::new(self.endianess));
        self.dwarf.write(&mut sections).unwrap();

        sections.for_each_mut(|id, section| {
            let debug_id = product.object.segment_name(StandardSegment::Debug);
            section.write_to_section(&mut product.object, debug_id, id.name(), SectionKind::Debug);

            Result::Ok(())
        })?;

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
                Some(FileInfo {
                    source: Some(LineString::String(loc.file.content.as_bytes().to_vec())),
                    ..Default::default()
                })
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

                    let dir_id = if dir_name.is_empty() {
                        line_program.default_directory()
                    } else {
                        line_program.add_directory(LineString::new(dir_name, encoding, line_strings))
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
}

#[derive(Clone)]
pub(crate) struct DebugReloc {
    pub(crate) offset: u32,
    pub(crate) size: u8,
    pub(crate) name: DebugRelocName,
    pub(crate) addend: i64,
    pub(crate) kind: object::RelocationKind,
}

#[derive(Clone)]
#[expect(dead_code, reason = "section relocations are currently not used")]
pub(crate) enum DebugRelocName {
    Section(SectionId),
    Symbol(usize),
}

/// A [`Writer`] that collects all necessary relocations.
#[derive(Clone)]
pub(super) struct WriterRelocate {
    pub(super) relocs: Vec<DebugReloc>,
    pub(super) writer: EndianVec<RunTimeEndian>,
}

impl WriterRelocate {
    pub(super) fn new(endian: RunTimeEndian) -> Self {
        WriterRelocate {
            relocs: Vec::new(),
            writer: EndianVec::new(endian),
        }
    }

    pub(super) fn write_to_section(
        &mut self,
        dest: &mut object::write::Object,
        segment: impl AsRef<[u8]>,
        section: impl AsRef<[u8]>,
        kind: SectionKind,
    ) {
        if !self.writer.slice().is_empty() {
            let data = self.writer.take().clone();

            let section_id = dest.add_section(segment.as_ref().to_vec(), section.as_ref().to_vec(), kind);
            dest.append_section_data(section_id, &data, 8);
        }
    }
}

#[allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap)]
impl Writer for WriterRelocate {
    type Endian = RunTimeEndian;

    fn endian(&self) -> Self::Endian {
        self.writer.endian()
    }

    fn len(&self) -> usize {
        self.writer.len()
    }

    fn write(&mut self, bytes: &[u8]) -> gimli::write::Result<()> {
        self.writer.write(bytes)
    }

    fn write_at(&mut self, offset: usize, bytes: &[u8]) -> gimli::write::Result<()> {
        self.writer.write_at(offset, bytes)
    }

    fn write_address(&mut self, address: Address, size: u8) -> gimli::write::Result<()> {
        match address {
            Address::Constant(val) => self.write_udata(val, size),
            Address::Symbol { symbol, addend } => {
                self.relocs.push(DebugReloc {
                    offset: self.len() as u32,
                    size,
                    name: DebugRelocName::Symbol(symbol),
                    addend,
                    kind: object::RelocationKind::Absolute,
                });

                self.write_udata(0, size)
            }
        }
    }

    fn write_offset(&mut self, val: usize, section: SectionId, size: u8) -> gimli::write::Result<()> {
        let offset = self.len() as u32;
        self.relocs.push(DebugReloc {
            offset,
            size,
            name: DebugRelocName::Section(section),
            addend: val as i64,
            kind: object::RelocationKind::Absolute,
        });

        self.write_udata(0, size)
    }

    fn write_offset_at(&mut self, offset: usize, val: usize, section: SectionId, size: u8) -> gimli::write::Result<()> {
        self.relocs.push(DebugReloc {
            offset: offset as u32,
            size,
            name: DebugRelocName::Section(section),
            addend: val as i64,
            kind: object::RelocationKind::Absolute,
        });

        self.write_udata_at(offset, 0, size)
    }

    fn write_eh_pointer(&mut self, address: Address, eh_pe: gimli::DwEhPe, size: u8) -> gimli::write::Result<()> {
        match address {
            // Address::Constant arm copied from gimli
            Address::Constant(val) => {
                // Indirect doesn't matter here.
                let val = match eh_pe.application() {
                    gimli::DW_EH_PE_absptr => val,
                    gimli::DW_EH_PE_pcrel => {
                        // FIXME better handling of sign
                        let offset = self.len() as u64;
                        offset.wrapping_sub(val)
                    }
                    _ => return Err(gimli::write::Error::UnsupportedPointerEncoding(eh_pe)),
                };

                self.write_eh_pointer_data(val, eh_pe.format(), size)
            }
            Address::Symbol { symbol, addend } => match eh_pe.application() {
                gimli::DW_EH_PE_pcrel => {
                    let size = match eh_pe.format() {
                        gimli::DW_EH_PE_sdata4 => 4,
                        gimli::DW_EH_PE_sdata8 => 8,
                        _ => return Err(gimli::write::Error::UnsupportedPointerEncoding(eh_pe)),
                    };

                    self.relocs.push(DebugReloc {
                        offset: self.len() as u32,
                        size,
                        name: DebugRelocName::Symbol(symbol),
                        addend,
                        kind: object::RelocationKind::Relative,
                    });

                    self.write_udata(0, size)
                }
                gimli::DW_EH_PE_absptr => {
                    self.relocs.push(DebugReloc {
                        offset: self.len() as u32,
                        size,
                        name: DebugRelocName::Symbol(symbol),
                        addend,
                        kind: object::RelocationKind::Absolute,
                    });

                    self.write_udata(0, size)
                }
                _ => Err(gimli::write::Error::UnsupportedPointerEncoding(eh_pe)),
            },
        }
    }
}
