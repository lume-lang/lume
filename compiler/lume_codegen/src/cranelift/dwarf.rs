use std::collections::HashMap;

use cranelift::codegen::ir::Endianness;
use cranelift::prelude::isa::TargetIsa;
use cranelift_module::{DataId, FuncId};
use cranelift_object::ObjectProduct;
use cranelift_object::object::write::{Relocation, StandardSegment};
use cranelift_object::object::{self, BinaryFormat, RelocationEncoding, RelocationFlags, SectionKind};

use error_snippet::IntoDiagnostic;
use gimli::write::{Address, AttributeValue, DwarfUnit, EndianVec, Sections, UnitEntryId, Writer};
use gimli::{DwLang, Encoding, RunTimeEndian, SectionId};
use indexmap::IndexMap;
use lume_errors::Result;
use lume_mir::Function;

use crate::Context;

/// DWARF identifier for the Lume language
pub const DW_LANG_LUME: DwLang = DwLang(0x00D8_u16);

/// Returns the content of the "producer" tag (`DW_AT_producer`) in the
/// resulting DWARF debug info unit.
pub(crate) fn producer() -> String {
    format!(
        "lumec v{}, cranelift v{}",
        env!("CARGO_PKG_VERSION"),
        cranelift::VERSION
    )
}

/// Populates the given root DWARF unit with attributes relating
/// to the given codegen context.
pub(crate) fn populate_dwarf_unit(ctx: &Context<'_>, dwarf_unit: &mut DwarfUnit) {
    let root = dwarf_unit.unit.get_mut(dwarf_unit.unit.root());

    // DW_AT_language
    root.set(gimli::DW_AT_language, AttributeValue::Language(DW_LANG_LUME));

    // DW_AT_producer
    let producer = producer();
    let producter_str = dwarf_unit.strings.add(producer);
    root.set(gimli::DW_AT_producer, AttributeValue::StringRef(producter_str));

    // DW_AT_name
    let name = dwarf_unit.strings.add(ctx.package.name.clone());
    root.set(gimli::DW_AT_name, AttributeValue::StringRef(name));

    // DW_AT_comp_dir
    let comp_dir = dwarf_unit.strings.add(format!("{}", ctx.package.path.display()));
    root.set(gimli::DW_AT_comp_dir, AttributeValue::StringRef(comp_dir));
}

pub(super) fn address_for_func(func_id: FuncId) -> Address {
    let symbol = func_id.as_u32();
    assert!(symbol & 1 << 31 == 0);

    Address::Symbol {
        symbol: symbol as usize,
        addend: 0,
    }
}

/// Context for creating DWARF debug info, which is defined
/// on the compilation unit itself, i.e. related to the package as
/// a whole.
pub(crate) struct RootDebugContext {
    dwarf_unit: DwarfUnit,
    endianess: RunTimeEndian,

    func_entries: IndexMap<lume_mir::FunctionId, UnitEntryId>,
}

impl RootDebugContext {
    pub(crate) fn new(ctx: &Context<'_>, isa: &dyn TargetIsa) -> Self {
        let encoding = Encoding {
            format: gimli::Format::Dwarf32,
            version: 3,
            address_size: isa.frontend_config().pointer_bytes(),
        };

        let mut dwarf_unit = DwarfUnit::new(encoding);
        populate_dwarf_unit(ctx, &mut dwarf_unit);

        let endianess = match isa.endianness() {
            Endianness::Big => RunTimeEndian::Big,
            Endianness::Little => RunTimeEndian::Little,
        };

        Self {
            dwarf_unit,
            endianess,
            func_entries: IndexMap::new(),
        }
    }

    /// Declares the initial debug information for the given function, so the
    /// layout of the DWARF tag is laid out. Some fields may be unset.
    pub(crate) fn declare_function(&mut self, func: &Function) {
        let root_scope = self.dwarf_unit.unit.root();

        let entry_id = self.dwarf_unit.unit.add(root_scope, gimli::DW_TAG_subprogram);
        let entry = self.dwarf_unit.unit.get_mut(entry_id);

        // DW_AT_name
        let name = self.dwarf_unit.strings.add(func.name.clone());
        entry.set(gimli::DW_AT_name, AttributeValue::StringRef(name));

        // DW_AT_calling_convention
        entry.set(
            gimli::DW_AT_calling_convention,
            AttributeValue::CallingConvention(gimli::DW_CC_normal),
        );

        // Will be replaced after the function has been defined.
        entry.set(gimli::DW_AT_low_pc, AttributeValue::Udata(0));
        entry.set(gimli::DW_AT_high_pc, AttributeValue::Udata(0));

        self.func_entries.insert(func.id, entry_id);
    }

    /// Defines the start and size of the function to the matching DWARF tag.
    pub(crate) fn define_function(&mut self, func_id: lume_mir::FunctionId, decl_id: FuncId, obj: &ObjectProduct) {
        let Some(entry_id) = self.func_entries.get(&func_id) else {
            return;
        };

        let sym_id = obj.function_symbol(decl_id);
        let func_size = obj.object.symbol(sym_id).size;

        let entry = self.dwarf_unit.unit.get_mut(*entry_id);
        entry.set(gimli::DW_AT_low_pc, AttributeValue::Address(address_for_func(decl_id)));
        entry.set(gimli::DW_AT_high_pc, AttributeValue::Udata(func_size));
    }

    /// Emits the DWARF debug info to the given [`ObjectProduct`] from Cranelift.
    pub(crate) fn emit_to(&mut self, obj: &mut ObjectProduct) -> Result<()> {
        let mut sections = Sections::new(WriterRelocate::new(self.endianess));
        self.dwarf_unit.write(&mut sections).unwrap();

        let mut section_map = HashMap::new();

        sections
            .for_each_mut(|id, section| {
                if !section.writer.slice().is_empty() {
                    let section_id = add_debug_section(id, section.writer.take(), obj);
                    section_map.insert(id, section_id);
                }

                gimli::write::Result::Ok(())
            })
            .map_err(IntoDiagnostic::into_diagnostic)?;

        sections
            .for_each(|id, section| {
                if let Some((section_id, _)) = section_map.get(&id) {
                    for reloc in &section.relocs {
                        add_debug_reloc(obj, &section_map, *section_id, reloc);
                    }
                }

                gimli::write::Result::Ok(())
            })
            .map_err(IntoDiagnostic::into_diagnostic)?;

        Ok(())
    }
}

#[derive(Clone)]
struct DebugReloc {
    pub offset: u64,
    pub size: u8,
    pub name: DebugRelocName,
    pub addend: i64,
    pub kind: object::RelocationKind,
}

#[derive(Clone)]
enum DebugRelocName {
    Section(SectionId),
    Symbol(usize),
}

#[derive(Clone)]
struct WriterRelocate {
    pub relocs: Vec<DebugReloc>,
    pub writer: EndianVec<RunTimeEndian>,
}

impl WriterRelocate {
    fn new(endian: RunTimeEndian) -> Self {
        WriterRelocate {
            relocs: Vec::new(),
            writer: EndianVec::new(endian),
        }
    }
}

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
                    offset: self.len() as u64,
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
        #[allow(clippy::cast_possible_wrap)]
        let addend = val as i64;

        self.relocs.push(DebugReloc {
            offset: self.len() as u64,
            size,
            name: DebugRelocName::Section(section),
            addend,
            kind: object::RelocationKind::Absolute,
        });

        self.write_udata(0, size)
    }

    fn write_offset_at(&mut self, offset: usize, val: usize, section: SectionId, size: u8) -> gimli::write::Result<()> {
        #[allow(clippy::cast_possible_wrap)]
        let addend = val as i64;

        self.relocs.push(DebugReloc {
            offset: offset as u64,
            size,
            name: DebugRelocName::Section(section),
            addend,
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
                    _ => {
                        return Err(gimli::write::Error::UnsupportedPointerEncoding(eh_pe));
                    }
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
                        offset: self.len() as u64,
                        size,
                        name: DebugRelocName::Symbol(symbol),
                        addend,
                        kind: object::RelocationKind::Relative,
                    });

                    self.write_udata(0, size)
                }
                gimli::DW_EH_PE_absptr => {
                    self.relocs.push(DebugReloc {
                        offset: self.len() as u64,
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

fn add_debug_section(
    id: SectionId,
    data: Vec<u8>,
    obj: &mut ObjectProduct,
) -> (object::write::SectionId, object::write::SymbolId) {
    let name = match obj.object.format() {
        BinaryFormat::MachO => id.name().replace('.', "__").into_bytes(),
        _ => id.name().to_string().into_bytes(),
    };

    let segment = obj.object.segment_name(StandardSegment::Debug).to_vec();
    let section_id = obj.object.add_section(segment, name, SectionKind::Debug);
    obj.object.section_mut(section_id).set_data(data, 1);

    let symbol_id = obj.object.section_symbol(section_id);

    (section_id, symbol_id)
}

fn add_debug_reloc(
    obj: &mut ObjectProduct,
    section_map: &HashMap<SectionId, (object::write::SectionId, object::write::SymbolId)>,
    section_id: object::write::SectionId,
    reloc: &DebugReloc,
) {
    let (symbol, symbol_offset) = match reloc.name {
        DebugRelocName::Section(id) => (section_map.get(&id).unwrap().1, 0),
        DebugRelocName::Symbol(id) => {
            let id = id.try_into().unwrap();
            let symbol_id = if id & 1 << 31 == 0 {
                obj.function_symbol(FuncId::from_u32(id))
            } else {
                obj.data_symbol(DataId::from_u32(id & !(1 << 31)))
            };

            obj.object
                .symbol_section_and_offset(symbol_id)
                .unwrap_or((symbol_id, 0))
        }
    };

    obj.object
        .add_relocation(
            section_id,
            Relocation {
                offset: reloc.offset,
                symbol,
                flags: RelocationFlags::Generic {
                    kind: reloc.kind,
                    encoding: RelocationEncoding::Generic,
                    size: reloc.size * 8,
                },
                addend: i64::try_from(symbol_offset).unwrap() + reloc.addend,
            },
        )
        .unwrap();
}
