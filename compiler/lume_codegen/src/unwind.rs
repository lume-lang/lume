use std::sync::Arc;

use cranelift_codegen::Context;
use cranelift_codegen::ir::Endianness;
use cranelift_codegen::isa::TargetIsa;
use cranelift_codegen::isa::unwind::UnwindInfo;
use cranelift_module::{DataId, FuncId};
use cranelift_object::ObjectProduct;
use gimli::RunTimeEndian;
use gimli::write::{EhFrame, FrameTable, Writer};
use indexmap::IndexMap;
use lume_span::NodeId;
use object::write::*;

use crate::dwarf::DebugRelocName;
use crate::{CraneliftBackend, WriterRelocate};

/// Context for creating unwinding frames.
pub(crate) struct RootUnwindContext {
    isa: Arc<dyn TargetIsa>,
    endianess: RunTimeEndian,

    frame_table: FrameTable,
    unwind_info: IndexMap<NodeId, UnwindInfo>,
}

impl RootUnwindContext {
    pub(crate) fn new(isa: Arc<dyn TargetIsa>) -> Self {
        let endianess = match isa.endianness() {
            Endianness::Big => RunTimeEndian::Big,
            Endianness::Little => RunTimeEndian::Little,
        };

        Self {
            isa,
            endianess,
            frame_table: FrameTable::default(),
            unwind_info: IndexMap::new(),
        }
    }

    pub(crate) fn add_function(&mut self, func_id: NodeId, ctx: &Context) {
        // The `object` crate doesn't currently support DW_GNU_EH_PE_absptr, which macOS
        // requires for unwinding tables. In addition on arm64 it currently doesn't
        // support 32bit relocations as we currently use for the unwinding table.
        //
        // See gimli-rs/object#415 and rust-lang/rustc_codegen_cranelift#1371
        if self.isa.triple().operating_system.is_like_darwin() {
            return;
        }

        if let Some(unwind_info) = ctx
            .compiled_code()
            .unwrap()
            .create_unwind_info(self.isa.as_ref())
            .unwrap()
        {
            self.unwind_info.insert(func_id, unwind_info);
        }
    }

    /// Write the unwind sections to the given object file.
    pub fn write(&mut self, backend: &CraneliftBackend, product: &mut ObjectProduct) {
        self.populate_frame_table(backend);

        let segment = product.object.segment_name(StandardSegment::Data).to_vec();
        let section_id = product
            .object
            .add_section(segment, b".eh_frame".to_vec(), SectionKind::ReadOnlyData);

        let mut eh_frame = EhFrame::from(WriterRelocate::new(self.endianess));
        self.frame_table.write_eh_frame(&mut eh_frame).unwrap();

        // Some unwinding implementations expect a terminating "empty" length so
        // a 0 is written at the end of the table for those implementations.
        let mut endian_vec = eh_frame.0.writer;
        endian_vec.write_u32(0).unwrap();
        product.object.append_section_data(section_id, endian_vec.slice(), 1);

        for reloc in eh_frame.0.relocs {
            let symbol = match reloc.name {
                DebugRelocName::Section(_) => unreachable!(),
                DebugRelocName::Symbol(id) => {
                    let id = id.try_into().unwrap();

                    if id & 1 << 31 == 0 {
                        product.function_symbol(FuncId::from_u32(id))
                    } else {
                        product.data_symbol(DataId::from_u32(id & !(1 << 31)))
                    }
                }
            };

            product
                .object
                .add_relocation(section_id, Relocation {
                    offset: u64::from(reloc.offset),
                    symbol,
                    flags: RelocationFlags::Generic {
                        kind: reloc.kind,
                        encoding: RelocationEncoding::Generic,
                        size: reloc.size * 8,
                    },
                    addend: reloc.addend,
                })
                .unwrap();
        }
    }

    fn populate_frame_table(&mut self, backend: &CraneliftBackend) {
        let mut cie = self.isa.create_systemv_cie().unwrap();
        cie.fde_address_encoding = gimli::DwEhPe(gimli::DW_EH_PE_pcrel.0 | gimli::DW_EH_PE_sdata4.0);

        let cie_id = self.frame_table.add_cie(cie);

        for (node_id, unwind_info) in std::mem::take(&mut self.unwind_info) {
            let func_decl = backend.declared_funcs.get(&node_id).unwrap();
            let func_addr = crate::address_for_func(func_decl.id);

            match unwind_info {
                UnwindInfo::SystemV(unwind_info) => {
                    let fde = unwind_info.to_fde(func_addr);

                    self.frame_table.add_fde(cie_id, fde);
                }
                UnwindInfo::WindowsArm64(_) | UnwindInfo::WindowsX64(_) => {}
                _ => unimplemented!(),
            }
        }
    }
}
