use cranelift_module::{DataDescription, Module};
use lume_type_metadata::TypeMetadata;

use crate::cranelift::CraneliftBackend;

impl CraneliftBackend<'_> {
    pub(crate) fn declare_type_metadata(&mut self) {
        let metadata_store = std::mem::take(&mut self.context.mir.metadata);

        for metadata_entry in metadata_store.metadata.values() {
            let type_name_id = self.declare_static_data(
                &format!("{}_NAME", metadata_entry.full_name),
                metadata_entry.full_name.as_bytes(),
            );

            let mut data_ctx = DataDescription::new();
            self.module_mut().declare_data_in_data(type_name_id, &mut data_ctx);

            data_ctx.define(Self::build_metadata_buffer(metadata_entry).into_boxed_slice());
            self.declare_static_data_ctx(&metadata_entry.full_name, &data_ctx);
        }

        self.context.mir.metadata = metadata_store;
    }

    fn build_metadata_buffer(entry: &TypeMetadata) -> Vec<u8> {
        let mut memory_block = Vec::new();

        // Allocate enough space for the pointer to the name of
        // the type, which will be inserted after the buffer is built.
        memory_block.extend_from_slice(&0_usize.to_ne_bytes()[..]);

        // Type.size
        memory_block.extend_from_slice(&entry.size.to_ne_bytes()[..]);

        // Type.alignment
        memory_block.extend_from_slice(&entry.alignment.to_ne_bytes()[..]);

        // Type.type_id
        let type_id = lume_span::hash_id(&entry.type_id);
        memory_block.extend_from_slice(&type_id.to_ne_bytes()[..]);

        // Type.fields
        memory_block.extend_from_slice(&entry.fields.len().to_ne_bytes()[..]);

        // Type.methods
        memory_block.extend_from_slice(&entry.methods.len().to_ne_bytes()[..]);

        // Type.type_arguments
        memory_block.extend_from_slice(&entry.type_arguments.len().to_ne_bytes()[..]);

        memory_block
    }
}
