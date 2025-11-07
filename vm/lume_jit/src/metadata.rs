use std::ops::Rem;

use cranelift_module::{DataDescription, DataId, FuncId, FuncOrDataId, Module};
use indexmap::{IndexMap, IndexSet};
use lume_type_metadata::*;

use crate::CraneliftBackend;

const NATIVE_PTR_SIZE: usize = std::mem::size_of::<*const ()>();
const NATIVE_PTR_ALIGN: usize = std::mem::align_of::<*const ()>();

struct MetadataEntries {
    pub type_metadata: String,
    pub field_metadata: String,
    pub method_metadata: String,
    pub parameter_metadata: String,
    pub type_parameter_metadata: String,
}

#[inline]
fn metadata_entry(entries: &IndexMap<TypeMetadataId, TypeMetadata>, name: &'static str) -> String {
    entries
        .values()
        .find_map(|ty| {
            if ty.full_name == name {
                Some(ty.symbol_name())
            } else {
                None
            }
        })
        .expect("expected to find metadata of `std::Type`")
}

impl CraneliftBackend {
    #[tracing::instrument(level = "DEBUG", skip(self))]
    pub(crate) fn declare_type_metadata(&mut self) {
        let metadata_store = &self.context.metadata;
        let mut found_types = IndexMap::new();

        for metadata_entry in metadata_store.metadata.values() {
            self.find_all_types_on(metadata_entry, &mut found_types);
        }

        self.declare_all_type_metadata(found_types.values());
        self.declare_all_field_metadata(found_types.values());
        self.declare_all_method_metadata(found_types.values());
        self.declare_all_parameter_metadata(found_types.values());
        self.declare_all_type_parameter_metadata(found_types.values());

        let metadata_symbols = MetadataEntries {
            type_metadata: metadata_entry(&found_types, "std::Type"),
            field_metadata: metadata_entry(&found_types, "std::Field"),
            method_metadata: metadata_entry(&found_types, "std::Method"),
            parameter_metadata: metadata_entry(&found_types, "std::Parameter"),
            type_parameter_metadata: metadata_entry(&found_types, "std::TypeParameter"),
        };

        let mut defined_methods = IndexSet::new();

        for metadata_entry in found_types.values() {
            self.build_type_metadata_buffer(metadata_entry, &metadata_symbols);

            for type_param_metadata in &metadata_entry.type_parameters {
                self.build_type_parameter_metadata_buffer(
                    type_param_metadata,
                    metadata_entry.full_name.clone(),
                    &metadata_symbols,
                );
            }

            for field_metadata in &metadata_entry.fields {
                self.build_field_metadata_buffer(field_metadata, metadata_entry, &metadata_symbols);
            }

            for method_metadata in &metadata_entry.methods {
                if defined_methods.contains(&method_metadata.func_id) {
                    continue;
                }

                defined_methods.insert(method_metadata.func_id);

                self.build_method_metadata_buffer(method_metadata, &metadata_symbols);

                for param_metadata in &method_metadata.parameters {
                    self.build_parameter_metadata_buffer(param_metadata, method_metadata, &metadata_symbols);
                }

                for type_param_metadata in &method_metadata.type_parameters {
                    self.build_type_parameter_metadata_buffer(
                        type_param_metadata,
                        method_metadata.full_name.clone(),
                        &metadata_symbols,
                    );
                }
            }
        }
    }
}

impl CraneliftBackend {
    /// Recursively finds all the types on the given metadata type. Children
    /// types can be defined within a parameter, field, type argument, etc.
    ///
    /// The `found` parameter defines which types have already been found,
    /// preventing infinite looping from recursive types.
    #[tracing::instrument(level = "TRACE", skip_all, fields(id = ?metadata.id, name = metadata.full_name))]
    fn find_all_types_on(&self, metadata: &TypeMetadata, found: &mut IndexMap<TypeMetadataId, TypeMetadata>) {
        if found.contains_key(&metadata.id) {
            return;
        }

        found.insert(metadata.id, metadata.to_owned());

        for field in &metadata.fields {
            let field_ty = self.find_metadata(field.ty);
            self.find_all_types_on(field_ty, found);
        }

        for method in &metadata.methods {
            for param in &method.parameters {
                let param_ty = self.find_metadata(param.ty);
                self.find_all_types_on(param_ty, found);
            }

            for type_param in &method.type_parameters {
                for constraint in &type_param.constraints {
                    let constraint_ty = self.find_metadata(*constraint);
                    self.find_all_types_on(constraint_ty, found);
                }
            }

            let return_ty = self.find_metadata(method.return_type);
            self.find_all_types_on(return_ty, found);
        }

        for type_arg in &metadata.type_arguments {
            let arg_ty = self.find_metadata(*type_arg);
            self.find_all_types_on(arg_ty, found);
        }
    }

    /// Finds an existing [`TypeMetadata`] instance from the given ID.
    #[inline]
    fn find_metadata(&self, id: TypeMetadataId) -> &TypeMetadata {
        self.context.metadata.metadata.get(&id).unwrap()
    }

    #[inline]
    #[allow(clippy::unused_self)]
    #[tracing::instrument(level = "TRACE", skip_all, fields(name = field.name, ty = ty.full_name))]
    fn metadata_name_of_field(&self, field: &FieldMetadata, ty: &TypeMetadata) -> String {
        let mut field_metadata_name = ty.symbol_name();
        field_metadata_name.push_str(&field.name);

        field_metadata_name
    }

    #[inline]
    #[allow(clippy::unused_self)]
    #[tracing::instrument(level = "TRACE", skip_all, fields(name = param.name, method = method.full_name))]
    fn metadata_name_of_param(&self, param: &ParameterMetadata, method: &MethodMetadata) -> String {
        let mut metadata_name = method.full_name.clone();
        metadata_name.push_str(&param.name);

        metadata_name
    }

    #[inline]
    #[allow(clippy::unused_self)]
    #[tracing::instrument(level = "TRACE", skip_all, fields(name = param.name, owner = owner_name))]
    fn metadata_name_of_type_param(&self, param: &TypeParameterMetadata, mut owner_name: String) -> String {
        owner_name.push('`');
        owner_name.push_str(&param.name);
        owner_name
    }

    /// Finds an existing data declaration for a metadata value with the given
    /// name.
    #[inline]
    #[tracing::instrument(level = "TRACE", skip(self), ret)]
    fn find_decl_by_name(&self, name: &str) -> DataId {
        let Some(FuncOrDataId::Data(data_id)) = self.module().declarations().get_name(name) else {
            panic!("bug!: metadata declaration not found: {name}");
        };

        data_id
    }

    /// Finds an existing data declaration for the type metadata with the given
    /// ID.
    #[inline]
    #[tracing::instrument(level = "TRACE", skip(self), ret)]
    fn find_type_decl(&self, id: TypeMetadataId) -> DataId {
        let metadata = self.find_metadata(id);
        let metadata_name = metadata.symbol_name();

        self.find_decl_by_name(&metadata_name)
    }

    /// Declares type metadata for the types in the given iterator, but without
    /// defining it.
    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn declare_all_type_metadata<'a>(&self, iter: impl Iterator<Item = &'a TypeMetadata>) {
        for metadata in iter {
            let metadata_name = metadata.symbol_name();
            let array_name = format!("{}__type_args", metadata.symbol_name());

            self.module_mut()
                .declare_data(&array_name, cranelift_module::Linkage::Local, false, false)
                .unwrap();

            lume_trace::debug!("declaring type metadata: {metadata_name}");

            self.module_mut()
                .declare_data(&metadata_name, cranelift_module::Linkage::Local, false, false)
                .unwrap();
        }
    }

    /// Declares the field metadata for all the types in the given iterator, but
    /// without defining it.
    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn declare_all_field_metadata<'a>(&self, iter: impl Iterator<Item = &'a TypeMetadata>) {
        for metadata in iter {
            let array_name = format!("{}__fields", metadata.symbol_name());

            self.module_mut()
                .declare_data(&array_name, cranelift_module::Linkage::Local, false, false)
                .unwrap();

            for field in &metadata.fields {
                let metadata_name = self.metadata_name_of_field(field, metadata);

                lume_trace::debug!("declaring field metadata: {metadata_name}");

                self.module_mut()
                    .declare_data(&metadata_name, cranelift_module::Linkage::Local, false, false)
                    .unwrap();
            }
        }
    }

    /// Declares the field metadata for all the types in the given iterator, but
    /// without defining it.
    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn declare_all_method_metadata<'a>(&self, iter: impl Iterator<Item = &'a TypeMetadata>) {
        for metadata in iter {
            let array_name = format!("{}__methods", metadata.symbol_name());

            self.module_mut()
                .declare_data(&array_name, cranelift_module::Linkage::Local, false, false)
                .unwrap();

            for method in &metadata.methods {
                let metadata_name = method.symbol_name();

                if self.module().declarations().get_name(&metadata_name).is_some() {
                    continue;
                }

                lume_trace::debug!("declaring method metadata: {metadata_name}");

                self.module_mut()
                    .declare_data(&metadata_name, cranelift_module::Linkage::Local, false, false)
                    .unwrap();
            }
        }
    }

    /// Declares the parameter metadata for all the types in the given iterator,
    /// but without defining it.
    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn declare_all_parameter_metadata<'a>(&self, iter: impl Iterator<Item = &'a TypeMetadata>) {
        for metadata in iter {
            for method in &metadata.methods {
                let array_name = format!("{}__params", method.symbol_name());

                self.module_mut()
                    .declare_data(&array_name, cranelift_module::Linkage::Local, false, false)
                    .unwrap();

                for param in &method.parameters {
                    let metadata_name = self.metadata_name_of_param(param, method);

                    lume_trace::debug!("declaring parameter metadata: {metadata_name}");

                    self.module_mut()
                        .declare_data(&metadata_name, cranelift_module::Linkage::Local, false, false)
                        .unwrap();
                }
            }
        }
    }

    /// Declares the type parameter metadata for all the types in the given
    /// iterator, but without defining it.
    #[tracing::instrument(level = "DEBUG", skip_all)]
    fn declare_all_type_parameter_metadata<'a>(&self, iter: impl Iterator<Item = &'a TypeMetadata>) {
        for metadata in iter {
            let array_name = format!("{}__type_params", metadata.symbol_name());

            self.module_mut()
                .declare_data(&array_name, cranelift_module::Linkage::Local, false, false)
                .unwrap();

            for type_param in &metadata.type_parameters {
                let metadata_name = self.metadata_name_of_type_param(type_param, metadata.full_name.clone());

                lume_trace::debug!("declaring type parameter metadata: {metadata_name}");

                self.module_mut()
                    .declare_data(&metadata_name, cranelift_module::Linkage::Local, false, false)
                    .unwrap();
            }

            for method in &metadata.methods {
                let array_name = format!("{}__type_params", method.symbol_name());

                self.module_mut()
                    .declare_data(&array_name, cranelift_module::Linkage::Local, false, false)
                    .unwrap();

                for type_param in &method.type_parameters {
                    let metadata_name = self.metadata_name_of_type_param(type_param, method.full_name.clone());

                    lume_trace::debug!("declaring type parameter metadata: {metadata_name}");

                    self.module_mut()
                        .declare_data(&metadata_name, cranelift_module::Linkage::Local, false, false)
                        .unwrap();
                }
            }
        }
    }
}

impl CraneliftBackend {
    #[tracing::instrument(level = "TRACE", skip(self, desc))]
    fn define_metadata(&self, data_id: DataId, name: String, desc: &DataDescription) {
        self.module_mut().define_data(data_id, desc).unwrap();
        self.static_data.write().unwrap().insert(name, data_id);
    }

    #[tracing::instrument(level = "TRACE", skip_all, fields(name = metadata.full_name))]
    fn build_type_metadata_buffer(&self, metadata: &TypeMetadata, metadata_symbols: &MetadataEntries) {
        let data_id = self.find_type_decl(metadata.id);
        let mut builder = MemoryBlockBuilder::new(self);

        // Metadata entry
        builder.append_metadata_address(&metadata_symbols.type_metadata);

        // Type.type_id
        builder.append(metadata.type_id_usize());

        // Type.name
        builder.append_str_address(metadata.full_name.clone());

        // Type.size
        builder.append(metadata.size);

        // Type.alignment
        builder.append(metadata.alignment);

        // Type.fields
        builder.append_slice_ptr_of(
            format!("{}__fields", metadata.symbol_name()),
            &metadata.fields,
            |builder, field| {
                let name = self.metadata_name_of_field(field, metadata);
                let data_id = self.find_decl_by_name(&name);

                builder.append_data_address(data_id, NATIVE_PTR_SIZE.cast_signed() as i64);
            },
        );

        // Type.methods
        builder.append_slice_ptr_of(
            format!("{}__methods", metadata.symbol_name()),
            &metadata.methods,
            |builder, method| {
                let name = method.symbol_name();
                let data_id = self.find_decl_by_name(&name);

                builder.append_data_address(data_id, NATIVE_PTR_SIZE.cast_signed() as i64);
            },
        );

        // Type.type_parameters
        builder.append_slice_ptr_of(
            format!("{}__type_params", metadata.symbol_name()),
            &metadata.type_parameters,
            |builder, type_param| {
                let name = self.metadata_name_of_type_param(type_param, metadata.full_name.clone());
                let data_id = self.find_decl_by_name(&name);

                builder.append_data_address(data_id, NATIVE_PTR_SIZE.cast_signed() as i64);
            },
        );

        // Type.type_arguments
        builder.append_slice_ptr_of(
            format!("{}__type_args", metadata.symbol_name()),
            &metadata.type_arguments,
            |builder, type_arg| {
                let data_id = self.find_type_decl(*type_arg);
                builder.append_data_address(data_id, 0);
            },
        );

        // Type.drop_ptr
        if let Some(drop_method) = metadata.drop_method {
            let drop_ptr = self.declared_funcs.get(&drop_method).unwrap();

            builder.append_func_address(drop_ptr.id);
        } else {
            builder.append_null_ptr();
        }

        self.define_metadata(data_id, metadata.full_name.clone(), &builder.finish());
    }

    #[tracing::instrument(level = "TRACE", skip_all, fields(name = metadata.name, ty = type_metadata.full_name))]
    fn build_field_metadata_buffer(
        &self,
        metadata: &FieldMetadata,
        type_metadata: &TypeMetadata,
        metadata_symbols: &MetadataEntries,
    ) {
        let metadata_name = self.metadata_name_of_field(metadata, type_metadata);
        let data_id = self.find_decl_by_name(&metadata_name);
        let mut builder = MemoryBlockBuilder::new(self);

        // Metadata entry
        builder.append_metadata_address(&metadata_symbols.field_metadata);

        // Field.name
        builder.append_str_address(metadata.name.clone());

        // Field.type
        let type_data_id = self.find_type_decl(metadata.ty);
        builder.append_data_address(type_data_id, NATIVE_PTR_SIZE.cast_signed() as i64);

        self.define_metadata(data_id, metadata_name, &builder.finish());
    }

    #[tracing::instrument(level = "TRACE", skip_all, fields(name = metadata.full_name))]
    fn build_method_metadata_buffer(&self, metadata: &MethodMetadata, metadata_symbols: &MetadataEntries) {
        let metadata_name = metadata.symbol_name();
        let data_id = self.find_decl_by_name(&metadata_name);
        let mut builder = MemoryBlockBuilder::new(self);

        // Metadata entry
        builder.append_metadata_address(&metadata_symbols.method_metadata);

        // Method.id
        builder.append(metadata.definition_id.as_usize());

        // Method.full_name
        builder.append_str_address(metadata.full_name.clone());

        // Method.parameters
        builder.append_slice_ptr_of(
            format!("{}__params", metadata.symbol_name()),
            &metadata.parameters,
            |builder, param| {
                let name = self.metadata_name_of_param(param, metadata);
                let data_id = self.find_decl_by_name(&name);

                builder.append_data_address(data_id, NATIVE_PTR_SIZE.cast_signed() as i64);
            },
        );

        // Method.type_parameters
        builder.append_slice_ptr_of(
            format!("{}__type_params", metadata.symbol_name()),
            &metadata.type_parameters,
            |builder, param| {
                let name = self.metadata_name_of_type_param(param, metadata.full_name.clone());
                let data_id = self.find_decl_by_name(&name);

                builder.append_data_address(data_id, NATIVE_PTR_SIZE.cast_signed() as i64);
            },
        );

        // Method.return_type
        let type_data_id = self.find_type_decl(metadata.return_type);
        builder.append_data_address(type_data_id, NATIVE_PTR_SIZE.cast_signed() as i64);

        // Method.func_ptr
        let func_ptr = self.declared_funcs.get(&metadata.func_id).unwrap();
        builder.append_func_address(func_ptr.id);

        self.define_metadata(data_id, metadata.full_name.clone(), &builder.finish());
    }

    #[tracing::instrument(level = "TRACE", skip_all, fields(name = metadata.name, method = method_metadata.full_name))]
    fn build_parameter_metadata_buffer(
        &self,
        metadata: &ParameterMetadata,
        method_metadata: &MethodMetadata,
        metadata_symbols: &MetadataEntries,
    ) {
        let metadata_name = self.metadata_name_of_param(metadata, method_metadata);
        let data_id = self.find_decl_by_name(&metadata_name);
        let mut builder = MemoryBlockBuilder::new(self);

        // Metadata entry
        builder.append_metadata_address(&metadata_symbols.parameter_metadata);

        // Parameter.name
        builder.append_str_address(metadata.name.clone());

        // Parameter.type
        let type_data_id = self.find_type_decl(metadata.ty);
        builder.append_data_address(type_data_id, NATIVE_PTR_SIZE.cast_signed() as i64);

        // Parameter.vararg
        builder.append_byte(u8::from(metadata.vararg));

        self.define_metadata(data_id, metadata_name, &builder.finish());
    }

    #[tracing::instrument(level = "TRACE", skip_all, fields(name = metadata.name, owner = owner_name))]
    fn build_type_parameter_metadata_buffer(
        &self,
        metadata: &TypeParameterMetadata,
        owner_name: String,
        metadata_symbols: &MetadataEntries,
    ) {
        let metadata_name = self.metadata_name_of_type_param(metadata, owner_name);
        let data_id = self.find_decl_by_name(&metadata_name);
        let mut builder = MemoryBlockBuilder::new(self);

        // Metadata entry
        builder.append_metadata_address(&metadata_symbols.type_parameter_metadata);

        // TypeParameter.name
        builder.append_str_address(metadata.name.clone());

        // TypeParameter.constraints
        builder.append_slice_of(&metadata.constraints, |builder, constraint| {
            let constraint_data_id = self.find_type_decl(*constraint);
            builder.append_data_address(constraint_data_id, NATIVE_PTR_SIZE.cast_signed() as i64);
        });

        self.define_metadata(data_id, metadata_name, &builder.finish());
    }
}

trait Encode {
    fn encode(&self) -> Box<[u8]>;
}

impl Encode for u8 {
    fn encode(&self) -> Box<[u8]> {
        vec![*self].into_boxed_slice()
    }
}

impl Encode for u16 {
    fn encode(&self) -> Box<[u8]> {
        self.to_ne_bytes().to_vec().into_boxed_slice()
    }
}

impl Encode for u32 {
    fn encode(&self) -> Box<[u8]> {
        self.to_ne_bytes().to_vec().into_boxed_slice()
    }
}

impl Encode for u64 {
    fn encode(&self) -> Box<[u8]> {
        self.to_ne_bytes().to_vec().into_boxed_slice()
    }
}

impl Encode for usize {
    fn encode(&self) -> Box<[u8]> {
        self.to_ne_bytes().to_vec().into_boxed_slice()
    }
}

struct MemoryBlockBuilder<'back> {
    backend: &'back CraneliftBackend,

    data: Vec<u8>,
    data_relocs: Vec<(usize, DataId, i64)>,
    func_relocs: Vec<(usize, FuncId)>,
    offset: usize,
}

impl<'back> MemoryBlockBuilder<'back> {
    pub fn new(backend: &'back CraneliftBackend) -> Self {
        Self {
            backend,
            data: Vec::new(),
            data_relocs: Vec::new(),
            func_relocs: Vec::new(),
            offset: 0,
        }
    }

    /// Checks whether the given value is aligned.
    #[inline]
    fn is_aligned(val: usize) -> bool {
        val.rem(NATIVE_PTR_ALIGN) == 0
    }

    /// Determines how many bytes the given value is off from being aligned.
    #[inline]
    fn unalignment_of(val: usize) -> usize {
        (NATIVE_PTR_ALIGN - val.rem(NATIVE_PTR_ALIGN)).rem(NATIVE_PTR_ALIGN)
    }

    /// Makes sure the offset is aligned with the target pointer alignment.
    fn align_offset(&mut self) {
        let rem = self.offset.rem(NATIVE_PTR_ALIGN);
        if rem != 0 {
            let extra = NATIVE_PTR_ALIGN - rem;

            self.offset += extra;
            self.data.resize(self.data.len() + extra, 0x00);

            debug_assert_eq!(self.offset.rem(NATIVE_PTR_ALIGN), 0);
        }
    }

    /// Appends an encodable value onto the data block.
    #[allow(clippy::needless_pass_by_value)]
    pub fn append<T: Encode>(&mut self, value: T) -> &mut Self {
        let encoded = value.encode();
        let len = encoded.len();

        self.data.extend(encoded);
        self.offset += len;

        self.align_offset();

        self
    }

    /// Appends a null pointer onto the data block.
    pub fn append_null_ptr(&mut self) -> &mut Self {
        self.append_bytes(&[0x00; NATIVE_PTR_SIZE])
    }

    /// Append a raw byte onto the data block.
    pub fn append_byte(&mut self, byte: u8) -> &mut Self {
        self.append_bytes(&[byte][..])
    }

    /// Append a raw list of bytes onto the data block.
    pub fn append_bytes(&mut self, bytes: &[u8]) -> &mut Self {
        let mut encoded = bytes.to_vec();
        let mut len = encoded.len();

        // If the added bytes are unaligned, prepend enough bytes
        // in the beginning of the array so they end up being aligned.
        let unalignment = Self::unalignment_of(len);
        if unalignment != 0 {
            encoded.splice(0..0, vec![0x00; unalignment]);
            len += unalignment;
        }

        debug_assert!(Self::is_aligned(len));
        debug_assert!(Self::is_aligned(encoded.len()));

        self.data.extend(encoded.into_boxed_slice());
        self.offset += len;

        self.align_offset();

        self
    }

    /// Appends a slice of items, where each item is built from the given
    /// closure.
    pub fn append_slice_of<T, F: Fn(&mut MemoryBlockBuilder<'back>, &T)>(&mut self, slice: &[T], f: F) -> &mut Self {
        self.append(slice.len());

        for item in slice {
            f(self, item);
        }

        self
    }

    /// Appends a slice of items, where each item is built from the given
    /// closure.
    pub fn append_slice_ptr_of<T, F: Fn(&mut MemoryBlockBuilder<'back>, &T)>(
        &mut self,
        decl_name: String,
        slice: &[T],
        f: F,
    ) -> &mut Self {
        let data_id = self.backend.find_decl_by_name(&decl_name);
        let mut builder = MemoryBlockBuilder::new(self.backend);

        // Array.length
        builder.append(slice.len());

        let ptr_decl_name = format!("{decl_name}__ptr");
        let ptr_data_id = self
            .backend
            .module_mut()
            .declare_data(&ptr_decl_name, cranelift_module::Linkage::Local, false, false)
            .unwrap();

        let mut ptr_builder = MemoryBlockBuilder::new(self.backend);

        for item in slice {
            f(&mut ptr_builder, item);
        }

        self.backend
            .define_metadata(ptr_data_id, ptr_decl_name, &ptr_builder.finish());
        builder.append_data_address(ptr_data_id, 0);

        self.backend.define_metadata(data_id, decl_name, &builder.finish());
        self.append_data_address(data_id, 0);

        self
    }

    /// Appends a pointer (relocation) to the given function to the data block.
    pub fn append_func_address(&mut self, id: FuncId) -> &mut Self {
        self.data.resize(self.data.len() + NATIVE_PTR_SIZE, 0x00);

        self.func_relocs.push((self.offset, id));
        self.offset += NATIVE_PTR_SIZE;
        self
    }

    /// Appends a pointer (relocation) of the given data to the data block.
    pub fn append_data_address(&mut self, id: DataId, addend: i64) -> &mut Self {
        self.data.resize(self.data.len() + NATIVE_PTR_SIZE, 0x00);

        self.data_relocs.push((self.offset, id, addend));
        self.offset += NATIVE_PTR_SIZE;
        self
    }

    /// Appends a pointer (relocation) of the given string data to the data
    /// block.
    pub fn append_str_address(&mut self, mut value: String) -> &mut Self {
        if !value.ends_with('\0') {
            value.push('\0');
        }

        let name_data_id = self.backend.declare_static_string(&value);

        self.append_data_address(name_data_id, 0)
    }

    /// Appends a pointer (relocation) of the metadata with the given symbol
    /// name.
    pub fn append_metadata_address(&mut self, metadata_name: &str) -> &mut Self {
        let data_id = self.backend.find_decl_by_name(metadata_name);

        self.append_data_address(data_id, NATIVE_PTR_SIZE.cast_signed() as i64)
    }

    /// Takes the builder instance and returns the underlying
    /// [`DataDescription`]
    pub fn finish(self) -> DataDescription {
        let mut ctx = DataDescription::new();
        ctx.set_align(8);
        ctx.set_used(true);

        ctx.define(self.data.into_boxed_slice());

        for (offset, data_reloc, addend) in self.data_relocs {
            let gv = self.backend.module_mut().declare_data_in_data(data_reloc, &mut ctx);

            #[allow(clippy::cast_possible_truncation)]
            ctx.write_data_addr(offset as u32, gv, addend);
        }

        for (offset, func_reloc) in self.func_relocs {
            let gv = self.backend.module_mut().declare_func_in_data(func_reloc, &mut ctx);

            #[allow(clippy::cast_possible_truncation)]
            ctx.write_function_addr(offset as u32, gv);
        }

        ctx
    }
}
