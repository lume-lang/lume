use std::ops::Rem;

use cranelift_codegen::isa::TargetIsa;
use cranelift_module::{DataDescription, DataId, FuncId, Module};
use indexmap::IndexMap;
use lume_span::NodeId;
use lume_type_metadata::*;

use crate::CraneliftBackend;

const NATIVE_PTR_SIZE: usize = std::mem::size_of::<*const ()>();
const NATIVE_PTR_ALIGN: usize = std::mem::align_of::<*const ()>();

const TYPE_TABLE_NAME: &str = "type_table";
const FIELD_TABLE_NAME: &str = "field_table";
const METHOD_TABLE_NAME: &str = "method_table";
const PARAMETER_TABLE_NAME: &str = "parameter_table";
const TYPE_PARAMETER_TABLE_NAME: &str = "type_parameter_table";

const OFFSET_TYPE_FIELDS: usize = NATIVE_PTR_SIZE * 6;
const OFFSET_TYPE_METHODS: usize = NATIVE_PTR_SIZE * 8;
const OFFSET_TYPE_TYPE_PARAMETERS: usize = NATIVE_PTR_SIZE * 10;
const OFFSET_FIELD_TYPE: usize = NATIVE_PTR_SIZE * 2;
const OFFSET_METHODS_PARAMETERS: usize = NATIVE_PTR_SIZE * 4;
const OFFSET_METHODS_TYPE_PARAMETERS: usize = NATIVE_PTR_SIZE * 6;
const OFFSET_METHODS_RETURN_TYPE: usize = NATIVE_PTR_SIZE * 7;
const OFFSET_PARAMETER_TYPE: usize = NATIVE_PTR_SIZE * 2;
const OFFSET_TYPE_PARAMETER_CONSTRAINTS: usize = NATIVE_PTR_SIZE * 3;

/// Definitions for symbols and tables inside the module.
struct Definitions<'back> {
    pub tables: TableDefinitions,
    pub offsets: TableOffsets,
    pub builders: TableBuilders<'back>,
    pub metadata_ptr: MetadataEntries,
}

/// Definitions for all tables inside the module.
struct TableDefinitions {
    pub types: DataId,
    pub fields: DataId,
    pub methods: DataId,
    pub parameters: DataId,
    pub type_parameters: DataId,
}

/// Builders for constructing tables inside the module, matching the IDs within
/// [`TableDefinitions`].
struct TableBuilders<'back> {
    pub types: MemoryBlockBuilder<'back>,
    pub fields: MemoryBlockBuilder<'back>,
    pub methods: MemoryBlockBuilder<'back>,
    pub parameters: MemoryBlockBuilder<'back>,
    pub type_parameters: MemoryBlockBuilder<'back>,
}

/// Gets the segment and section names for the metadata section.
fn metadata_section(isa: &dyn TargetIsa) -> (&'static str, &'static str) {
    if isa.triple().operating_system.is_like_darwin() {
        ("__LUMEC", "__metadata")
    } else {
        ("", ".lumec.metadata.types")
    }
}

/// Gets the segment and section names for the type alias section.
fn alias_section(isa: &dyn TargetIsa) -> (&'static str, &'static str) {
    if isa.triple().operating_system.is_like_darwin() {
        ("__LUMEC", "__aliases")
    } else {
        ("", ".lumec.metadata.aliases")
    }
}

impl<'back> TableBuilders<'back> {
    pub fn new(backend: &'back CraneliftBackend) -> Self {
        let (segment, section) = metadata_section(backend.isa.as_ref());

        let mut types = MemoryBlockBuilder::new(backend);
        types.set_segment_section(segment, section);

        let mut fields = MemoryBlockBuilder::new(backend);
        fields.set_segment_section(segment, section);

        let mut methods = MemoryBlockBuilder::new(backend);
        methods.set_segment_section(segment, section);

        let mut parameters = MemoryBlockBuilder::new(backend);
        parameters.set_segment_section(segment, section);

        let mut type_parameters = MemoryBlockBuilder::new(backend);
        type_parameters.set_segment_section(segment, section);

        Self {
            types,
            fields,
            methods,
            parameters,
            type_parameters,
        }
    }
}

/// Defines the offset (in bytes) of each metadata symbol inside their own
/// parent table.
#[derive(Default)]
struct TableOffsets {
    pub types: IndexMap<TypeMetadataId, usize>,
    pub fields: IndexMap<NodeId, usize>,
    pub methods: IndexMap<NodeId, usize>,
    pub parameters: IndexMap<NodeId, usize>,
    pub type_parameters: IndexMap<NodeId, usize>,
}

/// Mangled names of metadata types.
struct MetadataEntries {
    pub ty: TypeMetadataId,
    pub field: TypeMetadataId,
    pub method: TypeMetadataId,
    pub parameter: TypeMetadataId,
    pub type_parameter: TypeMetadataId,
}

impl MetadataEntries {
    pub fn new(metadata: &IndexMap<TypeMetadataId, TypeMetadata>) -> Self {
        fn metadata_entry(metadata: &IndexMap<TypeMetadataId, TypeMetadata>, name: &'static str) -> TypeMetadataId {
            metadata
                .iter()
                .find_map(|(&id, metadata)| (metadata.full_name == name).then_some(id))
                .unwrap_or_else(|| panic!("expected to find metadata of `{name}`"))
        }

        Self {
            ty: metadata_entry(metadata, "std::Type"),
            field: metadata_entry(metadata, "std::Field"),
            method: metadata_entry(metadata, "std::Method"),
            parameter: metadata_entry(metadata, "std::Parameter"),
            type_parameter: metadata_entry(metadata, "std::TypeParameter"),
        }
    }
}

impl CraneliftBackend {
    /// Declares and defines metadata tables inside the module.
    #[libftrace::traced(level = Debug)]
    pub(crate) fn declare_type_metadata(&mut self) {
        let tables = self.declare_tables();

        let mut definitions = Definitions {
            tables,
            builders: TableBuilders::new(self),
            offsets: TableOffsets::default(),
            metadata_ptr: MetadataEntries::new(&self.context.metadata.types),
        };

        declare_builder_stubs(self, &mut definitions);
        self.declare_type_aliases(&mut definitions);
        self.populate_builder_stubs(&mut definitions);
        self.write_table_contents(definitions);
    }
}

impl CraneliftBackend {
    /// Declares all metadata table definitions in the module.
    #[libftrace::traced(level = Debug)]
    fn declare_tables(&self) -> TableDefinitions {
        let types = self
            .module_mut()
            .declare_data(TYPE_TABLE_NAME, cranelift_module::Linkage::Export, false, false)
            .unwrap();

        let fields = self
            .module_mut()
            .declare_data(FIELD_TABLE_NAME, cranelift_module::Linkage::Export, false, false)
            .unwrap();

        let methods = self
            .module_mut()
            .declare_data(METHOD_TABLE_NAME, cranelift_module::Linkage::Export, false, false)
            .unwrap();

        let parameters = self
            .module_mut()
            .declare_data(PARAMETER_TABLE_NAME, cranelift_module::Linkage::Export, false, false)
            .unwrap();

        let type_parameters = self
            .module_mut()
            .declare_data(
                TYPE_PARAMETER_TABLE_NAME,
                cranelift_module::Linkage::Export,
                false,
                false,
            )
            .unwrap();

        TableDefinitions {
            types,
            fields,
            methods,
            parameters,
            type_parameters,
        }
    }

    #[libftrace::traced(level = Debug)]
    fn write_table_contents(&self, defs: Definitions) {
        self.define_metadata(
            defs.tables.types,
            TYPE_TABLE_NAME.to_owned(),
            &defs.builders.types.finish(),
        );

        self.define_metadata(
            defs.tables.fields,
            FIELD_TABLE_NAME.to_owned(),
            &defs.builders.fields.finish(),
        );

        self.define_metadata(
            defs.tables.methods,
            METHOD_TABLE_NAME.to_owned(),
            &defs.builders.methods.finish(),
        );

        self.define_metadata(
            defs.tables.parameters,
            PARAMETER_TABLE_NAME.to_owned(),
            &defs.builders.parameters.finish(),
        );

        self.define_metadata(
            defs.tables.type_parameters,
            TYPE_PARAMETER_TABLE_NAME.to_owned(),
            &defs.builders.type_parameters.finish(),
        );
    }

    #[libftrace::traced(level = Trace, fields(data_id, name))]
    fn define_metadata(&self, data_id: DataId, name: String, desc: &DataDescription) {
        self.module_mut().define_data(data_id, desc).unwrap();
        self.static_data.write().unwrap().insert(name, data_id);
    }
}

/// Creates stubs for all metadata entries.
///
/// We can't define entire metadata entries yet, since we need to reference
/// other metadata entries which might not exist yet.
///
/// This function also defines the offset of each metadata entry and saves it
/// inside the [`Definitions::offsets`] field.
#[libftrace::traced(level = Debug)]
fn declare_builder_stubs(ctx: &CraneliftBackend, defs: &mut Definitions) {
    let metadata_store = &ctx.context.metadata;

    for type_ in metadata_store.types.values() {
        declare_type_metadata_stub(ctx, type_, defs);
    }

    for (&id, field) in &metadata_store.fields {
        declare_field_metadata_stub(id, field, defs);
    }

    for method in metadata_store.methods.values() {
        declare_method_metadata_stub(ctx, method, defs);
    }

    for parameter in metadata_store.parameters.values() {
        declare_parameter_metadata_stub(parameter, defs);
    }

    for type_parameter in metadata_store.type_parameters.values() {
        declare_type_parameter_metadata_stub(type_parameter, defs);
    }
}

#[libftrace::traced(level = Trace, fields(name = metadata.full_name))]
fn declare_type_metadata_stub(ctx: &CraneliftBackend, metadata: &TypeMetadata, defs: &mut Definitions) {
    let builder = &mut defs.builders.types;
    let base_offset = builder.offset();

    // Metadata entry
    builder.append_null_ptr();

    // Type.type_id
    builder.append(metadata.type_id_usize());

    // Type.name
    builder.append_str_address(metadata.full_name.clone());

    // Type.size
    builder.append(metadata.size);

    // Type.alignment
    builder.append(metadata.alignment);

    // Type.fields
    builder.append(metadata.fields.len() as u64);
    debug_assert_eq!(builder.offset() - base_offset, OFFSET_TYPE_FIELDS);
    builder.append_null_ptr();

    // Type.methods
    builder.append(metadata.methods.len() as u64);
    debug_assert_eq!(builder.offset() - base_offset, OFFSET_TYPE_METHODS);
    builder.append_null_ptr();

    // Type.type_parameters
    builder.append(metadata.type_parameters.len() as u64);
    debug_assert_eq!(builder.offset() - base_offset, OFFSET_TYPE_TYPE_PARAMETERS);
    builder.append_null_ptr();

    // Type.drop_ptr
    if let Some(drop_method) = metadata.drop_method {
        let drop_ptr = ctx.declared_funcs.get(&drop_method).unwrap();

        builder.append_func_address(drop_ptr.id);
    } else {
        builder.append_null_ptr();
    }

    defs.offsets.types.insert(metadata.id, base_offset);
}

#[libftrace::traced(level = Trace, fields(name = metadata.name))]
fn declare_field_metadata_stub(id: NodeId, metadata: &FieldMetadata, defs: &mut Definitions) {
    let builder = &mut defs.builders.fields;
    let base_offset = builder.offset();

    // Metadata entry
    builder.append_null_ptr();

    // Field.name
    builder.append_str_address(metadata.name.clone());

    // Field.type
    debug_assert_eq!(builder.offset() - base_offset, OFFSET_FIELD_TYPE);
    builder.append_null_ptr();

    defs.offsets.fields.insert(id, base_offset);
}

#[libftrace::traced(level = Trace, fields(name = metadata.full_name))]
fn declare_method_metadata_stub(ctx: &CraneliftBackend, metadata: &MethodMetadata, defs: &mut Definitions) {
    let builder = &mut defs.builders.methods;
    let base_offset = builder.offset();

    // Metadata entry
    builder.append_null_ptr();

    // Method.id
    builder.append(metadata.definition_id.as_usize());

    // Method.full_name
    builder.append_str_address(metadata.full_name.clone());

    // Method.parameters
    builder.append(metadata.parameters.len() as u64);
    debug_assert_eq!(builder.offset() - base_offset, OFFSET_METHODS_PARAMETERS);
    builder.append_null_ptr();

    // Method.type_parameters
    builder.append(metadata.type_parameters.len() as u64);
    debug_assert_eq!(builder.offset() - base_offset, OFFSET_METHODS_TYPE_PARAMETERS);
    builder.append_null_ptr();

    // Method.return_type
    debug_assert_eq!(builder.offset() - base_offset, OFFSET_METHODS_RETURN_TYPE);
    builder.append_null_ptr();

    // Method.func_ptr
    match ctx.declared_funcs.get(&metadata.func_id) {
        Some(func) => builder.append_func_address(func.id),
        None => builder.append_null_ptr(),
    };

    defs.offsets.methods.insert(metadata.func_id, base_offset);
}

#[libftrace::traced(level = Trace, fields(name = metadata.name))]
fn declare_parameter_metadata_stub(metadata: &ParameterMetadata, defs: &mut Definitions) {
    let builder = &mut defs.builders.parameters;
    let base_offset = builder.offset();

    // Metadata entry
    builder.append_null_ptr();

    // Parameter.name
    builder.append_str_address(metadata.name.clone());

    // Parameter.type
    debug_assert_eq!(builder.offset() - base_offset, OFFSET_PARAMETER_TYPE);
    builder.append_null_ptr();

    // Parameter.vararg
    builder.append_byte(u8::from(metadata.vararg));

    defs.offsets.parameters.insert(metadata.id, base_offset);
}

#[libftrace::traced(level = Trace, fields(name = metadata.name))]
fn declare_type_parameter_metadata_stub(metadata: &TypeParameterMetadata, defs: &mut Definitions) {
    let builder = &mut defs.builders.type_parameters;
    let base_offset = builder.offset();

    // Metadata entry
    builder.append_null_ptr();

    // TypeParameter.name
    builder.append_str_address(metadata.name.clone());

    // TypeParameter.constraints
    builder.append(metadata.constraints.len() as u64);
    debug_assert_eq!(builder.offset() - base_offset, OFFSET_TYPE_PARAMETER_CONSTRAINTS);
    builder.append_null_ptr();

    defs.offsets.type_parameters.insert(metadata.id, base_offset);
}

impl CraneliftBackend {
    /// Declares aliases for all type metadata entries, which contain a pointer
    /// to the type's metadata entry. This is mostly used in the runtime, to
    /// make it easier to access type metadata from the Rust-based runtime.
    #[libftrace::traced(level = Debug)]
    fn declare_type_aliases(&self, defs: &mut Definitions) {
        let type_table_data_id = defs.tables.types;

        for (&id, &symbol_offset) in &defs.offsets.types {
            let metadata = self.context.metadata.types.get(&id).unwrap();

            libftrace::debug!("declaring type alias: {} at +{symbol_offset:0x}", metadata.mangled_name);
            let (segment, section) = alias_section(self.isa.as_ref());

            let alias_data_id = self
                .module_mut()
                .declare_data(&metadata.mangled_name, cranelift_module::Linkage::Export, false, false)
                .unwrap();

            let mut builder = MemoryBlockBuilder::new(self);
            builder.set_segment_section(segment, section);
            builder.append_data_address(type_table_data_id, symbol_offset.cast_signed() as i64);

            self.define_metadata(alias_data_id, metadata.mangled_name.clone(), &builder.finish());
        }
    }
}

impl CraneliftBackend {
    /// Populates the stubs which were created in the [`declare_builder_stubs`]
    /// function.
    #[libftrace::traced(level = Debug)]
    fn populate_builder_stubs(&self, defs: &mut Definitions) {
        for (id, offset) in defs.offsets.types.clone() {
            self.populate_type_metadata_stub(id, offset, defs);
        }

        for (id, offset) in defs.offsets.fields.clone() {
            self.populate_field_metadata_stub(id, offset, defs);
        }

        for (id, offset) in defs.offsets.methods.clone() {
            self.populate_method_metadata_stub(id, offset, defs);
        }

        for (id, offset) in defs.offsets.parameters.clone() {
            self.populate_parameter_metadata_stub(id, offset, defs);
        }

        for (id, offset) in defs.offsets.type_parameters.clone() {
            self.populate_type_parameter_metadata_stub(id, offset, defs);
        }
    }

    #[libftrace::traced(level = Trace, fields(id, offset))]
    fn populate_type_metadata_stub(&self, id: TypeMetadataId, offset: usize, defs: &mut Definitions) {
        let builder = &mut defs.builders.types;
        let metadata = self.context.metadata.types.get(&id).unwrap();

        // Metadata entry
        let metadata_offset = *defs.offsets.types.get(&defs.metadata_ptr.ty).unwrap();
        builder.append_data_address(defs.tables.types, metadata_offset.cast_signed() as i64);

        // Type.fields
        if let Some(first_field) = metadata.fields.first() {
            let field_offset = *defs.offsets.fields.get(first_field).unwrap();
            builder.append_data_address_at(
                defs.tables.fields,
                offset + OFFSET_TYPE_FIELDS,
                field_offset.cast_signed() as i64,
            );
        }

        // Type.methods
        if let Some(first_method) = metadata.methods.first() {
            let method_offset = *defs.offsets.methods.get(first_method).unwrap();

            builder.append_data_address_at(
                defs.tables.methods,
                offset + OFFSET_TYPE_METHODS,
                method_offset.cast_signed() as i64,
            );
        }

        // Type.type_parameters
        if let Some(first_type_parameter) = metadata.type_parameters.first() {
            let type_parameter_offset = *defs.offsets.type_parameters.get(first_type_parameter).unwrap();

            builder.append_data_address_at(
                defs.tables.type_parameters,
                offset + OFFSET_TYPE_TYPE_PARAMETERS,
                type_parameter_offset.cast_signed() as i64,
            );
        }
    }

    #[libftrace::traced(level = Trace, fields(id, offset))]
    fn populate_field_metadata_stub(&self, id: NodeId, offset: usize, defs: &mut Definitions) {
        let builder = &mut defs.builders.fields;
        let metadata = self.context.metadata.fields.get(&id).unwrap();

        // Metadata entry
        let metadata_offset = *defs.offsets.types.get(&defs.metadata_ptr.field).unwrap();
        builder.append_data_address(defs.tables.types, metadata_offset.cast_signed() as i64);

        // Field.type
        let type_offset = *defs.offsets.types.get(&metadata.ty).unwrap();
        builder.append_data_address_at(
            defs.tables.types,
            offset + OFFSET_FIELD_TYPE,
            type_offset.cast_signed() as i64,
        );
    }

    #[libftrace::traced(level = Trace, fields(id, offset))]
    fn populate_method_metadata_stub(&self, id: NodeId, offset: usize, defs: &mut Definitions) {
        let builder = &mut defs.builders.methods;
        let metadata = self.context.metadata.methods.get(&id).unwrap();

        // Metadata entry
        let metadata_offset = *defs.offsets.types.get(&defs.metadata_ptr.method).unwrap();
        builder.append_data_address(defs.tables.types, metadata_offset.cast_signed() as i64);

        // Method.parameters
        if let Some(first_parameter) = metadata.parameters.first() {
            let parameter_offset = *defs.offsets.parameters.get(first_parameter).unwrap();

            builder.append_data_address_at(
                defs.tables.parameters,
                offset + OFFSET_METHODS_PARAMETERS,
                parameter_offset.cast_signed() as i64,
            );
        }

        // Method.type_parameters
        if let Some(first_parameter) = metadata.type_parameters.first() {
            let parameter_offset = *defs.offsets.type_parameters.get(first_parameter).unwrap();

            builder.append_data_address_at(
                defs.tables.type_parameters,
                offset + OFFSET_METHODS_TYPE_PARAMETERS,
                parameter_offset.cast_signed() as i64,
            );
        }

        // Method.return_type
        let type_offset = *defs.offsets.types.get(&metadata.return_type).unwrap();
        builder.append_data_address_at(
            defs.tables.types,
            offset + OFFSET_METHODS_RETURN_TYPE,
            type_offset.cast_signed() as i64,
        );
    }

    #[libftrace::traced(level = Trace, fields(id, offset))]
    fn populate_parameter_metadata_stub(&self, id: NodeId, offset: usize, defs: &mut Definitions) {
        let builder = &mut defs.builders.parameters;
        let metadata = self.context.metadata.parameters.get(&id).unwrap();

        // Metadata entry
        let metadata_offset = *defs.offsets.types.get(&defs.metadata_ptr.parameter).unwrap();
        builder.append_data_address(defs.tables.types, metadata_offset.cast_signed() as i64);

        // Parameter.type
        let type_offset = *defs.offsets.types.get(&metadata.ty).unwrap();
        builder.append_data_address_at(
            defs.tables.types,
            offset + OFFSET_PARAMETER_TYPE,
            type_offset.cast_signed() as i64,
        );
    }

    #[libftrace::traced(level = Trace, fields(id, offset))]
    fn populate_type_parameter_metadata_stub(&self, id: NodeId, offset: usize, defs: &mut Definitions) {
        let builder = &mut defs.builders.type_parameters;
        let metadata = self.context.metadata.type_parameters.get(&id).unwrap();

        // Metadata entry
        let metadata_offset = *defs.offsets.types.get(&defs.metadata_ptr.type_parameter).unwrap();
        builder.append_data_address(defs.tables.types, metadata_offset.cast_signed() as i64);

        // TypeParameter.constraints
        if let Some(first_constraint) = metadata.constraints.first() {
            let constraint_offset = *defs.offsets.types.get(first_constraint).unwrap();

            builder.append_data_address_at(
                defs.tables.types,
                offset + OFFSET_TYPE_PARAMETER_CONSTRAINTS,
                constraint_offset.cast_signed() as i64,
            );
        }
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

/// Builds a memory block with support for data- and function relocations.
struct MemoryBlockBuilder<'back> {
    backend: &'back CraneliftBackend,

    data: Vec<u8>,
    data_relocs: Vec<(usize, DataId, i64)>,
    func_relocs: Vec<(usize, FuncId)>,
    offset: usize,

    seg_sect_name: Option<(&'static str, &'static str)>,
}

impl<'back> MemoryBlockBuilder<'back> {
    pub fn new(backend: &'back CraneliftBackend) -> Self {
        Self {
            backend,
            data: Vec::new(),
            data_relocs: Vec::new(),
            func_relocs: Vec::new(),
            offset: 0,
            seg_sect_name: None,
        }
    }

    /// Gets the current offset of the builder.
    #[inline]
    fn offset(&self) -> usize {
        self.offset
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

    /// Appends a pointer (relocation) of the given data to the data block at
    /// the given offset.
    ///
    /// This method **will overwrite** whatever existed at the offset.
    /// The offset within the builder is not changed.
    pub fn append_data_address_at(&mut self, id: DataId, offset: usize, addend: i64) -> &mut Self {
        self.data_relocs.push((offset, id, addend));
        self
    }

    /// Appends a pointer (relocation) of the given string data to the data
    /// block.
    pub fn append_str_address(&mut self, mut value: String) -> &mut Self {
        if !value.ends_with('\0') {
            value.push('\0');
        }

        let name_data_id = self.backend.define_string(&value);

        self.append_data_address(name_data_id, 0)
    }

    /// Sets the section and segment names for the metadata block.
    pub fn set_segment_section(&mut self, segment: &'static str, section: &'static str) -> &mut Self {
        self.seg_sect_name = Some((segment, section));
        self
    }

    /// Takes the builder instance and returns the underlying
    /// [`DataDescription`]
    pub fn finish(mut self) -> DataDescription {
        let mut ctx = DataDescription::new();
        ctx.set_align(8);
        ctx.set_used(true);

        if let Some((segment, section)) = self.seg_sect_name.take() {
            ctx.set_segment_section(segment, section);
        }

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
