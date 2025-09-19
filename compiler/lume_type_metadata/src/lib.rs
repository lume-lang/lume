use indexmap::IndexMap;
use lume_span::{DefId, hash_id};
use lume_types::TypeId;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Default, Debug, Clone)]
pub struct StaticMetadata {
    pub metadata: IndexMap<TypeMetadataId, TypeMetadata>,
}

impl StaticMetadata {
    /// Merges the current [`StaticMetadata`] into the other given map.
    pub fn merge_into(mut self, dest: &mut StaticMetadata) {
        dest.metadata.append(&mut self.metadata);
    }
}

#[derive(Serialize, Deserialize, Hash, Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeMetadataId(pub usize);

impl From<&lume_types::TypeRef> for TypeMetadataId {
    fn from(value: &lume_types::TypeRef) -> Self {
        Self(hash_id(&value.instance_of))
    }
}

#[derive(Serialize, Deserialize, Default, Debug, Clone)]
pub struct TypeMetadata {
    /// Gets the unique ID of the type metadata.
    pub id: TypeMetadataId,

    /// Gets the fully qualified name of the type, including namespace.
    pub full_name: String,

    /// Gets the canonical size of the type, in bytes.
    pub size: usize,

    /// Gets the canonical alignment of the type, in bytes.
    pub alignment: usize,

    /// Gets the unique ID of the type, used mostly for internal referencing.
    pub type_id: TypeId,

    /// Gets all the fields defined on the type, in the order that they're declared.
    pub fields: Vec<FieldMetadata>,

    /// Gets all the methods defined on the type, in the order that they're declared.
    pub methods: Vec<MethodMetadata>,

    /// Gets all the type arguments defined on the type, in the order
    /// that they're declared.
    pub type_arguments: Vec<TypeMetadataId>,

    /// Gets the definition of the `Dispose` method implementation, if any.
    pub drop_method: Option<DefId>,
}

impl TypeMetadata {
    /// Gets the `type_id` field as a single `usize`.
    #[inline]
    pub fn type_id_usize(&self) -> usize {
        lume_span::hash_id(&self.type_id)
    }

    /// Gets the symbol name of the type metadata.
    #[inline]
    pub fn symbol_name(&self) -> String {
        format!("@__SYM_TYPE_{:08X}_{}", self.type_id_usize(), self.full_name)
    }
}

impl std::hash::Hash for TypeMetadata {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.full_name.hash(state);
    }
}

impl PartialEq for TypeMetadata {
    fn eq(&self, other: &Self) -> bool {
        self.full_name == other.full_name
    }
}

impl Eq for TypeMetadata {}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct FieldMetadata {
    /// Gets the name of the field.
    pub name: String,

    /// Gets the type of the field.
    pub ty: TypeMetadataId,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct MethodMetadata {
    /// Gets the fully-qualified name of the method, including namespace and type name.
    pub full_name: String,

    /// Gets the unique ID of the method, used mostly for internal referencing.
    pub func_id: DefId,

    /// Gets the unique ID of the method definition, which this method implements.
    pub definition_id: DefId,

    /// Gets all the parameters defined on the method, in the order that they're declared.
    pub parameters: Vec<ParameterMetadata>,

    /// Gets all the type parameters defined on the method, in the order
    /// that they're declared.
    pub type_parameters: Vec<TypeParameterMetadata>,

    /// Gets the return type of the method.
    pub return_type: TypeMetadataId,
}

impl MethodMetadata {
    /// Gets the symbol name of the method metadata.
    #[inline]
    pub fn symbol_name(&self) -> String {
        format!("@__SYM_FUNC_{:08X}_{}", self.func_id.as_usize(), self.full_name)
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ParameterMetadata {
    /// Gets the name of the parameter.
    pub name: String,

    /// Gets the type of the field.
    pub ty: TypeMetadataId,

    /// Determines whether the parameter is a variable parameter.
    pub vararg: bool,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct TypeParameterMetadata {
    /// Gets the name of the type parameter.
    pub name: String,

    /// Gets all the constraints defined on the type parameter, in the order
    /// that they're declared.
    pub constraints: Vec<TypeMetadataId>,
}
