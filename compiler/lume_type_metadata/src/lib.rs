use indexmap::IndexMap;
use lume_types::TypeId;

#[derive(Hash, Debug, Copy, Clone, PartialEq, Eq)]
pub enum FunctionKind {
    Function,
    Method,
}

#[derive(Hash, Debug, Copy, Clone, PartialEq, Eq)]
pub struct FunctionId(usize);

impl FunctionId {
    pub fn new(kind: FunctionKind, id: usize) -> Self {
        // Used to prevent `hash_id` from creating a value of 0 when the kind is
        // `FunctionKind::Function` and the ID is 0. A function ID of 0 can look
        // wrong or misleading, so we're explicitly removing that possiblity.
        static HASH_OFFSET: usize = 0x4D6B_0189;

        Self(lume_span::hash_id(&(kind, id + HASH_OFFSET)))
    }

    pub fn as_usize(self) -> usize {
        self.0
    }
}

#[derive(Default, Debug, Clone)]
pub struct StaticMetadata {
    pub metadata: IndexMap<TypeMetadataId, TypeMetadata>,
}

#[derive(Hash, Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeMetadataId(pub usize);

#[derive(Default, Debug, Clone)]
pub struct TypeMetadata {
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

#[derive(Debug, Clone)]
pub struct FieldMetadata {
    /// Gets the name of the field.
    pub name: String,

    /// Gets the type of the field.
    pub ty: TypeMetadataId,
}

#[derive(Debug, Clone)]
pub struct MethodMetadata {
    /// Gets the fully-qualified name of the method, including namespace and type name.
    pub full_name: String,

    /// Gets the unique ID of the method, used mostly for internal referencing.
    pub func_id: FunctionId,

    /// Gets all the parameters defined on the method, in the order that they're declared.
    pub parameters: Vec<ParameterMetadata>,

    /// Gets all the type parameters defined on the method, in the order
    /// that they're declared.
    pub type_parameters: Vec<TypeParameterMetadata>,

    /// Gets the return type of the method.
    pub return_type: TypeMetadataId,
}

#[derive(Debug, Clone)]
pub struct ParameterMetadata {
    /// Gets the name of the parameter.
    pub name: String,

    /// Gets the type of the field.
    pub ty: TypeMetadataId,

    /// Determines whether the parameter is a variable parameter.
    pub vararg: bool,
}

#[derive(Debug, Clone)]
pub struct TypeParameterMetadata {
    /// Gets the name of the type parameter.
    pub name: String,

    /// Gets all the constraints defined on the type parameter, in the order
    /// that they're declared.
    pub constraints: Vec<TypeMetadataId>,
}
