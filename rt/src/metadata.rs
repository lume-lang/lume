//! Type metadata structures.
//!
//! None of these structs are meant to be exported - they exist purely to
//! better read type information from passed metadata arguments.

use std::os::raw::{c_char, c_void};

#[repr(C)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct TypeId(usize);

#[repr(C)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct FunctionId(usize);

#[repr(C)]
#[derive(Debug)]
pub struct TypeMetadata {
    /// Gets the fully qualified name of the type, including namespace.
    pub full_name: *const c_char,

    /// Gets the canonical size of the type, in bytes.
    pub size: usize,

    /// Gets the canonical alignment of the type, in bytes.
    pub alignment: usize,

    /// Gets the unique ID of the type, used mostly for internal referencing.
    pub type_id: TypeId,

    /// Gets all the fields defined on the type, in the order that they're declared.
    pub fields: *const List<FieldMetadata>,

    /// Gets all the methods defined on the type, in the order that they're declared.
    pub methods: *const List<MethodMetadata>,

    /// Gets all the type arguments defined on the type, in the order
    /// that they're declared.
    pub type_arguments: *const List<TypeMetadata>,
}

#[repr(C)]
#[derive(Debug)]
pub struct FieldMetadata {
    /// Gets the name of the field.
    pub name: *const c_char,

    /// Gets the type of the field.
    pub ty: *const TypeMetadata,
}

#[repr(C)]
#[derive(Debug)]
pub struct MethodMetadata {
    /// Gets the fully-qualified name of the method, including namespace and type name.
    pub full_name: *const c_char,

    /// Gets the unique ID of the method, used mostly for internal referencing.
    pub func_id: FunctionId,

    /// Gets all the parameters defined on the method, in the order that they're declared.
    pub parameters: *const List<ParameterMetadata>,

    /// Gets all the type parameters defined on the method, in the order
    /// that they're declared.
    pub type_parameters: *const List<TypeParameterMetadata>,

    /// Gets the return type of the method.
    pub return_type: *const TypeMetadata,

    /// Gets the address of the method.
    pub func_ptr: *const c_void,
}

#[repr(C)]
#[derive(Debug)]
pub struct ParameterMetadata {
    /// Gets the name of the parameter.
    pub name: *const c_char,

    /// Gets the type of the field.
    pub ty: *const TypeMetadata,

    /// Determines whether the parameter is a variable parameter.
    pub vararg: bool,
}

#[repr(C)]
#[derive(Debug)]
pub struct TypeParameterMetadata {
    /// Gets the name of the type parameter.
    pub name: *const c_char,

    /// Gets all the constraints defined on the type parameter, in the order
    /// that they're declared.
    pub constraints: *const List<TypeMetadata>,
}

#[repr(C)]
#[derive(Debug)]
pub struct List<T> {
    /// Defines the amount of items within the list.
    pub length: usize,

    /// Defines all the items in the list.
    pub items: [T; 0],
}
