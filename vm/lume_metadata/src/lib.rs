//! Type metadata structures.
//!
//! None of these structs are meant to be exported - they exist purely to
//! better read type information from passed metadata arguments.

use std::{
    marker::PhantomData,
    os::raw::{c_char, c_void},
};

#[repr(C)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct TypeId(pub usize);

#[repr(C)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct FunctionId(pub usize);

#[repr(C)]
pub struct TypeMetadata {
    /// Gets the unique ID of the type, used mostly for internal referencing.
    pub type_id: TypeId,

    /// Gets the fully qualified name of the type, including namespace.
    pub full_name: *const c_char,

    /// Gets the canonical size of the type, in bytes.
    pub size: usize,

    /// Gets the canonical alignment of the type, in bytes.
    pub alignment: usize,

    /// Gets all the fields defined on the type, in the order that they're declared.
    pub fields: List<FieldMetadata>,

    /// Gets all the methods defined on the type, in the order that they're declared.
    pub methods: List<MethodMetadata>,

    /// Gets all the type arguments defined on the type, in the order
    /// that they're declared.
    pub type_arguments: List<TypeMetadata>,

    /// Gets the address of the `Dispose` method implementation, if any.
    ///
    /// If no `Dispose` method is defined, returns `null`.
    pub drop_ptr: *const c_void,
}

#[repr(C)]
pub struct FieldMetadata {
    /// Gets the name of the field.
    pub name: *const c_char,

    /// Gets the type of the field.
    pub ty: *const TypeMetadata,
}

#[repr(C)]
pub struct MethodMetadata {
    /// Gets the unique ID of the method, used mostly for internal referencing.
    pub func_id: FunctionId,

    /// Gets the fully-qualified name of the method, including namespace and type name.
    pub full_name: *const c_char,

    /// Gets all the parameters defined on the method, in the order that they're declared.
    pub parameters: List<ParameterMetadata>,

    /// Gets all the type parameters defined on the method, in the order
    /// that they're declared.
    pub type_parameters: List<TypeParameterMetadata>,

    /// Gets the return type of the method.
    pub return_type: *const TypeMetadata,

    /// Gets the address of the method.
    pub func_ptr: *const c_void,
}

#[repr(C)]
pub struct ParameterMetadata {
    /// Gets the name of the parameter.
    pub name: *const c_char,

    /// Gets the type of the field.
    pub ty: *const TypeMetadata,

    /// Determines whether the parameter is a variable parameter.
    pub vararg: bool,
}

#[repr(C)]
pub struct TypeParameterMetadata {
    /// Gets the name of the type parameter.
    pub name: *const c_char,

    /// Gets all the constraints defined on the type parameter, in the order
    /// that they're declared.
    pub constraints: List<TypeMetadata>,
}

/// Owned list data structure.
///
/// The [`List<T>`] structure follows a memory layout where the
/// length, a 64-bit unsigned integer, is followed directory with `N`
/// pointers to each of the elements in the list.
#[repr(C)]
pub struct List<T> {
    /// Provides a pointer to the base of the list.
    base: *const (),

    _marker: PhantomData<T>,
}

impl<T> List<T> {
    /// Gets the length of the list.
    #[inline]
    #[must_use]
    pub fn len(&self) -> usize {
        unsafe { (self.base as *const usize).read() }
    }

    /// Gets a slice of all items in the list, as a slice of pointer.
    #[inline]
    #[must_use]
    pub fn items(&self) -> &[*const T] {
        let len = self.len();
        let ptr = unsafe { self.base.byte_add(std::mem::size_of::<usize>()) as *const *const T };

        unsafe { std::slice::from_raw_parts(ptr, len) }
    }
}
