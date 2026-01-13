//! Type metadata structures.
//!
//! None of these structs are meant to be exported - they exist purely to
//! better read type information from passed metadata arguments.

use std::marker::PhantomData;
use std::os::raw::{c_char, c_void};

#[repr(C)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct TypeId(pub usize);

#[repr(C)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct FunctionId(pub usize);

#[repr(C)]
pub struct TypeMetadata {
    /// Metadata entry for the type metadata itself.
    pub metadata: *const TypeMetadata,

    /// Gets the unique ID of the type, used mostly for internal referencing.
    pub type_id: TypeId,

    /// Gets the fully qualified name of the type, including namespace.
    pub full_name: *const c_char,

    /// Gets the canonical size of the type, in bytes.
    pub size: usize,

    /// Gets the canonical alignment of the type, in bytes.
    pub alignment: usize,

    /// Gets all the fields defined on the type, in the order that they're
    /// declared.
    pub fields: List<FieldMetadata>,

    /// Gets all the methods defined on the type, in the order that they're
    /// declared.
    pub methods: List<MethodMetadata>,

    /// Gets all the type parameters defined on the type, in the order
    /// that they're declared.
    pub type_parameters: List<TypeParameterMetadata>,

    /// Gets the address of the `Dispose` method implementation, if any.
    ///
    /// If no `Dispose` method is defined, returns `null`.
    pub drop_ptr: *const c_void,
}

impl TypeMetadata {
    /// Gets the full name of the type as a [`String`].
    pub fn full_name(&self) -> String {
        cstr_to_string(self.full_name)
    }
}

#[repr(C)]
pub struct FieldMetadata {
    /// Metadata entry for the field metadata itself.
    pub metadata: *const TypeMetadata,

    /// Gets the name of the field.
    pub name: *const c_char,

    /// Gets the type of the field.
    pub ty: *const TypeMetadata,
}

impl FieldMetadata {
    /// Gets the name of the field as a [`String`].
    pub fn name(&self) -> String {
        cstr_to_string(self.name)
    }
}

#[repr(C)]
pub struct MethodMetadata {
    /// Metadata entry for the method metadata itself.
    pub metadata: *const TypeMetadata,

    /// Gets the unique ID of the method, used mostly for internal referencing.
    pub func_id: FunctionId,

    /// Gets the fully-qualified name of the method, including namespace and
    /// type name.
    pub full_name: *const c_char,

    /// Gets all the parameters defined on the method, in the order that they're
    /// declared.
    pub parameters: List<ParameterMetadata>,

    /// Gets all the type parameters defined on the method, in the order
    /// that they're declared.
    pub type_parameters: List<TypeParameterMetadata>,

    /// Gets the return type of the method.
    pub return_type: *const TypeMetadata,

    /// Gets the address of the method.
    pub func_ptr: *const c_void,
}

impl MethodMetadata {
    /// Gets the full name of the method as a [`String`].
    pub fn full_name(&self) -> String {
        cstr_to_string(self.full_name)
    }
}

#[repr(C)]
pub struct ParameterMetadata {
    /// Metadata entry for the method parameter metadata itself.
    pub metadata: *const TypeMetadata,

    /// Gets the name of the parameter.
    pub name: *const c_char,

    /// Gets the type of the field.
    pub ty: *const TypeMetadata,

    /// Determines whether the parameter is a variable parameter.
    pub vararg: bool,
}

impl ParameterMetadata {
    /// Gets the name of the parameter as a [`String`].
    pub fn name(&self) -> String {
        cstr_to_string(self.name)
    }
}

#[repr(C)]
pub struct TypeParameterMetadata {
    /// Metadata entry for the type parameter metadata itself.
    pub metadata: *const TypeMetadata,

    /// Gets the name of the type parameter.
    pub name: *const c_char,

    /// Gets all the constraints defined on the type parameter, in the order
    /// that they're declared.
    pub constraints: List<TypeMetadata>,
}

impl TypeParameterMetadata {
    /// Gets the name of the type parameter as a [`String`].
    pub fn name(&self) -> String {
        cstr_to_string(self.name)
    }
}

/// Owned list data structure.
///
/// The [`List<T>`] structure follows a memory layout where the
/// length, a 64-bit unsigned integer, is followed directory with `N`
/// pointers to each of the elements in the list.
#[repr(C)]
pub struct List<T> {
    /// The length of the list.
    length: u64,

    /// Provides a pointer to the base of the list.
    base: *const (),

    _marker: PhantomData<T>,
}

impl<T> List<T> {
    /// Gets the pointer to the base of the list.
    #[inline]
    pub fn base(&self) -> *const T {
        self.base.cast()
    }

    /// Gets the length of the list.
    #[inline]
    #[allow(clippy::cast_possible_truncation)]
    pub fn len(&self) -> usize {
        self.length as usize
    }

    /// Determines if the list is empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Gets a slice of all items in the list, as a slice of pointer.
    #[inline]
    #[must_use]
    pub fn items(&self) -> &[T] {
        if self.is_empty() {
            return &[];
        }

        let len = self.len();
        let ptr = self.base.cast::<T>();

        unsafe { std::slice::from_raw_parts(ptr, len) }
    }
}

fn cstr_to_string(ptr: *const std::ffi::c_char) -> String {
    unsafe {
        let c_str = std::ffi::CStr::from_ptr(ptr);

        c_str.to_string_lossy().into_owned()
    }
}
