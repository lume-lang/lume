use std::ffi::{CStr, CString, c_char};

pub type LumeString = *const c_char;

/// Creates a [`String`] from the given [`LumeString`].
pub(crate) fn cstr_to_string(ptr: LumeString) -> String {
    unsafe {
        let c_str = CStr::from_ptr(ptr);

        c_str.to_string_lossy().into_owned()
    }
}

/// Creates a [`LumeString`] from the given [`String`].
pub(crate) fn cstr_from_string(val: String) -> LumeString {
    let c_str = CString::new(val).expect("failed to create `CString` from `String`");

    c_str.as_ptr()
}
