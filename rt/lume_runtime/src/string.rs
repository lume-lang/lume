use std::ffi::{CStr, c_char};

use lume_rt_metadata::TypeMetadata;
use lume_tagged::strip_tags;

pub type LumeString = *const c_char;

/// Creates a [`String`] from the given [`LumeString`].
pub(crate) fn cstr_to_string<'a>(ptr: LumeString) -> &'a str {
    let ptr = strip_tags(ptr.cast_mut());

    unsafe {
        #[allow(clippy::cast_ptr_alignment)]
        let string_obj = ptr.cast::<*const c_char>().read();
        let c_str = CStr::from_ptr(string_obj);

        c_str.to_str().unwrap()
    }
}

unsafe extern "C" {
    pub fn memcpy(dst: *mut u8, src: *const u8, len: usize);

    #[link_name = "_L1_SP3stdN3std6String"]
    static __STD_STRING: u8;
}

/// Creates a [`LumeString`] from the given [`String`].
pub(crate) fn cstr_from_string(val: &str) -> LumeString {
    #[allow(clippy::cast_ptr_alignment)]
    let metadata_ptr = unsafe { (&raw const __STD_STRING).cast::<*const TypeMetadata>().read() };

    unsafe {
        let string_object = lume_gc::allocate_object(lume_gc::POINTER_SIZE * 2, metadata_ptr);
        let string_content = crate::mem::lumert_alloc(val.len() + 1, metadata_ptr);

        let dst_slice: &mut [u8] = std::slice::from_raw_parts_mut(string_content, val.len() + 1);
        dst_slice[0..val.len()].copy_from_slice(val.as_bytes());
        dst_slice[val.len()] = 0;

        #[allow(clippy::cast_ptr_alignment)]
        strip_tags(string_object).cast::<*const u8>().write(string_content);

        string_object.cast()
    }
}
