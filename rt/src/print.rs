use std::{io::Write, os::raw::c_char};

use crate::{LumeString, array::Array};

#[unsafe(export_name = "std::io::print")]
pub extern "C" fn print(fmt: *const c_char, args: *const Array<*const c_char>) {
    print!("{}", internal_format_ffi(fmt, args));
    flush_stdout();
}

#[unsafe(export_name = "std::io::println")]
pub extern "C" fn println(fmt: *const c_char, args: *const Array<*const c_char>) {
    println!("{}", internal_format_ffi(fmt, args));
    flush_stdout();
}

#[unsafe(export_name = "std::io::format")]
pub extern "C" fn format(fmt: *const c_char, args: *const Array<*const c_char>) -> *const c_char {
    let formatted = internal_format_ffi(fmt, args);

    crate::string::cstr_from_string(formatted)
}

/// Flushes any buffered content in `stdout` to the processes output.
///
/// This is mostly used when using `std::io::print` without a newline, which might
/// not be flushed before the program exists.
fn flush_stdout() {
    let _ = std::io::stdout().flush();
}

/// Formats the given FFI strings into a formatted [`String`], which can be printed
/// or sent back to the Lume application.
fn internal_format_ffi(fmt: *const c_char, args: *const Array<*const c_char>) -> String {
    let fmt_str = crate::string::cstr_to_string(fmt);

    let args = unsafe { args.read() };
    let mut args_str = Vec::with_capacity(args.length as usize);

    for arg in args.iter() {
        let arg_str = crate::string::cstr_to_string(arg.cast());

        args_str.push(arg_str);
    }

    internal_format_str(fmt_str, &args_str)
}

/// Formats the given Rust strings into a formatted [`String`], which can be printed
/// or sent back to the Lume application.
fn internal_format_str(fmt: String, args: &[String]) -> String {
    let mut idx = 0;
    let mut formatted = String::with_capacity(fmt.len());

    while let Some(start_idx) = fmt[idx..].find('{') {
        let start_idx = start_idx + idx;
        let mut end_idx = start_idx + 1;

        for c in fmt[(start_idx + 1)..].chars() {
            if !c.is_ascii_digit() {
                break;
            }
            end_idx += 1;
        }

        let unformatted_slice = &fmt[idx..start_idx];
        formatted.push_str(unformatted_slice);

        let arg_idx = fmt[(start_idx + 1)..end_idx].parse::<u8>().unwrap();
        formatted.push_str(&args[arg_idx as usize]);

        idx = end_idx + 1;
    }

    formatted.push_str(&fmt[idx..]);

    formatted
}

#[unsafe(export_name = "std::Int8::to_string")]
pub extern "C" fn intr_int8_tostring(val: i8) -> LumeString {
    crate::cstr_from_string(format!("{val}"))
}

#[unsafe(export_name = "std::Int16::to_string")]
pub extern "C" fn intr_int16_tostring(val: i16) -> LumeString {
    crate::cstr_from_string(format!("{val}"))
}

#[unsafe(export_name = "std::Int32::to_string")]
pub extern "C" fn intr_int32_tostring(val: i32) -> LumeString {
    crate::cstr_from_string(format!("{val}"))
}

#[unsafe(export_name = "std::Int64::to_string")]
pub extern "C" fn intr_int64_tostring(val: i64) -> LumeString {
    crate::cstr_from_string(format!("{val}"))
}

#[unsafe(export_name = "std::UInt8::to_string")]
pub extern "C" fn intr_uint8_tostring(val: u8) -> LumeString {
    crate::cstr_from_string(format!("{val}"))
}

#[unsafe(export_name = "std::UInt16::to_string")]
pub extern "C" fn intr_uint16_tostring(val: u16) -> LumeString {
    crate::cstr_from_string(format!("{val}"))
}

#[unsafe(export_name = "std::UInt32::to_string")]
pub extern "C" fn intr_uint32_tostring(val: u32) -> LumeString {
    crate::cstr_from_string(format!("{val}"))
}

#[unsafe(export_name = "std::UInt64::to_string")]
pub extern "C" fn intr_uint64_tostring(val: u64) -> LumeString {
    crate::cstr_from_string(format!("{val}"))
}

#[unsafe(export_name = "std::Float::to_string")]
pub extern "C" fn intr_float_tostring(val: f32) -> LumeString {
    crate::cstr_from_string(format!("{val}"))
}

#[unsafe(export_name = "std::Double::to_string")]
pub extern "C" fn intr_double_tostring(val: f64) -> LumeString {
    crate::cstr_from_string(format!("{val}"))
}
