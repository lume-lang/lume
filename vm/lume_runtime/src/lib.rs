pub mod array;
pub mod io;
pub mod mem;
pub mod string;

pub const INTRINSIC_FUNCTIONS: &[(&str, *const u8)] = &[
    ("std::type_of", type_of as *const u8),
    ("std::mem::alloc", mem::lumert_alloc as *const u8),
    ("std::mem::realloc", mem::lumert_realloc as *const u8),
    ("std::mem::dealloc", mem::lumert_dealloc as *const u8),
    ("std::mem::ptr_read", mem::lumert_ptr_read as *const u8),
    ("std::mem::ptr_write", mem::lumert_ptr_write as *const u8),
    ("std::mem::GC::invoke", lume_gc::trigger_collection_force as *const u8),
    ("std::io::format", io::format as *const u8),
    ("std::io::print", io::print as *const u8),
    ("std::io::println", io::println as *const u8),
    ("std::Int8::to_string", io::int8_tostring as *const u8),
    ("std::Int16::to_string", io::int16_tostring as *const u8),
    ("std::Int32::to_string", io::int32_tostring as *const u8),
    ("std::Int64::to_string", io::int64_tostring as *const u8),
    ("std::Int8::to_string", io::uint8_tostring as *const u8),
    ("std::UInt16::to_string", io::uint16_tostring as *const u8),
    ("std::UInt32::to_string", io::uint32_tostring as *const u8),
    ("std::UInt64::to_string", io::uint64_tostring as *const u8),
    ("std::Float::to_string", io::float_tostring as *const u8),
    ("std::Double::to_string", io::double_tostring as *const u8),
];

/// Retrieves of the type metadata of the first type parameter.
///
/// Since Lume passes the metadata of type arguments after all other parameters,
/// we can safely return the first input pointer.
pub extern "C" fn type_of(metadata: *const ()) -> *const () {
    metadata
}
