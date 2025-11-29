pub mod l1;

use lume_errors::Result;

pub fn try_demangle(mangled: &str) -> Result<String> {
    if mangled.starts_with(l1::MANGLED_PREFIX) {
        l1::try_demangle(mangled)
    } else {
        // If the given string isn't mangled, we have nothing to demangle.
        return Ok(mangled.to_string());
    }
}
