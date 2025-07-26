#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        let mut parser = lume_parser::Parser::new_with_str(s);
        parser.disable_recovery();

        let _ = parser.parse_statements();
    }
});
