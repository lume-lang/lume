#![no_main]

use std::sync::Arc;

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    use lume_lexer::*;

    if let Ok(s) = std::str::from_utf8(data) {
        let source = lume_span::SourceFile::internal(s);
        let mut lexer = Lexer::new(Arc::new(source));

        loop {
            let Ok(token) = lexer.next_token() else {
                break;
            };

            if let TokenKind::Eof = token.kind {
                break;
            }
        }
    }
});
