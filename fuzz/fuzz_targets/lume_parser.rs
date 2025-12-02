#![no_main]

use std::sync::Arc;

use libfuzzer_sys::fuzz_target;
use lume_errors::DiagCtxHandle;
use lume_span::SourceFile;

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        let source = Arc::new(SourceFile::internal(s));

        let mut lexer = lume_lexer::Lexer::new(source.clone());
        let tokens = lexer.lex().unwrap();

        let mut parser = lume_parser::Parser::new(source, tokens, DiagCtxHandle::shim());
        parser.disable_recovery();

        let _ = parser.parse();
    }
});
