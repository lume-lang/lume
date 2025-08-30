use error_snippet_derive::Diagnostic;
use lume_span::Location;

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "dynamic dispatch not implemented", code = "LM9999")]
pub struct DynamicDispatchUnimplemented {
    #[label(source, "dynamic is currently not implemented")]
    pub source: Location,
}
