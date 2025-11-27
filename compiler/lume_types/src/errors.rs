use error_snippet_derive::Diagnostic;
use lume_span::NodeId;

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "could not find node {id:?} in context")]
pub struct NodeNotFound {
    pub id: NodeId,
}
