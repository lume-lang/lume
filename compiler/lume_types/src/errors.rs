use error_snippet_derive::Diagnostic;
use lume_hir::{Node, Path};
use lume_span::{Location, NodeId};

use crate::TypeKindRef;

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "could not find node {id:?} in context")]
pub struct NodeNotFound {
    pub id: NodeId,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "could not find item")]
pub struct TypeNameNotFound {
    pub name: Path,

    #[label(source, "could not find {name:+} in context")]
    pub location: Location,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "expected path {path:+} to have a parent")]
pub struct PathWithoutParent {
    pub path: Path,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "attempted to get return type of non-invokable type: {item:?}")]
pub struct ReturnTypeOnNoninvokableItem {
    pub item: Node,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "attempted to get type parameters on non-generic item: {id:?}")]
pub struct TypeParametersOnNonGenericItem {
    pub id: NodeId,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "attempted to get type parameters on non-generic type: {ty:?}")]
pub struct TypeParametersOnNonGenericType {
    pub ty: TypeKindRef,
}

#[derive(Diagnostic, Debug)]
#[diagnostic(message = "expected type to be kind of {expected:?}, found {found:?}")]
pub struct UnexpectedTypeKind {
    pub expected: TypeKindRef,
    pub found: TypeKindRef,
}
