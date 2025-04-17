use std::collections::HashMap;

use crate::hir::{self, ItemId, NodeId};

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Map {
    /// Defines the mapping between local expressions and their type information.
    pub exprs: HashMap<NodeId, Expr>,
}

impl Map {
    pub fn new() -> Self {
        Map { exprs: HashMap::new() }
    }
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: Ty,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub enum ExprKind {
    Assignment { target: NodeId, value: NodeId },
    Call { fun: ItemId, args: Box<[NodeId]> },
    Literal { kind: LiteralKind },
    Member { target: NodeId, name: String },
    Var { id: NodeId },
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub struct Ty(pub Box<TyKind>);

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub enum TyKind {
    Class(ItemId),
    Bool,
    Int(IntKind),
    Float(FloatKind),
    Str,
    Array(Ty),
    Fn(Vec<Ty>, Ty),
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub enum IntKind {
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
    IPtr,
    UPtr,
}

impl From<hir::IntKind> for IntKind {
    fn from(kind: hir::IntKind) -> Self {
        match kind {
            hir::IntKind::I8 => IntKind::I8,
            hir::IntKind::U8 => IntKind::U8,
            hir::IntKind::I16 => IntKind::I16,
            hir::IntKind::U16 => IntKind::U16,
            hir::IntKind::I32 => IntKind::I32,
            hir::IntKind::U32 => IntKind::U32,
            hir::IntKind::I64 => IntKind::I64,
            hir::IntKind::U64 => IntKind::U64,
            hir::IntKind::IPtr => IntKind::IPtr,
            hir::IntKind::UPtr => IntKind::UPtr,
        }
    }
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub enum FloatKind {
    F32,
    F64,
}

impl From<hir::FloatKind> for FloatKind {
    fn from(kind: hir::FloatKind) -> Self {
        match kind {
            hir::FloatKind::F32 => FloatKind::F32,
            hir::FloatKind::F64 => FloatKind::F64,
        }
    }
}

#[derive(serde::Serialize, Debug, Clone, PartialEq)]
pub enum LiteralKind {
    Bool(bool),
    Int(i64, IntKind),
    Float(f64, FloatKind),
    Str(String),
}
