//! DO NOT EDIT MANUALLY.
//!
//! This file is auto-generated from `tools/grammar/lume.ungram`. To
//! regenerate this file, run the following command:
//!
//! ```sh
//! cargo run --package grammar
//! ```
#![cfg_attr(rustfmt, rustfmt_skip)]
use crate::*;
use lume_syntax::*;
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Name {
    syntax: SyntaxNode,
}
impl Name {}
impl AstNode for Name {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::NAME => Some(Name { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct DocComment {
    syntax: SyntaxNode,
}
impl DocComment {}
impl AstNode for DocComment {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::DOC_COMMENT => Some(DocComment { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct SourceFile {
    syntax: SyntaxNode,
}
impl SourceFile {
    pub fn item(&self) -> AstChildren<Item> {
        crate::support::children(self.syntax())
    }
}
impl AstNode for SourceFile {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::SOURCE_FILE => Some(SourceFile { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub enum Item {
    Import(Import),
    Namespace(Namespace),
    Fn(Fn),
    Struct(Struct),
    Trait(Trait),
    Enum(Enum),
    Impl(Impl),
    TraitImpl(TraitImpl),
}
impl AstNode for Item {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::IMPORT => Some(Self::Import(Import::cast(syntax).unwrap())),
            SyntaxKind::NAMESPACE => {
                Some(Self::Namespace(Namespace::cast(syntax).unwrap()))
            }
            SyntaxKind::FN => Some(Self::Fn(Fn::cast(syntax).unwrap())),
            SyntaxKind::STRUCT => Some(Self::Struct(Struct::cast(syntax).unwrap())),
            SyntaxKind::TRAIT => Some(Self::Trait(Trait::cast(syntax).unwrap())),
            SyntaxKind::ENUM => Some(Self::Enum(Enum::cast(syntax).unwrap())),
            SyntaxKind::IMPL => Some(Self::Impl(Impl::cast(syntax).unwrap())),
            SyntaxKind::TRAIT_IMPL => {
                Some(Self::TraitImpl(TraitImpl::cast(syntax).unwrap()))
            }
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::Import(it) => it.syntax(),
            Self::Namespace(it) => it.syntax(),
            Self::Fn(it) => it.syntax(),
            Self::Struct(it) => it.syntax(),
            Self::Trait(it) => it.syntax(),
            Self::Enum(it) => it.syntax(),
            Self::Impl(it) => it.syntax(),
            Self::TraitImpl(it) => it.syntax(),
        }
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Import {
    syntax: SyntaxNode,
}
impl Import {
    pub fn import_kw(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::IMPORT_KW)
    }
    pub fn import_path(&self) -> Option<ImportPath> {
        crate::support::child(self.syntax())
    }
    pub fn import_list(&self) -> Option<ImportList> {
        crate::support::child(self.syntax())
    }
}
impl AstNode for Import {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::IMPORT => Some(Import { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Namespace {
    syntax: SyntaxNode,
}
impl Namespace {
    pub fn namespace_kw(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::NAMESPACE_KW)
    }
    pub fn import_path(&self) -> Option<ImportPath> {
        crate::support::child(self.syntax())
    }
}
impl AstNode for Namespace {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::NAMESPACE => Some(Namespace { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Fn {
    syntax: SyntaxNode,
}
impl Fn {
    pub fn visibility(&self) -> Option<Visibility> {
        crate::support::child(self.syntax())
    }
    pub fn sig(&self) -> Option<Sig> {
        crate::support::child(self.syntax())
    }
    pub fn block(&self) -> Option<Block> {
        crate::support::child(self.syntax())
    }
    pub fn doc_comments(&self) -> AstChildren<DocComment> {
        crate::support::children(self.syntax())
    }
    pub fn attr(&self) -> AstChildren<Attr> {
        crate::support::children(self.syntax())
    }
}
impl AstNode for Fn {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::FN => Some(Fn { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Struct {
    syntax: SyntaxNode,
}
impl Struct {
    pub fn struct_kw(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::STRUCT_KW)
    }
    pub fn left_brace(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::LEFT_BRACE)
    }
    pub fn right_brace(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::RIGHT_BRACE)
    }
    pub fn visibility(&self) -> Option<Visibility> {
        crate::support::child(self.syntax())
    }
    pub fn name(&self) -> Option<Name> {
        crate::support::child(self.syntax())
    }
    pub fn bound_types(&self) -> Option<BoundTypes> {
        crate::support::child(self.syntax())
    }
    pub fn doc_comments(&self) -> AstChildren<DocComment> {
        crate::support::children(self.syntax())
    }
    pub fn attr(&self) -> AstChildren<Attr> {
        crate::support::children(self.syntax())
    }
    pub fn fields(&self) -> AstChildren<Field> {
        crate::support::children(self.syntax())
    }
}
impl AstNode for Struct {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::STRUCT => Some(Struct { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Trait {
    syntax: SyntaxNode,
}
impl Trait {
    pub fn trait_kw(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::TRAIT_KW)
    }
    pub fn left_brace(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::LEFT_BRACE)
    }
    pub fn right_brace(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::RIGHT_BRACE)
    }
    pub fn visibility(&self) -> Option<Visibility> {
        crate::support::child(self.syntax())
    }
    pub fn name(&self) -> Option<Name> {
        crate::support::child(self.syntax())
    }
    pub fn bound_types(&self) -> Option<BoundTypes> {
        crate::support::child(self.syntax())
    }
    pub fn doc_comments(&self) -> AstChildren<DocComment> {
        crate::support::children(self.syntax())
    }
    pub fn attr(&self) -> AstChildren<Attr> {
        crate::support::children(self.syntax())
    }
    pub fn methods(&self) -> AstChildren<Method> {
        crate::support::children(self.syntax())
    }
}
impl AstNode for Trait {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::TRAIT => Some(Trait { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Enum {
    syntax: SyntaxNode,
}
impl Enum {
    pub fn enum_kw(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::ENUM_KW)
    }
    pub fn left_brace(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::LEFT_BRACE)
    }
    pub fn right_brace(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::RIGHT_BRACE)
    }
    pub fn visibility(&self) -> Option<Visibility> {
        crate::support::child(self.syntax())
    }
    pub fn name(&self) -> Option<Name> {
        crate::support::child(self.syntax())
    }
    pub fn bound_types(&self) -> Option<BoundTypes> {
        crate::support::child(self.syntax())
    }
    pub fn doc_comments(&self) -> AstChildren<DocComment> {
        crate::support::children(self.syntax())
    }
    pub fn attr(&self) -> AstChildren<Attr> {
        crate::support::children(self.syntax())
    }
    pub fn cases(&self) -> AstChildren<Case> {
        crate::support::children(self.syntax())
    }
}
impl AstNode for Enum {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ENUM => Some(Enum { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Impl {
    syntax: SyntaxNode,
}
impl Impl {
    pub fn impl_kw(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::IMPL_KW)
    }
    pub fn left_brace(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::LEFT_BRACE)
    }
    pub fn right_brace(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::RIGHT_BRACE)
    }
    pub fn bound_types(&self) -> Option<BoundTypes> {
        crate::support::child(self.syntax())
    }
    pub fn target(&self) -> Option<Type> {
        crate::support::child(self.syntax())
    }
    pub fn doc_comments(&self) -> AstChildren<DocComment> {
        crate::support::children(self.syntax())
    }
    pub fn attr(&self) -> AstChildren<Attr> {
        crate::support::children(self.syntax())
    }
    pub fn methods(&self) -> AstChildren<Method> {
        crate::support::children(self.syntax())
    }
}
impl AstNode for Impl {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::IMPL => Some(Impl { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct TraitImpl {
    syntax: SyntaxNode,
}
impl TraitImpl {
    pub fn use_kw(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::USE_KW)
    }
    pub fn colon(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::COLON)
    }
    pub fn left_brace(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::LEFT_BRACE)
    }
    pub fn right_brace(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::RIGHT_BRACE)
    }
    pub fn bound_types(&self) -> Option<BoundTypes> {
        crate::support::child(self.syntax())
    }
    pub fn __trait(&self) -> Option<Type> {
        crate::support::child(self.syntax())
    }
    pub fn __target(&self) -> Option<Type> {
        crate::support::child(self.syntax())
    }
    pub fn doc_comments(&self) -> AstChildren<DocComment> {
        crate::support::children(self.syntax())
    }
    pub fn attr(&self) -> AstChildren<Attr> {
        crate::support::children(self.syntax())
    }
    pub fn methods(&self) -> AstChildren<Method> {
        crate::support::children(self.syntax())
    }
}
impl AstNode for TraitImpl {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::TRAIT_IMPL => Some(TraitImpl { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Visibility {
    syntax: SyntaxNode,
}
impl Visibility {
    pub fn pub_kw(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::PUB_KW)
    }
    pub fn internal_kw(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::INTERNAL_KW)
    }
    pub fn priv_kw(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::PRIV_KW)
    }
}
impl AstNode for Visibility {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::VISIBILITY => Some(Visibility { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct ImportPath {
    syntax: SyntaxNode,
}
impl ImportPath {
    pub fn path(&self) -> AstChildren<Name> {
        crate::support::children(self.syntax())
    }
}
impl AstNode for ImportPath {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::IMPORT_PATH => Some(ImportPath { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct ImportList {
    syntax: SyntaxNode,
}
impl ImportList {
    pub fn left_paren(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::LEFT_PAREN)
    }
    pub fn right_paren(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::RIGHT_PAREN)
    }
    pub fn items(&self) -> AstChildren<Name> {
        crate::support::children(self.syntax())
    }
}
impl AstNode for ImportList {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::IMPORT_LIST => Some(ImportList { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Attr {
    syntax: SyntaxNode,
}
impl Attr {
    pub fn not(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::NOT)
    }
    pub fn left_bracket(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::LEFT_BRACKET)
    }
    pub fn right_bracket(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::RIGHT_BRACKET)
    }
    pub fn name(&self) -> Option<Name> {
        crate::support::child(self.syntax())
    }
    pub fn arg_list(&self) -> Option<AttrArgList> {
        crate::support::child(self.syntax())
    }
}
impl AstNode for Attr {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ATTR => Some(Attr { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Sig {
    syntax: SyntaxNode,
}
impl Sig {
    pub fn fn_kw(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::FN_KW)
    }
    pub fn unsafe_kw(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::UNSAFE_KW)
    }
    pub fn extern_kw(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::EXTERN_KW)
    }
    pub fn name(&self) -> Option<Name> {
        crate::support::child(self.syntax())
    }
    pub fn bound_types(&self) -> Option<BoundTypes> {
        crate::support::child(self.syntax())
    }
    pub fn param_list(&self) -> Option<ParamList> {
        crate::support::child(self.syntax())
    }
    pub fn return_type(&self) -> Option<ReturnType> {
        crate::support::child(self.syntax())
    }
}
impl AstNode for Sig {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::SIG => Some(Sig { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Block {
    syntax: SyntaxNode,
}
impl Block {
    pub fn left_brace(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::LEFT_BRACE)
    }
    pub fn right_brace(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::RIGHT_BRACE)
    }
    pub fn stmt(&self) -> AstChildren<Stmt> {
        crate::support::children(self.syntax())
    }
}
impl AstNode for Block {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::BLOCK => Some(Block { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Method {
    syntax: SyntaxNode,
}
impl Method {
    pub fn visibility(&self) -> Option<Visibility> {
        crate::support::child(self.syntax())
    }
    pub fn sig(&self) -> Option<Sig> {
        crate::support::child(self.syntax())
    }
    pub fn block(&self) -> Option<Block> {
        crate::support::child(self.syntax())
    }
    pub fn doc_comments(&self) -> AstChildren<DocComment> {
        crate::support::children(self.syntax())
    }
    pub fn attr(&self) -> AstChildren<Attr> {
        crate::support::children(self.syntax())
    }
}
impl AstNode for Method {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::METHOD => Some(Method { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct BoundTypes {
    syntax: SyntaxNode,
}
impl BoundTypes {
    pub fn less(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::LESS)
    }
    pub fn greater(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::GREATER)
    }
    pub fn types(&self) -> AstChildren<BoundType> {
        crate::support::children(self.syntax())
    }
}
impl AstNode for BoundTypes {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::BOUND_TYPES => Some(BoundTypes { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct ParamList {
    syntax: SyntaxNode,
}
impl ParamList {
    pub fn left_paren(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::LEFT_PAREN)
    }
    pub fn right_paren(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::RIGHT_PAREN)
    }
    pub fn param(&self) -> AstChildren<Param> {
        crate::support::children(self.syntax())
    }
}
impl AstNode for ParamList {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::PARAM_LIST => Some(ParamList { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct ReturnType {
    syntax: SyntaxNode,
}
impl ReturnType {
    pub fn arrow(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::ARROW)
    }
    pub fn ty(&self) -> Option<Type> {
        crate::support::child(self.syntax())
    }
}
impl AstNode for ReturnType {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::RETURN_TYPE => Some(ReturnType { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Param {
    syntax: SyntaxNode,
}
impl Param {
    pub fn vararg(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::DOT_DOT_DOT)
    }
    pub fn colon(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::COLON)
    }
    pub fn self_expr(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::SELF_EXPR)
    }
    pub fn name(&self) -> Option<Name> {
        crate::support::child(self.syntax())
    }
    pub fn ty(&self) -> Option<Type> {
        crate::support::child(self.syntax())
    }
}
impl AstNode for Param {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::PARAM => Some(Param { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub enum Type {
    NamedType(NamedType),
    ArrayType(ArrayType),
    SelfType(SelfType),
    PointerType(PointerType),
}
impl AstNode for Type {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::NAMED_TYPE => {
                Some(Self::NamedType(NamedType::cast(syntax).unwrap()))
            }
            SyntaxKind::ARRAY_TYPE => {
                Some(Self::ArrayType(ArrayType::cast(syntax).unwrap()))
            }
            SyntaxKind::SELF_TYPE => {
                Some(Self::SelfType(SelfType::cast(syntax).unwrap()))
            }
            SyntaxKind::POINTER_TYPE => {
                Some(Self::PointerType(PointerType::cast(syntax).unwrap()))
            }
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::NamedType(it) => it.syntax(),
            Self::ArrayType(it) => it.syntax(),
            Self::SelfType(it) => it.syntax(),
            Self::PointerType(it) => it.syntax(),
        }
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Field {
    syntax: SyntaxNode,
}
impl Field {
    pub fn colon(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::COLON)
    }
    pub fn assign(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::ASSIGN)
    }
    pub fn semicolon(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::SEMICOLON)
    }
    pub fn visibility(&self) -> Option<Visibility> {
        crate::support::child(self.syntax())
    }
    pub fn name(&self) -> Option<Name> {
        crate::support::child(self.syntax())
    }
    pub fn field_type(&self) -> Option<Type> {
        crate::support::child(self.syntax())
    }
    pub fn default_value(&self) -> Option<Expr> {
        crate::support::child(self.syntax())
    }
    pub fn doc_comments(&self) -> AstChildren<DocComment> {
        crate::support::children(self.syntax())
    }
    pub fn attr(&self) -> AstChildren<Attr> {
        crate::support::children(self.syntax())
    }
}
impl AstNode for Field {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::FIELD => Some(Field { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    ArrayExpr(ArrayExpr),
    AssignmentExpr(AssignmentExpr),
    InstanceCallExpr(InstanceCallExpr),
    StaticCallExpr(StaticCallExpr),
    CastExpr(CastExpr),
    ConstructExpr(ConstructExpr),
    IfExpr(IfExpr),
    IsExpr(IsExpr),
    LitExpr(LitExpr),
    MemberExpr(MemberExpr),
    RangeExpr(RangeExpr),
    ScopeExpr(ScopeExpr),
    SwitchExpr(SwitchExpr),
    ParenExpr(ParenExpr),
    UnsafeExpr(UnsafeExpr),
    VariableExpr(VariableExpr),
    VariantExpr(VariantExpr),
    BinExpr(BinExpr),
    UnaryExpr(UnaryExpr),
    PostfixExpr(PostfixExpr),
}
impl AstNode for Expr {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ARRAY_EXPR => {
                Some(Self::ArrayExpr(ArrayExpr::cast(syntax).unwrap()))
            }
            SyntaxKind::ASSIGNMENT_EXPR => {
                Some(Self::AssignmentExpr(AssignmentExpr::cast(syntax).unwrap()))
            }
            SyntaxKind::INSTANCE_CALL_EXPR => {
                Some(Self::InstanceCallExpr(InstanceCallExpr::cast(syntax).unwrap()))
            }
            SyntaxKind::STATIC_CALL_EXPR => {
                Some(Self::StaticCallExpr(StaticCallExpr::cast(syntax).unwrap()))
            }
            SyntaxKind::CAST_EXPR => {
                Some(Self::CastExpr(CastExpr::cast(syntax).unwrap()))
            }
            SyntaxKind::CONSTRUCT_EXPR => {
                Some(Self::ConstructExpr(ConstructExpr::cast(syntax).unwrap()))
            }
            SyntaxKind::IF_EXPR => Some(Self::IfExpr(IfExpr::cast(syntax).unwrap())),
            SyntaxKind::IS_EXPR => Some(Self::IsExpr(IsExpr::cast(syntax).unwrap())),
            SyntaxKind::LIT_EXPR => Some(Self::LitExpr(LitExpr::cast(syntax).unwrap())),
            SyntaxKind::MEMBER_EXPR => {
                Some(Self::MemberExpr(MemberExpr::cast(syntax).unwrap()))
            }
            SyntaxKind::RANGE_EXPR => {
                Some(Self::RangeExpr(RangeExpr::cast(syntax).unwrap()))
            }
            SyntaxKind::SCOPE_EXPR => {
                Some(Self::ScopeExpr(ScopeExpr::cast(syntax).unwrap()))
            }
            SyntaxKind::SWITCH_EXPR => {
                Some(Self::SwitchExpr(SwitchExpr::cast(syntax).unwrap()))
            }
            SyntaxKind::PAREN_EXPR => {
                Some(Self::ParenExpr(ParenExpr::cast(syntax).unwrap()))
            }
            SyntaxKind::UNSAFE_EXPR => {
                Some(Self::UnsafeExpr(UnsafeExpr::cast(syntax).unwrap()))
            }
            SyntaxKind::VARIABLE_EXPR => {
                Some(Self::VariableExpr(VariableExpr::cast(syntax).unwrap()))
            }
            SyntaxKind::VARIANT_EXPR => {
                Some(Self::VariantExpr(VariantExpr::cast(syntax).unwrap()))
            }
            SyntaxKind::BIN_EXPR => Some(Self::BinExpr(BinExpr::cast(syntax).unwrap())),
            SyntaxKind::UNARY_EXPR => {
                Some(Self::UnaryExpr(UnaryExpr::cast(syntax).unwrap()))
            }
            SyntaxKind::POSTFIX_EXPR => {
                Some(Self::PostfixExpr(PostfixExpr::cast(syntax).unwrap()))
            }
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::ArrayExpr(it) => it.syntax(),
            Self::AssignmentExpr(it) => it.syntax(),
            Self::InstanceCallExpr(it) => it.syntax(),
            Self::StaticCallExpr(it) => it.syntax(),
            Self::CastExpr(it) => it.syntax(),
            Self::ConstructExpr(it) => it.syntax(),
            Self::IfExpr(it) => it.syntax(),
            Self::IsExpr(it) => it.syntax(),
            Self::LitExpr(it) => it.syntax(),
            Self::MemberExpr(it) => it.syntax(),
            Self::RangeExpr(it) => it.syntax(),
            Self::ScopeExpr(it) => it.syntax(),
            Self::SwitchExpr(it) => it.syntax(),
            Self::ParenExpr(it) => it.syntax(),
            Self::UnsafeExpr(it) => it.syntax(),
            Self::VariableExpr(it) => it.syntax(),
            Self::VariantExpr(it) => it.syntax(),
            Self::BinExpr(it) => it.syntax(),
            Self::UnaryExpr(it) => it.syntax(),
            Self::PostfixExpr(it) => it.syntax(),
        }
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Case {
    syntax: SyntaxNode,
}
impl Case {
    pub fn name(&self) -> Option<Name> {
        crate::support::child(self.syntax())
    }
    pub fn case_param_list(&self) -> Option<CaseParamList> {
        crate::support::child(self.syntax())
    }
    pub fn doc_comments(&self) -> AstChildren<DocComment> {
        crate::support::children(self.syntax())
    }
}
impl AstNode for Case {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::CASE => Some(Case { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct CaseParamList {
    syntax: SyntaxNode,
}
impl CaseParamList {
    pub fn left_paren(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::LEFT_PAREN)
    }
    pub fn right_paren(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::RIGHT_PAREN)
    }
    pub fn ty(&self) -> AstChildren<Type> {
        crate::support::children(self.syntax())
    }
}
impl AstNode for CaseParamList {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::CASE_PARAM_LIST => Some(CaseParamList { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct AttrArgList {
    syntax: SyntaxNode,
}
impl AttrArgList {
    pub fn left_paren(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::LEFT_PAREN)
    }
    pub fn right_paren(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::RIGHT_PAREN)
    }
    pub fn args(&self) -> AstChildren<AttrArg> {
        crate::support::children(self.syntax())
    }
}
impl AstNode for AttrArgList {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ATTR_ARG_LIST => Some(AttrArgList { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct AttrArg {
    syntax: SyntaxNode,
}
impl AttrArg {
    pub fn assign(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::ASSIGN)
    }
    pub fn name(&self) -> Option<Name> {
        crate::support::child(self.syntax())
    }
    pub fn value(&self) -> Option<Literal> {
        crate::support::child(self.syntax())
    }
}
impl AstNode for AttrArg {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ATTR_ARG => Some(AttrArg { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    IntegerLit(IntegerLit),
    FloatLit(FloatLit),
    BooleanLit(BooleanLit),
    StringLit(StringLit),
}
impl AstNode for Literal {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::INTEGER_LIT => {
                Some(Self::IntegerLit(IntegerLit::cast(syntax).unwrap()))
            }
            SyntaxKind::FLOAT_LIT => {
                Some(Self::FloatLit(FloatLit::cast(syntax).unwrap()))
            }
            SyntaxKind::BOOLEAN_LIT => {
                Some(Self::BooleanLit(BooleanLit::cast(syntax).unwrap()))
            }
            SyntaxKind::STRING_LIT => {
                Some(Self::StringLit(StringLit::cast(syntax).unwrap()))
            }
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::IntegerLit(it) => it.syntax(),
            Self::FloatLit(it) => it.syntax(),
            Self::BooleanLit(it) => it.syntax(),
            Self::StringLit(it) => it.syntax(),
        }
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    LetStmt(LetStmt),
    BreakStmt(BreakStmt),
    ContinueStmt(ContinueStmt),
    FinalStmt(FinalStmt),
    ReturnStmt(ReturnStmt),
    LoopStmt(LoopStmt),
    ForStmt(ForStmt),
    WhileStmt(WhileStmt),
    ExprStmt(ExprStmt),
}
impl AstNode for Stmt {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::LET_STMT => Some(Self::LetStmt(LetStmt::cast(syntax).unwrap())),
            SyntaxKind::BREAK_STMT => {
                Some(Self::BreakStmt(BreakStmt::cast(syntax).unwrap()))
            }
            SyntaxKind::CONTINUE_STMT => {
                Some(Self::ContinueStmt(ContinueStmt::cast(syntax).unwrap()))
            }
            SyntaxKind::FINAL_STMT => {
                Some(Self::FinalStmt(FinalStmt::cast(syntax).unwrap()))
            }
            SyntaxKind::RETURN_STMT => {
                Some(Self::ReturnStmt(ReturnStmt::cast(syntax).unwrap()))
            }
            SyntaxKind::LOOP_STMT => {
                Some(Self::LoopStmt(LoopStmt::cast(syntax).unwrap()))
            }
            SyntaxKind::FOR_STMT => Some(Self::ForStmt(ForStmt::cast(syntax).unwrap())),
            SyntaxKind::WHILE_STMT => {
                Some(Self::WhileStmt(WhileStmt::cast(syntax).unwrap()))
            }
            SyntaxKind::EXPR_STMT => {
                Some(Self::ExprStmt(ExprStmt::cast(syntax).unwrap()))
            }
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::LetStmt(it) => it.syntax(),
            Self::BreakStmt(it) => it.syntax(),
            Self::ContinueStmt(it) => it.syntax(),
            Self::FinalStmt(it) => it.syntax(),
            Self::ReturnStmt(it) => it.syntax(),
            Self::LoopStmt(it) => it.syntax(),
            Self::ForStmt(it) => it.syntax(),
            Self::WhileStmt(it) => it.syntax(),
            Self::ExprStmt(it) => it.syntax(),
        }
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct LetStmt {
    syntax: SyntaxNode,
}
impl LetStmt {
    pub fn let_kw(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::LET_KW)
    }
    pub fn colon(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::COLON)
    }
    pub fn assign(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::ASSIGN)
    }
    pub fn semicolon(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::SEMICOLON)
    }
    pub fn name(&self) -> Option<Name> {
        crate::support::child(self.syntax())
    }
    pub fn ty(&self) -> Option<Type> {
        crate::support::child(self.syntax())
    }
    pub fn expr(&self) -> Option<Expr> {
        crate::support::child(self.syntax())
    }
}
impl AstNode for LetStmt {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::LET_STMT => Some(LetStmt { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct BreakStmt {
    syntax: SyntaxNode,
}
impl BreakStmt {
    pub fn break_kw(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::BREAK_KW)
    }
    pub fn semicolon(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::SEMICOLON)
    }
}
impl AstNode for BreakStmt {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::BREAK_STMT => Some(BreakStmt { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct ContinueStmt {
    syntax: SyntaxNode,
}
impl ContinueStmt {
    pub fn continue_kw(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::CONTINUE_KW)
    }
    pub fn semicolon(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::SEMICOLON)
    }
}
impl AstNode for ContinueStmt {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::CONTINUE_STMT => Some(ContinueStmt { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct FinalStmt {
    syntax: SyntaxNode,
}
impl FinalStmt {
    pub fn expr(&self) -> Option<Expr> {
        crate::support::child(self.syntax())
    }
}
impl AstNode for FinalStmt {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::FINAL_STMT => Some(FinalStmt { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct ReturnStmt {
    syntax: SyntaxNode,
}
impl ReturnStmt {
    pub fn return_kw(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::RETURN_KW)
    }
    pub fn semicolon(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::SEMICOLON)
    }
    pub fn expr(&self) -> Option<Expr> {
        crate::support::child(self.syntax())
    }
}
impl AstNode for ReturnStmt {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::RETURN_STMT => Some(ReturnStmt { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct LoopStmt {
    syntax: SyntaxNode,
}
impl LoopStmt {
    pub fn loop_kw(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::LOOP_KW)
    }
    pub fn block(&self) -> Option<Block> {
        crate::support::child(self.syntax())
    }
}
impl AstNode for LoopStmt {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::LOOP_STMT => Some(LoopStmt { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct ForStmt {
    syntax: SyntaxNode,
}
impl ForStmt {
    pub fn for_kw(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::FOR_KW)
    }
    pub fn in_kw(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::IN_KW)
    }
    pub fn pattern(&self) -> Option<Name> {
        crate::support::child(self.syntax())
    }
    pub fn collection(&self) -> Option<Expr> {
        crate::support::child(self.syntax())
    }
    pub fn block(&self) -> Option<Block> {
        crate::support::child(self.syntax())
    }
}
impl AstNode for ForStmt {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::FOR_STMT => Some(ForStmt { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct WhileStmt {
    syntax: SyntaxNode,
}
impl WhileStmt {
    pub fn while_kw(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::WHILE_KW)
    }
    pub fn condition(&self) -> Option<Expr> {
        crate::support::child(self.syntax())
    }
    pub fn block(&self) -> Option<Block> {
        crate::support::child(self.syntax())
    }
}
impl AstNode for WhileStmt {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::WHILE_STMT => Some(WhileStmt { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct ExprStmt {
    syntax: SyntaxNode,
}
impl ExprStmt {
    pub fn semicolon(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::SEMICOLON)
    }
    pub fn expr(&self) -> Option<Expr> {
        crate::support::child(self.syntax())
    }
}
impl AstNode for ExprStmt {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::EXPR_STMT => Some(ExprStmt { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct ArrayExpr {
    syntax: SyntaxNode,
}
impl ArrayExpr {
    pub fn left_bracket(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::LEFT_BRACKET)
    }
    pub fn right_bracket(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::RIGHT_BRACKET)
    }
    pub fn items(&self) -> AstChildren<Expr> {
        crate::support::children(self.syntax())
    }
}
impl AstNode for ArrayExpr {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ARRAY_EXPR => Some(ArrayExpr { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct AssignmentExpr {
    syntax: SyntaxNode,
}
impl AssignmentExpr {
    pub fn assign(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::ASSIGN)
    }
    pub fn __target(&self) -> Option<Expr> {
        crate::support::child(self.syntax())
    }
    pub fn __value(&self) -> Option<Expr> {
        crate::support::child(self.syntax())
    }
}
impl AstNode for AssignmentExpr {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ASSIGNMENT_EXPR => Some(AssignmentExpr { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct InstanceCallExpr {
    syntax: SyntaxNode,
}
impl InstanceCallExpr {
    pub fn dot(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::DOT)
    }
    pub fn callee(&self) -> Option<Expr> {
        crate::support::child(self.syntax())
    }
    pub fn name(&self) -> Option<Name> {
        crate::support::child(self.syntax())
    }
    pub fn generic_args(&self) -> Option<GenericArgs> {
        crate::support::child(self.syntax())
    }
    pub fn arg_list(&self) -> Option<ArgList> {
        crate::support::child(self.syntax())
    }
}
impl AstNode for InstanceCallExpr {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::INSTANCE_CALL_EXPR => Some(InstanceCallExpr { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct StaticCallExpr {
    syntax: SyntaxNode,
}
impl StaticCallExpr {
    pub fn path(&self) -> Option<Path> {
        crate::support::child(self.syntax())
    }
    pub fn arg_list(&self) -> Option<ArgList> {
        crate::support::child(self.syntax())
    }
}
impl AstNode for StaticCallExpr {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::STATIC_CALL_EXPR => Some(StaticCallExpr { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct CastExpr {
    syntax: SyntaxNode,
}
impl CastExpr {
    pub fn as_kw(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::AS_KW)
    }
    pub fn expr(&self) -> Option<Expr> {
        crate::support::child(self.syntax())
    }
    pub fn ty(&self) -> Option<Type> {
        crate::support::child(self.syntax())
    }
}
impl AstNode for CastExpr {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::CAST_EXPR => Some(CastExpr { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct ConstructExpr {
    syntax: SyntaxNode,
}
impl ConstructExpr {
    pub fn left_brace(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::LEFT_BRACE)
    }
    pub fn right_brace(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::RIGHT_BRACE)
    }
    pub fn ty(&self) -> Option<Path> {
        crate::support::child(self.syntax())
    }
    pub fn fields(&self) -> AstChildren<ConstructorField> {
        crate::support::children(self.syntax())
    }
}
impl AstNode for ConstructExpr {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::CONSTRUCT_EXPR => Some(ConstructExpr { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct IfExpr {
    syntax: SyntaxNode,
}
impl IfExpr {
    pub fn cases(&self) -> AstChildren<Condition> {
        crate::support::children(self.syntax())
    }
}
impl AstNode for IfExpr {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::IF_EXPR => Some(IfExpr { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct IsExpr {
    syntax: SyntaxNode,
}
impl IsExpr {
    pub fn is_kw(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::IS_KW)
    }
    pub fn expr(&self) -> Option<Expr> {
        crate::support::child(self.syntax())
    }
    pub fn pat(&self) -> Option<Pat> {
        crate::support::child(self.syntax())
    }
}
impl AstNode for IsExpr {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::IS_EXPR => Some(IsExpr { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct LitExpr {
    syntax: SyntaxNode,
}
impl LitExpr {
    pub fn literal(&self) -> Option<Literal> {
        crate::support::child(self.syntax())
    }
}
impl AstNode for LitExpr {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::LIT_EXPR => Some(LitExpr { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct MemberExpr {
    syntax: SyntaxNode,
}
impl MemberExpr {
    pub fn dot(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::DOT)
    }
    pub fn expr(&self) -> Option<Expr> {
        crate::support::child(self.syntax())
    }
    pub fn name(&self) -> Option<Name> {
        crate::support::child(self.syntax())
    }
}
impl AstNode for MemberExpr {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::MEMBER_EXPR => Some(MemberExpr { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct RangeExpr {
    syntax: SyntaxNode,
}
impl RangeExpr {
    pub fn dot_dot(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::DOT_DOT)
    }
    pub fn assign(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::ASSIGN)
    }
    pub fn __lower(&self) -> Option<Expr> {
        crate::support::child(self.syntax())
    }
    pub fn __upper(&self) -> Option<Expr> {
        crate::support::child(self.syntax())
    }
}
impl AstNode for RangeExpr {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::RANGE_EXPR => Some(RangeExpr { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct ScopeExpr {
    syntax: SyntaxNode,
}
impl ScopeExpr {
    pub fn block(&self) -> Option<Block> {
        crate::support::child(self.syntax())
    }
}
impl AstNode for ScopeExpr {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::SCOPE_EXPR => Some(ScopeExpr { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct SwitchExpr {
    syntax: SyntaxNode,
}
impl SwitchExpr {
    pub fn switch_kw(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::SWITCH_KW)
    }
    pub fn left_brace(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::LEFT_BRACE)
    }
    pub fn right_brace(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::RIGHT_BRACE)
    }
    pub fn expr(&self) -> Option<Expr> {
        crate::support::child(self.syntax())
    }
    pub fn arms(&self) -> AstChildren<SwitchArm> {
        crate::support::children(self.syntax())
    }
}
impl AstNode for SwitchExpr {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::SWITCH_EXPR => Some(SwitchExpr { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct ParenExpr {
    syntax: SyntaxNode,
}
impl ParenExpr {
    pub fn left_paren(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::LEFT_PAREN)
    }
    pub fn right_paren(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::RIGHT_PAREN)
    }
    pub fn expr(&self) -> Option<Expr> {
        crate::support::child(self.syntax())
    }
}
impl AstNode for ParenExpr {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::PAREN_EXPR => Some(ParenExpr { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct UnsafeExpr {
    syntax: SyntaxNode,
}
impl UnsafeExpr {
    pub fn unsafe_kw(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::UNSAFE_KW)
    }
    pub fn block(&self) -> Option<Block> {
        crate::support::child(self.syntax())
    }
}
impl AstNode for UnsafeExpr {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::UNSAFE_EXPR => Some(UnsafeExpr { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct VariableExpr {
    syntax: SyntaxNode,
}
impl VariableExpr {
    pub fn name(&self) -> Option<Name> {
        crate::support::child(self.syntax())
    }
}
impl AstNode for VariableExpr {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::VARIABLE_EXPR => Some(VariableExpr { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct VariantExpr {
    syntax: SyntaxNode,
}
impl VariantExpr {
    pub fn path(&self) -> Option<Path> {
        crate::support::child(self.syntax())
    }
    pub fn arg_list(&self) -> Option<ArgList> {
        crate::support::child(self.syntax())
    }
}
impl AstNode for VariantExpr {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::VARIANT_EXPR => Some(VariantExpr { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct BinExpr {
    syntax: SyntaxNode,
}
impl BinExpr {
    pub fn binary_and(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::BINARY_AND)
    }
    pub fn binary_or(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::BINARY_OR)
    }
    pub fn binary_xor(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::BINARY_XOR)
    }
    pub fn add(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::ADD)
    }
    pub fn sub(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::SUB)
    }
    pub fn mul(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::MUL)
    }
    pub fn div(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::DIV)
    }
    pub fn and(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::AND)
    }
    pub fn or(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::OR)
    }
    pub fn equal(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::EQUAL)
    }
    pub fn nequal(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::NEQUAL)
    }
    pub fn less(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::LESS)
    }
    pub fn lequal(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::LEQUAL)
    }
    pub fn greater(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::GREATER)
    }
    pub fn gequal(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::GEQUAL)
    }
    pub fn __lhs(&self) -> Option<Expr> {
        crate::support::child(self.syntax())
    }
    pub fn __rhs(&self) -> Option<Expr> {
        crate::support::child(self.syntax())
    }
}
impl AstNode for BinExpr {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::BIN_EXPR => Some(BinExpr { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct UnaryExpr {
    syntax: SyntaxNode,
}
impl UnaryExpr {
    pub fn not(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::NOT)
    }
    pub fn sub(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::SUB)
    }
    pub fn expr(&self) -> Option<Expr> {
        crate::support::child(self.syntax())
    }
}
impl AstNode for UnaryExpr {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::UNARY_EXPR => Some(UnaryExpr { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct PostfixExpr {
    syntax: SyntaxNode,
}
impl PostfixExpr {
    pub fn increment(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::INCREMENT)
    }
    pub fn decrement(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::DECREMENT)
    }
    pub fn expr(&self) -> Option<Expr> {
        crate::support::child(self.syntax())
    }
}
impl AstNode for PostfixExpr {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::POSTFIX_EXPR => Some(PostfixExpr { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct GenericArgs {
    syntax: SyntaxNode,
}
impl GenericArgs {
    pub fn less(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::LESS)
    }
    pub fn greater(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::GREATER)
    }
    pub fn args(&self) -> AstChildren<Type> {
        crate::support::children(self.syntax())
    }
}
impl AstNode for GenericArgs {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::GENERIC_ARGS => Some(GenericArgs { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct ArgList {
    syntax: SyntaxNode,
}
impl ArgList {
    pub fn left_paren(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::LEFT_PAREN)
    }
    pub fn right_paren(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::RIGHT_PAREN)
    }
    pub fn expr(&self) -> AstChildren<Expr> {
        crate::support::children(self.syntax())
    }
}
impl AstNode for ArgList {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ARG_LIST => Some(ArgList { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Path {
    syntax: SyntaxNode,
}
impl Path {
    pub fn path_segment(&self) -> AstChildren<PathSegment> {
        crate::support::children(self.syntax())
    }
}
impl AstNode for Path {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::PATH => Some(Path { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct ConstructorField {
    syntax: SyntaxNode,
}
impl ConstructorField {
    pub fn assign(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::ASSIGN)
    }
    pub fn semicolon(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::SEMICOLON)
    }
    pub fn name(&self) -> Option<Name> {
        crate::support::child(self.syntax())
    }
    pub fn value(&self) -> Option<Expr> {
        crate::support::child(self.syntax())
    }
}
impl AstNode for ConstructorField {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::CONSTRUCTOR_FIELD => Some(ConstructorField { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Condition {
    syntax: SyntaxNode,
}
impl Condition {
    pub fn if_kw(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::IF_KW)
    }
    pub fn else_kw(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::ELSE_KW)
    }
    pub fn condition(&self) -> Option<Expr> {
        crate::support::child(self.syntax())
    }
    pub fn block(&self) -> Option<Block> {
        crate::support::child(self.syntax())
    }
}
impl AstNode for Condition {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::CONDITION => Some(Condition { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub enum Pat {
    PatIdent(PatIdent),
    PatLiteral(PatLiteral),
    PatVariant(PatVariant),
    PatWildcard(PatWildcard),
}
impl AstNode for Pat {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::PAT_IDENT => {
                Some(Self::PatIdent(PatIdent::cast(syntax).unwrap()))
            }
            SyntaxKind::PAT_LITERAL => {
                Some(Self::PatLiteral(PatLiteral::cast(syntax).unwrap()))
            }
            SyntaxKind::PAT_VARIANT => {
                Some(Self::PatVariant(PatVariant::cast(syntax).unwrap()))
            }
            SyntaxKind::PAT_WILDCARD => {
                Some(Self::PatWildcard(PatWildcard::cast(syntax).unwrap()))
            }
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::PatIdent(it) => it.syntax(),
            Self::PatLiteral(it) => it.syntax(),
            Self::PatVariant(it) => it.syntax(),
            Self::PatWildcard(it) => it.syntax(),
        }
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct SwitchArm {
    syntax: SyntaxNode,
}
impl SwitchArm {
    pub fn big_arrow(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::BIG_ARROW)
    }
    pub fn pat(&self) -> Option<Pat> {
        crate::support::child(self.syntax())
    }
    pub fn expr(&self) -> Option<Expr> {
        crate::support::child(self.syntax())
    }
}
impl AstNode for SwitchArm {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::SWITCH_ARM => Some(SwitchArm { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct IntegerLit {
    syntax: SyntaxNode,
}
impl IntegerLit {}
impl AstNode for IntegerLit {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::INTEGER_LIT => Some(IntegerLit { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct FloatLit {
    syntax: SyntaxNode,
}
impl FloatLit {}
impl AstNode for FloatLit {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::FLOAT_LIT => Some(FloatLit { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct BooleanLit {
    syntax: SyntaxNode,
}
impl BooleanLit {}
impl AstNode for BooleanLit {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::BOOLEAN_LIT => Some(BooleanLit { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct StringLit {
    syntax: SyntaxNode,
}
impl StringLit {}
impl AstNode for StringLit {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::STRING_LIT => Some(StringLit { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct PatIdent {
    syntax: SyntaxNode,
}
impl PatIdent {
    pub fn name(&self) -> Option<Name> {
        crate::support::child(self.syntax())
    }
}
impl AstNode for PatIdent {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::PAT_IDENT => Some(PatIdent { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct PatLiteral {
    syntax: SyntaxNode,
}
impl PatLiteral {
    pub fn literal(&self) -> Option<Literal> {
        crate::support::child(self.syntax())
    }
}
impl AstNode for PatLiteral {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::PAT_LITERAL => Some(PatLiteral { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct PatVariant {
    syntax: SyntaxNode,
}
impl PatVariant {
    pub fn path(&self) -> Option<Path> {
        crate::support::child(self.syntax())
    }
    pub fn pat(&self) -> AstChildren<Pat> {
        crate::support::children(self.syntax())
    }
}
impl AstNode for PatVariant {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::PAT_VARIANT => Some(PatVariant { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct PatWildcard {
    syntax: SyntaxNode,
}
impl PatWildcard {
    pub fn dot_dot(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::DOT_DOT)
    }
}
impl AstNode for PatWildcard {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::PAT_WILDCARD => Some(PatWildcard { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct BoundType {
    syntax: SyntaxNode,
}
impl BoundType {
    pub fn name(&self) -> Option<Name> {
        crate::support::child(self.syntax())
    }
    pub fn constraints(&self) -> Option<Constraints> {
        crate::support::child(self.syntax())
    }
}
impl AstNode for BoundType {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::BOUND_TYPE => Some(BoundType { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct Constraints {
    syntax: SyntaxNode,
}
impl Constraints {
    pub fn colon(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::COLON)
    }
    pub fn ty(&self) -> AstChildren<Type> {
        crate::support::children(self.syntax())
    }
}
impl AstNode for Constraints {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::CONSTRAINTS => Some(Constraints { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct NamedType {
    syntax: SyntaxNode,
}
impl NamedType {
    pub fn path(&self) -> Option<Path> {
        crate::support::child(self.syntax())
    }
}
impl AstNode for NamedType {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::NAMED_TYPE => Some(NamedType { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct ArrayType {
    syntax: SyntaxNode,
}
impl ArrayType {
    pub fn left_bracket(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::LEFT_BRACKET)
    }
    pub fn right_bracket(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::RIGHT_BRACKET)
    }
    pub fn elemental(&self) -> Option<Type> {
        crate::support::child(self.syntax())
    }
}
impl AstNode for ArrayType {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::ARRAY_TYPE => Some(ArrayType { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct SelfType {
    syntax: SyntaxNode,
}
impl SelfType {
    pub fn self_type(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::SELF_TYPE)
    }
}
impl AstNode for SelfType {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::SELF_TYPE => Some(SelfType { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct PointerType {
    syntax: SyntaxNode,
}
impl PointerType {
    pub fn mul(&self) -> Option<SyntaxToken> {
        crate::support::token(self.syntax(), SyntaxKind::MUL)
    }
    pub fn elemental(&self) -> Option<Type> {
        crate::support::child(self.syntax())
    }
}
impl AstNode for PointerType {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::POINTER_TYPE => Some(PointerType { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub enum PathSegment {
    PathNamespace(PathNamespace),
    PathVariant(PathVariant),
    PathCallable(PathCallable),
    PathType(PathType),
}
impl AstNode for PathSegment {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::PATH_NAMESPACE => {
                Some(Self::PathNamespace(PathNamespace::cast(syntax).unwrap()))
            }
            SyntaxKind::PATH_VARIANT => {
                Some(Self::PathVariant(PathVariant::cast(syntax).unwrap()))
            }
            SyntaxKind::PATH_CALLABLE => {
                Some(Self::PathCallable(PathCallable::cast(syntax).unwrap()))
            }
            SyntaxKind::PATH_TYPE => {
                Some(Self::PathType(PathType::cast(syntax).unwrap()))
            }
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::PathNamespace(it) => it.syntax(),
            Self::PathVariant(it) => it.syntax(),
            Self::PathCallable(it) => it.syntax(),
            Self::PathType(it) => it.syntax(),
        }
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct PathNamespace {
    syntax: SyntaxNode,
}
impl PathNamespace {
    pub fn name(&self) -> Option<Name> {
        crate::support::child(self.syntax())
    }
}
impl AstNode for PathNamespace {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::PATH_NAMESPACE => Some(PathNamespace { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct PathVariant {
    syntax: SyntaxNode,
}
impl PathVariant {
    pub fn name(&self) -> Option<Name> {
        crate::support::child(self.syntax())
    }
}
impl AstNode for PathVariant {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::PATH_VARIANT => Some(PathVariant { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct PathCallable {
    syntax: SyntaxNode,
}
impl PathCallable {
    pub fn name(&self) -> Option<Name> {
        crate::support::child(self.syntax())
    }
    pub fn generic_args(&self) -> Option<GenericArgs> {
        crate::support::child(self.syntax())
    }
}
impl AstNode for PathCallable {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::PATH_CALLABLE => Some(PathCallable { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub struct PathType {
    syntax: SyntaxNode,
}
impl PathType {
    pub fn name(&self) -> Option<Name> {
        crate::support::child(self.syntax())
    }
    pub fn generic_args(&self) -> Option<GenericArgs> {
        crate::support::child(self.syntax())
    }
}
impl AstNode for PathType {
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::PATH_TYPE => Some(PathType { syntax }),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
