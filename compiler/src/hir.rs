#[derive(serde::Serialize, Debug, Clone, PartialEq, Eq)]
pub struct SourceFileId(usize);

#[derive(serde::Serialize, Debug, Clone, PartialEq, Eq)]
pub struct Location {
    /// Defines the file which the location refers to.
    pub file: SourceFileId,

    /// Defines the index within the source file
    pub range: std::ops::Range<usize>,
}

impl Location {
    pub fn start(&self) -> usize {
        self.range.start
    }

    pub fn end(&self) -> usize {
        self.range.end
    }
}

#[derive(serde::Serialize, Debug, Clone, PartialEq, Eq)]
pub struct Identifier {
    pub name: String,
    pub location: Location,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq, Eq)]
pub struct IdentifierPath {
    pub path: Vec<Identifier>,
    pub location: Location,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq, Eq)]
pub struct SymbolName {
    /// Defines the namespace which the symbol was defined in, if any.
    pub namespace: Option<IdentifierPath>,

    /// Defines the relative name of the symbol within it's namespace.
    pub name: Identifier,
}

#[derive(serde::Serialize, Debug, Clone, PartialEq, Eq)]
pub enum Symbol {}
