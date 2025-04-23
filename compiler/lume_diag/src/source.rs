use std::path::PathBuf;

use crate::Result;

/// Defines a source file, which can be used to provide context for diagnostics.
///
/// This trait represents some sort of source code, which will be reported to the user as
/// part of the reporting process.
pub trait Source: Send + Sync + std::fmt::Debug {
    /// Defines the name of the source file.
    fn name(&self) -> Option<&str> {
        None
    }

    /// Gets the full content of the source file.
    fn content(&self) -> Box<&str>;
}

impl Source for [u8] {
    fn content(&self) -> Box<&str> {
        Box::new(std::str::from_utf8(self).unwrap())
    }
}

impl Source for &[u8] {
    fn content(&self) -> Box<&str> {
        <[u8] as Source>::content(self)
    }
}

impl Source for Vec<u8> {
    fn content(&self) -> Box<&str> {
        <[u8] as Source>::content(self)
    }
}

impl Source for str {
    fn content(&self) -> Box<&str> {
        <[u8] as Source>::content(self.as_bytes())
    }
}

impl Source for &str {
    fn content(&self) -> Box<&str> {
        <str as Source>::content(self)
    }
}

impl Source for String {
    fn content(&self) -> Box<&str> {
        <str as Source>::content(self)
    }
}

impl Source for &String {
    fn content(&self) -> Box<&str> {
        <String as Source>::content(self)
    }
}

/// Represents a simple source with only string-based content.
#[derive(serde::Serialize, Debug, Clone)]
pub struct StringSource {
    /// Defines the content of the source file.
    pub content: String,
}

impl StringSource {
    /// Creates a new [`StringSource`] from the content.
    pub fn new(content: String) -> Self {
        Self { content }
    }
}

impl Source for StringSource {
    fn name(&self) -> Option<&str> {
        None
    }

    fn content(&self) -> Box<&str> {
        Box::new(self.content.as_str())
    }
}

/// Represents a source file with a name and content.
///
/// This is the default implementation of the [`Source`] trait and is used
/// internally to create diagnostics using derive-macros.
#[derive(serde::Serialize, Debug, Clone, PartialEq, Eq)]
pub struct NamedSource {
    /// Defines the name of the source file.
    pub name: String,

    /// Defines the content of the source file.
    pub content: String,
}

impl NamedSource {
    /// Creates a new [`NamedSource`] from the given name and content.
    pub fn new(name: String, content: String) -> Self {
        Self { name, content }
    }

    /// Creates a new [`NamedSource`] instance from an existing file.
    pub fn from_file(path: PathBuf) -> Result<NamedSource> {
        let name = path.to_string_lossy().to_string();
        let content = std::fs::read_to_string(path)?;

        Ok(NamedSource { name, content })
    }
}

impl Source for NamedSource {
    fn name(&self) -> Option<&str> {
        Some(self.name.as_str())
    }

    fn content(&self) -> Box<&str> {
        Box::new(self.content.as_str())
    }
}
