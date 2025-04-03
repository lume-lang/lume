use arc::errors::ArcError;

#[derive(serde::Serialize, Debug, Clone, PartialEq, Eq)]
pub struct NamedSource {
    pub name: Option<String>,

    pub content: String,
}

impl NamedSource {
    pub fn new(name: String, content: String) -> Self {
        NamedSource {
            name: Some(name),
            content,
        }
    }

    /// Create a new unnamed source from a string.
    ///
    /// This source will have no name associated with it, so it's mostly used for testing or debugging.
    pub fn unnamed(content: String) -> Self {
        NamedSource { name: None, content }
    }

    /// Create a new named source from a file path.
    ///
    /// The file will be read and its content will be used as the source content.
    pub fn from_file(path: &std::path::PathBuf) -> Result<Self, ArcError> {
        let file_path = path.to_string_lossy().into_owned();

        let content = match std::fs::read_to_string(&file_path) {
            Ok(content) => content,
            Err(err) => return Err(ArcError::IoError(err)),
        };

        Ok(NamedSource {
            name: Some(file_path),
            content,
        })
    }
}
