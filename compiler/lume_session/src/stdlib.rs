use std::path::PathBuf;
use std::sync::Arc;

use lume_span::{FileName, PackageId, SourceFile, SourceFileId};
use rust_embed::Embed;

#[derive(Embed)]
#[folder = "$CARGO_MANIFEST_DIR/../../std"]
#[include = "**/*.lm"]
pub struct Assets;

impl Assets {
    /// Read all the standard library files as [`Arc<SourceFile>`]-instances.
    #[expect(clippy::missing_panics_doc, reason = "infallible")]
    pub fn as_sources(package: PackageId) -> impl Iterator<Item = Arc<SourceFile>> {
        Assets::iter().map(move |asset| {
            let name = asset.as_ref();
            let embedded_file = Assets::get(name).unwrap();
            let content = std::str::from_utf8(embedded_file.data.as_ref()).unwrap();

            Arc::new(SourceFile {
                id: SourceFileId::new(package, name),
                name: FileName::StandardLibrary(PathBuf::from(name)),
                content: content.to_string(),
                package,
            })
        })
    }
}
