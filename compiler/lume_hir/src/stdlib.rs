use std::sync::Arc;

use error_snippet::Result;
use lume_span::{PackageId, SourceFile};
use rust_embed::Embed;

#[derive(Embed)]
#[folder = "$CARGO_MANIFEST_DIR/../../std"]
pub(crate) struct Assets;

impl Assets {
    /// Read all the standard library files as [`Arc<SourceFile>`]-instances.
    pub(crate) fn as_sources(package: PackageId) -> Result<Vec<Arc<SourceFile>>> {
        Assets::iter()
            .map(|asset| {
                let name = asset.as_ref();
                let embedded_file = Assets::get(name).unwrap();
                let content = std::str::from_utf8(embedded_file.data.as_ref()).unwrap();

                Ok(Arc::new(SourceFile::new(
                    package,
                    name.to_string(),
                    content.to_string(),
                )))
            })
            .collect::<Result<Vec<_>>>()
    }
}
