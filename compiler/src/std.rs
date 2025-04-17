use diag::{Result, source::NamedSource};
use rust_embed::Embed;

#[derive(Embed)]
#[folder = "$CARGO_MANIFEST_DIR/../std"]
pub(crate) struct Assets;

impl Assets {
    /// Read all the standard library files as [`NamedSource`]-instances.
    pub(crate) fn as_sources() -> Result<Vec<NamedSource>> {
        Assets::iter()
            .map(|asset| {
                let name = asset.as_ref();
                let embedded_file = Assets::get(name).unwrap();
                let content = std::str::from_utf8(embedded_file.data.as_ref()).unwrap();

                Ok(NamedSource::new(name.to_string(), content.to_string()))
            })
            .collect::<Result<Vec<_>>>()
    }
}
