use std::ops::Deref;
use std::path::{Path, PathBuf};

use lume_errors::{MapDiagnostic, Result, SimpleDiagnostic};
use lume_hir::map::Map;
use lume_infer::TyInferCtx;
use lume_session::{Package, PackageHash};
use lume_span::SourceMap;
use serde::{Deserialize, Serialize};

/// Defines the file extension to use for metadata library files.
pub const METADATA_FILE_EXTENSION: &str = "mlib";

/// Defines the file magic number within metadata library files.
pub const METADATA_FILE_MAGIC: &str = "MLIB!";

/// Gets the metadata file-name which corresponds to the given package name.
pub fn metadata_filename_of(package_name: &str) -> PathBuf {
    let name = format!("{package_name}.{METADATA_FILE_EXTENSION}");

    PathBuf::from(name)
}

/// Serializable package header.
///
/// The package header is meant to determine whether a given
/// pre-compiled package is applicable to use in the current
/// compilation context.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct PackageHeader {
    pub name: String,
    pub hash: PackageHash,
}

impl PackageHeader {
    pub fn create_from(pkg: &Package) -> Self {
        Self {
            name: pkg.name.clone(),
            hash: pkg.package_hash(),
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct PackageMetadata {
    pub header: LazyData<PackageHeader>,

    /// Source file information for all the sources directly within the package
    /// source tree.
    ///
    /// NOTE: this field MUST appear before any [Location] fields, even nested
    /// ones. The source map must be deserialized first, in order for
    /// deduplicated [Location] keys to be resolved correctly. See
    /// more in the [`lume_span::source::serialize`] module.
    ///
    /// [Location]: lume_span::Location
    pub source_map: LazyData<SourceMap>,
    pub hir: LazyData<Map>,
}

impl PackageMetadata {
    /// Creates a new [`PackageMetadata`] from the given [`Package`].
    pub fn create(pkg: &Package, tcx: &TyInferCtx) -> Self {
        let header = PackageHeader::create_from(pkg);
        let source_map = tcx.gcx().session.source_map.clone();
        let public_hir = partition_public_nodes(tcx);

        Self {
            header: LazyData::new(header),
            source_map: LazyData::new(source_map),
            hir: LazyData::new(public_hir),
        }
    }
}

/// Partition the given HIR map into only public, exported items.
///
/// Any item or node which is not visible outside of the package is
/// not included. As a result of this function, a new HIR map is created
/// with all public items cloned into it.
pub fn partition_public_nodes(tcx: &TyInferCtx) -> Map {
    if tcx.gcx().session.options.export_private_nodes {
        return tcx.hir().clone();
    }

    let mut pub_hir = Map::empty(tcx.hir().package);

    pub_hir.nodes = tcx
        .hir_local_nodes()
        .filter(|node| tcx.should_export(node.id()).unwrap_or(false))
        .map(|node| (node.id(), node.to_owned()))
        .collect();

    tcx.hir().types.clone_into(&mut pub_hir.types);
    tcx.hir().lang_items.clone_into(&mut pub_hir.lang_items);

    pub_hir
}

/// Reads the serialized representation of the metadata from the given package
/// from disk and returns it.
///
/// If the metadata file does not exist, this function returns [`None`], wrapped
/// in [`Ok`].
#[tracing::instrument(level = "DEBUG", skip_all, err)]
pub fn read_metadata_object<P: AsRef<Path>>(metadata_path: P) -> Result<Option<PackageMetadata>> {
    let metadata_path = metadata_path.as_ref();
    tracing::debug!(path = %metadata_path.display());

    if !metadata_path.exists() {
        return Ok(None);
    }

    let metadata_file = std::fs::OpenOptions::new()
        .read(true)
        .open(metadata_path)
        .map_cause(format!("failed to read package metadata ({})", metadata_path.display()))?;

    let mut reader = std::io::BufReader::new(metadata_file);

    let metadata = deserialize_metadata(&mut reader).map_err(|err| {
        let diag = SimpleDiagnostic::new(format!("failed to deserialize metadata ({})", metadata_path.display()))
            .add_cause(SimpleDiagnostic::new(err.to_string()));

        Box::new(diag) as lume_errors::Error
    })?;

    Ok(Some(metadata))
}

/// Reads only the header of the metadata from the given package from disk and
/// returns it. This is significantly faster when only the header is needed, as
/// very little of the file is actually read and deserialized.
///
/// If the metadata file does not exist, this function returns [`None`], wrapped
/// in [`Ok`].
#[tracing::instrument(level = "DEBUG", skip_all, err)]
pub fn read_metadata_header<P: AsRef<Path>>(metadata_path: P) -> Result<Option<PackageHeader>> {
    let metadata_path = metadata_path.as_ref();
    tracing::debug!(path = %metadata_path.display());

    if !metadata_path.exists() {
        return Ok(None);
    }

    let mut metadata_file = std::fs::OpenOptions::new()
        .read(true)
        .open(metadata_path)
        .map_cause(format!("failed to read package metadata ({})", metadata_path.display()))?;

    let header = deserialize_metadata_header(&mut metadata_file).map_err(|err| {
        let diag = SimpleDiagnostic::new(format!("failed to deserialize metadata ({})", metadata_path.display()))
            .add_cause(SimpleDiagnostic::new(err.to_string()));

        Box::new(diag) as lume_errors::Error
    })?;

    Ok(Some(header.inner))
}

/// Writes the serialized representation of `metadata` to disk within the given
/// metadata directory. The name of the written file is determined by
/// [`metadata_filename_of`].
pub fn write_metadata_object<P: AsRef<Path>>(metadata_directory: P, metadata: &PackageMetadata) -> Result<()> {
    // Ensure the parent directory exists first.
    let metadata_directory = metadata_directory.as_ref();

    std::fs::create_dir_all(metadata_directory).map_err(|err| {
        Box::new(
            SimpleDiagnostic::new(format!(
                "failed to create metadata directory ({})",
                metadata_directory.display()
            ))
            .add_cause(err),
        ) as lume_errors::Error
    })?;

    let metadata_filename = metadata_filename_of(&metadata.header.name);
    let metadata_path = metadata_directory.join(metadata_filename);

    let mut serialized = Vec::<u8>::new();

    serialize_metadata(&mut serialized, metadata).map_err(|err| {
        let diag = SimpleDiagnostic::new(format!("failed to serialize metadata ({})", metadata.header.name))
            .add_cause(SimpleDiagnostic::new(err.to_string()));

        Box::new(diag) as lume_errors::Error
    })?;

    tracing::debug!(
        package = %metadata.header.name,
        total_size = serialized.len(),
        path = %metadata_path.display(),
        "meta_serialized"
    );

    std::fs::write(metadata_path, serialized).map_err(|err| {
        Box::new(SimpleDiagnostic::new("failed to write metadata").add_cause(err)) as lume_errors::Error
    })?;

    Ok(())
}

pub struct LazyData<T> {
    inner: T,
}

impl<T> LazyData<T> {
    pub(crate) fn new(inner: T) -> Self {
        Self { inner }
    }
}

impl<T: Serialize> LazyData<T> {
    /// Serializes the instance to the given writer.
    pub(crate) fn serialize_to<W: std::io::Write>(
        &self,
        writer: &mut W,
    ) -> std::result::Result<(), Box<dyn std::error::Error>> {
        let mut encoded = Vec::<u8>::new();
        ciborium::into_writer(&self.inner, &mut encoded)?;

        tracing::debug!(
            type = %std::any::type_name::<Self>(),
            size = encoded.len(),
            "meta_item_serialized"
        );

        writer.write_all(&usize::to_ne_bytes(encoded.len()))?;
        writer.write_all(&encoded)?;

        Ok(())
    }
}

impl<T: serde::de::DeserializeOwned> LazyData<T> {
    /// Deserialize a single "item" from the given reader.
    ///
    /// An "item" is just any length-prefixed (as `usize`) serialized blob
    /// within the reader.
    ///
    /// This function assumes the file magic has already been read from the
    /// reader.
    pub(crate) fn deserialize_from<R: std::io::Read>(
        reader: &mut R,
    ) -> std::result::Result<Self, Box<dyn std::error::Error>> {
        let mut size_bytes: [u8; _] = [0x00; size_of::<usize>()];
        reader.read_exact(&mut size_bytes)?;

        let size = usize::from_ne_bytes(size_bytes);
        let mut buffer = vec![0x00; size];
        reader.read_exact(&mut buffer)?;

        tracing::debug!(
            type = %std::any::type_name::<Self>(),
            size = buffer.len(),
            "meta_item_deserialized"
        );

        let section = ciborium::from_reader::<T, &[u8]>(buffer.as_ref())?;

        Ok(LazyData { inner: section })
    }
}

impl<T> Deref for LazyData<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T: Serialize> Serialize for LazyData<T> {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.inner.serialize(serializer)
    }
}

impl<'de, T: Deserialize<'de>> Deserialize<'de> for LazyData<T> {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        T::deserialize(deserializer).map(LazyData::new)
    }
}

#[tracing::instrument(level = "DEBUG", skip_all, fields(package = %metadata.header.name), err)]
fn serialize_metadata<W: std::io::Write>(
    writer: &mut W,
    metadata: &PackageMetadata,
) -> std::result::Result<(), Box<dyn std::error::Error>> {
    writer.write_all(METADATA_FILE_MAGIC.as_bytes())?;
    metadata.header.serialize_to(writer)?;
    metadata.source_map.serialize_to(writer)?;
    metadata.hir.serialize_to(writer)?;

    Ok(())
}

/// Verify the header of the given reader, and ensure it matches the magic of
/// the metadata file magic.
fn verify_metadata_header<R: std::io::Read>(reader: &mut R) -> std::result::Result<(), Box<dyn std::error::Error>> {
    let mut magic: [u8; _] = [0x00; METADATA_FILE_MAGIC.len()];
    reader.read_exact(&mut magic)?;

    if magic != METADATA_FILE_MAGIC.as_bytes() {
        return Err(Box::new(std::io::Error::other("invalid magic")));
    }

    Ok(())
}

/// Deserialize only the [`PackageHeader`] object from the given reader.
fn deserialize_metadata_header<R: std::io::Read>(
    reader: &mut R,
) -> std::result::Result<LazyData<PackageHeader>, Box<dyn std::error::Error>> {
    verify_metadata_header(reader)?;

    LazyData::<PackageHeader>::deserialize_from(reader)
}

/// Deserialize the whole [`PackageMetadata`] object from the given reader.
fn deserialize_metadata<R: std::io::Read>(
    reader: &mut R,
) -> std::result::Result<PackageMetadata, Box<dyn std::error::Error>> {
    verify_metadata_header(reader)?;

    let header = LazyData::deserialize_from(reader)?;
    let source_map = LazyData::deserialize_from(reader)?;
    let hir = LazyData::deserialize_from(reader)?;

    Ok(PackageMetadata {
        header,
        source_map,
        hir,
    })
}
