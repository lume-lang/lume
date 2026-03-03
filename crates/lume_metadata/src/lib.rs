use std::path::{Path, PathBuf};

use lume_errors::{IntoDiagnostic, MapDiagnostic, Result, SimpleDiagnostic};
use lume_hir::map::Map;
use lume_session::{Package, PackageHash};
use lume_typech::TyCheckCtx;
use serde::{Deserialize, Serialize};

/// Defines the file extension to use for metadata library files.
pub const METADATA_FILE_EXTENSION: &str = "mlib";

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
    pub header: PackageHeader,
    pub hir: Map,
}

impl PackageMetadata {
    /// Creates a new [`PackageMetadata`] from the given [`Package`].
    pub fn create(pkg: &Package, tcx: &TyCheckCtx) -> Self {
        let public_hir = partition_public_nodes(tcx);
        let header = PackageHeader::create_from(pkg);

        Self {
            header,
            hir: public_hir,
        }
    }
}

/// Partition the given HIR map into only public, exported items.
///
/// Any item or node which is not visible outside of the package is
/// not included. As a result of this function, a new HIR map is created
/// with all public items cloned into it.
pub fn partition_public_nodes(tcx: &TyCheckCtx) -> Map {
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
pub fn read_metadata_object<P: AsRef<Path>>(metadata_path: P) -> Result<Option<PackageMetadata>> {
    let metadata_path = metadata_path.as_ref();
    if !metadata_path.exists() {
        return Ok(None);
    }

    let metadata_bytes = std::fs::read(metadata_path)
        .map_cause(format!("failed to read package metadata ({})", metadata_path.display()))?;

    let deserialized = ciborium::from_reader::<PackageMetadata, &[u8]>(metadata_bytes.as_ref()).map_cause(format!(
        "failed to deserialize package metadata ({})",
        metadata_path.display()
    ))?;

    Ok(Some(deserialized))
}

/// Reads only the header of the metadata from the given package from disk and
/// returns it. This is significantly faster when only the header is needed, as
/// very little of the file is actually read and deserialized.
///
/// If the metadata file does not exist, this function returns [`None`], wrapped
/// in [`Ok`].
pub fn read_metadata_header<P: AsRef<Path>>(metadata_path: P) -> Result<Option<PackageHeader>> {
    let metadata_path = metadata_path.as_ref();
    if !metadata_path.exists() {
        return Ok(None);
    }

    let metadata_bytes = std::fs::read(metadata_path)
        .map_cause(format!("failed to read package metadata ({})", metadata_path.display()))?;

    let deserialized = ciborium::from_reader::<PackageMetadata, &[u8]>(metadata_bytes.as_ref()).map_cause(format!(
        "failed to deserialize package metadata ({})",
        metadata_path.display()
    ))?;

    Ok(Some(deserialized.header))
}

/// Writes the serialized representation of `metadata` to disk within the
/// metadata directory defined by `gcx` (via
/// [`GlobalCtx::obj_metadata_path()`]).
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
    ciborium::into_writer(metadata, &mut serialized).map_err(|err| {
        let diag = SimpleDiagnostic::new(format!("failed to serialize metadata ({})", metadata.header.name))
            .add_cause(err.into_diagnostic());

        Box::new(diag) as lume_errors::Error
    })?;

    std::fs::write(metadata_path, serialized).map_err(|err| {
        Box::new(SimpleDiagnostic::new("failed to write metadata").add_cause(err)) as lume_errors::Error
    })?;

    Ok(())
}
