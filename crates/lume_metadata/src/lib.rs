use std::path::PathBuf;
use std::time::SystemTime;

use lume_hir::map::Map;
use lume_session::Package;
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
    pub build_time: SystemTime,
}

impl PackageHeader {
    pub fn create_from(pkg: &Package) -> Self {
        let build_time = match std::fs::metadata(pkg.root()).and_then(|attr| attr.modified()) {
            Ok(time) => time,
            Err(_) => SystemTime::now(),
        };

        Self {
            name: pkg.name.clone(),
            build_time,
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
fn partition_public_nodes(tcx: &TyCheckCtx) -> Map {
    let mut pub_hir = Map::empty(tcx.hir().package);

    pub_hir.nodes = tcx
        .hir_nodes()
        .filter(|node| tcx.is_visible_outside_package(node.id()))
        .map(|node| (node.id(), node.to_owned()))
        .collect();

    pub_hir
}
