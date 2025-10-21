use std::path::PathBuf;

use lume_hir::map::Map;
use lume_session::Package;
use lume_typech::TyCheckCtx;
use serde::{Deserialize, Serialize};

/// Defines the file extension to use for metadata library files.
pub const METADATA_FILE_EXTENSION: &str = "mlib";

/// Gets the metadata file-name which corresponds to the given package name.
pub fn metadata_filename_of(hdr: &PackageHeader) -> PathBuf {
    let name = format!("{}.{METADATA_FILE_EXTENSION}", hdr.name);

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
}

impl PackageHeader {
    pub fn create_from(pkg: &Package) -> Self {
        Self { name: pkg.name.clone() }
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
