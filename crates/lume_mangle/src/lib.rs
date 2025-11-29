use lume_errors::Result;
use lume_span::NodeId;
use lume_typech::TyCheckCtx;

pub mod l1;

/// Defines the mangling version to use when mangling symbol names.
///
/// There is currently only a single version, `L1`, which is the default across
/// all packages.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub enum Version {
    #[default]
    L1,
}

pub fn mangled(tcx: &TyCheckCtx, id: NodeId, version: Version) -> Result<String> {
    match version {
        Version::L1 => l1::mangled_name_of(tcx, id),
    }
}
