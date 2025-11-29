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
