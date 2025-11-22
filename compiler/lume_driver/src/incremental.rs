use std::sync::Arc;

use lume_errors::{MapDiagnostic, Result, SimpleDiagnostic};
use lume_metadata::PackageMetadata;
use lume_session::{GlobalCtx, Package};

/// Determines whether the given package needs to be compiled or re-compiled.
///
/// This takes the state of the current package metadata into account, as well
/// as if anything has changed within it' source code.
#[libftrace::traced(level = Debug, fields(name = package.name), err, ret)]
pub fn needs_compilation(gcx: &Arc<GlobalCtx>, package: &Package) -> Result<bool> {
    // If no metadata file could be found, the package has likely not been built
    // yet - in which case it obviously needs to be built.
    let Ok(Some(metadata)) = read_metadata_object(gcx, package) else {
        libftrace::trace!("re-compilation required: could not read metadata object");
        return Ok(true);
    };

    let last_modified = match std::fs::metadata(package.root()).and_then(|attr| attr.modified()) {
        Ok(time) => time,
        Err(_) => {
            libftrace::trace!("re-compilation required: could not read current modification date");
            return Ok(true);
        }
    };

    let Ok(duration_since_build) = last_modified.duration_since(metadata.header.build_time) else {
        libftrace::trace!("re-compilation required: could not get difference in build time");
        return Ok(true);
    };

    libftrace::trace!("duration since build: {duration_since_build:?}");

    Ok(!duration_since_build.is_zero())
}

/// Reads the serialized representation of the metadata from the given package
/// from disk and returns it.
///
/// If the metadata file does not exist, this function returns [`None`], wrapped
/// in [`Ok`].
pub(crate) fn read_metadata_object(gcx: &Arc<GlobalCtx>, package: &Package) -> Result<Option<PackageMetadata>> {
    let metadata_directory = gcx.obj_metadata_path();
    let metadata_filename = lume_metadata::metadata_filename_of(&package.name);
    let metadata_path = metadata_directory.join(metadata_filename);

    if !metadata_path.exists() {
        return Ok(None);
    }

    let metadata_bytes = std::fs::read(&metadata_path).map_diagnostic()?;
    let deserialized = postcard::from_bytes::<PackageMetadata>(&metadata_bytes).map_diagnostic()?;

    Ok(Some(deserialized))
}

/// Writes the serialized representation of `metadata` to disk within the
/// metadata directory defined by `gcx` (via
/// [`GlobalCtx::obj_metadata_path()`]).
pub(crate) fn write_metadata_object(gcx: &Arc<GlobalCtx>, metadata: &PackageMetadata) -> Result<()> {
    // Ensure the parent directory exists first.
    let metadata_directory = gcx.obj_metadata_path();

    std::fs::create_dir_all(&metadata_directory).map_err(|err| {
        Box::new(
            SimpleDiagnostic::new(format!(
                "failed to create metadata directory ({})",
                metadata_directory.display()
            ))
            .add_cause(err),
        ) as lume_errors::Error
    })?;

    let metadata_filename = lume_metadata::metadata_filename_of(&metadata.header.name);
    let metadata_path = metadata_directory.join(metadata_filename);

    let serialized = postcard::to_allocvec(metadata).map_diagnostic()?;

    std::fs::write(metadata_path, serialized).map_err(|err| {
        Box::new(SimpleDiagnostic::new("failed to write metadata").add_cause(err)) as lume_errors::Error
    })?;

    Ok(())
}
