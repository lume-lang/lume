use error_snippet::Result;

use crate::PackageParser;
use crate::parser::Block;
use crate::serializer::{ManifestDependencies, ManifestDependency};

impl PackageParser {
    /// Parses the dependencies from the given package block.
    pub(crate) fn parse_dependencies(&self, block: &Block) -> Result<ManifestDependencies> {
        let no_std = match block.find_prop("no_std") {
            Some(prop) => self.expect_prop_bool(prop)?,
            None => false,
        };

        let Some(dependencies) = block.find_prop("dependencies") else {
            return Ok(ManifestDependencies {
                no_std,
                dependencies: Vec::new(),
            });
        };

        let dependencies = self
            .expect_prop_array(dependencies)?
            .iter()
            .map(|dep| {
                let block = self.expect_prop_block(dep)?;
                let dep = self.parse_dependency(block)?;

                Ok(dep)
            })
            .collect::<Result<Vec<_>>>()?;

        Ok(ManifestDependencies { no_std, dependencies })
    }

    /// Parses the dependencies from the given package block.
    pub(crate) fn parse_dependency(&self, block: &Block) -> Result<ManifestDependency> {
        if block.ty.value() != "Dependency" {
            todo!();
        }

        let Some(source_prop) = block.find_prop("source") else {
            todo!();
        };

        let Some(version_prop) = block.find_prop("version") else {
            todo!();
        };

        let source = self.expect_prop_string(source_prop)?.to_owned();
        let required_version = self.version_req(version_prop)?.into_value();

        Ok(ManifestDependency {
            source,
            required_version,
        })
    }
}
