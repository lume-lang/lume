//! This module handles deserialization of source files.
//!
//! In the Lume compiler, source files are shared using [`Arc`] to save memory.
//! But, this leads to a ton of duplication when serialization metadata to disk,
//! since every [`Location`] struct would otherwise contain a duplicate copy of
//! the entire source file.
//!
//! To avoid this, the metadata contains a separate entry for source files which
//! are read before the rest of the location entries. This source map is stored
//! in a global source map. Location entries are then just deserialized from
//! their source IDs and retrieved from the global.

use std::ops::Range;
use std::sync::{Arc, LazyLock, Mutex};

use indexmap::IndexMap;
use serde::ser::SerializeStruct;
use serde::{Deserialize, Serialize};

use crate::source::Location;
use crate::{PackageId, SourceFile, SourceFileId, SourceMap};

/// Global state for deserialized source maps. This value is populated when any
/// [`SourceMap`] is deserialized, either by reading `.mlib` metadata files or
/// manually. When additional [`SourceMap`] instances are deserialized, this
/// value will be expanded to contain the new source files.
static DESERIALIZE_STATE: LazyLock<Mutex<SourceMap>> = LazyLock::new(|| Mutex::new(SourceMap::new()));

/// Empty source file to be duplicated when trying to deserialize empty source
/// file IDs (i.e. IDs created with [`SourceFileId::empty()`]).
static EMPTY_SOURCE_FILE: LazyLock<Arc<SourceFile>> = LazyLock::new(|| Arc::new(SourceFile::empty()));

fn source_from_state(id: SourceFileId) -> Arc<SourceFile> {
    if id == SourceFileId(PackageId::empty(), 0) {
        return (*EMPTY_SOURCE_FILE).clone();
    }

    let state = (*DESERIALIZE_STATE)
        .try_lock()
        .expect("failed to lock deserialization state - is another thread trying to deserialize?");

    state
        .files
        .get(&id)
        .unwrap_or_else(|| panic!("failed to find source file {id:?}"))
        .clone()
}

impl Serialize for SourceMap {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        <IndexMap<SourceFileId, Arc<SourceFile>> as Serialize>::serialize(&self.files, serializer)
    }
}

impl<'de> serde::Deserialize<'de> for SourceMap {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let files = <IndexMap<SourceFileId, Arc<SourceFile>> as Deserialize>::deserialize(deserializer)?;
        let mut state = (*DESERIALIZE_STATE)
            .try_lock()
            .expect("failed to lock deserialization state - is another thread trying to deserialize?");

        files.clone_into(&mut state.files);

        Ok(Self { files })
    }
}

impl serde::Serialize for Location {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut s = serializer.serialize_struct("Location", 2)?;
        s.serialize_field("file", &self.file.id)?;
        s.serialize_field("index", &self.index)?;
        s.end()
    }
}

impl<'de> serde::Deserialize<'de> for Location {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(field_identifier, rename_all = "lowercase")]
        enum Field {
            File,
            Index,
        }

        struct LocationVisitor;

        impl<'de> serde::de::Visitor<'de> for LocationVisitor {
            type Value = Location;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("struct Location")
            }

            fn visit_seq<V>(self, mut seq: V) -> Result<Location, V::Error>
            where
                V: serde::de::SeqAccess<'de>,
            {
                let file: SourceFileId = seq
                    .next_element()?
                    .ok_or_else(|| serde::de::Error::invalid_length(0, &self))?;

                let index: Range<usize> = seq
                    .next_element()?
                    .ok_or_else(|| serde::de::Error::invalid_length(1, &self))?;

                Ok(Location {
                    file: source_from_state(file),
                    index,
                })
            }

            fn visit_map<V>(self, mut map: V) -> Result<Location, V::Error>
            where
                V: serde::de::MapAccess<'de>,
            {
                let mut file: Option<SourceFileId> = None;
                let mut index: Option<Range<usize>> = None;

                while let Some(key) = map.next_key()? {
                    match key {
                        Field::File => {
                            if file.is_some() {
                                return Err(serde::de::Error::duplicate_field("file"));
                            }

                            file = Some(map.next_value()?);
                        }
                        Field::Index => {
                            if index.is_some() {
                                return Err(serde::de::Error::duplicate_field("index"));
                            }

                            index = Some(map.next_value()?);
                        }
                    }
                }

                let file = file.ok_or_else(|| serde::de::Error::missing_field("file"))?;
                let index = index.ok_or_else(|| serde::de::Error::missing_field("index"))?;

                Ok(Location {
                    file: source_from_state(file),
                    index,
                })
            }
        }

        deserializer.deserialize_struct("Location", &["file", "index"], LocationVisitor)
    }
}
