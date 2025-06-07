use error_snippet::Result;

use crate::PackageParser;
use crate::errors::*;
use crate::parser::{Block, Property, Value};

impl PackageParser {
    /// Expects the given [`Block`] to have a [`Property`] with the given name.
    pub(crate) fn opt_string_prop(&self, block: &Block, name: &'static str) -> Result<Option<String>> {
        match block.find_prop(name) {
            Some(prop) => Ok(Some(self.expect_prop_string(prop)?.to_owned())),
            None => Ok(None),
        }
    }

    /// Expects the value of the given [`Property`] to be of type [`Value::Boolean`].
    pub(crate) fn expect_prop_bool(&self, prop: &Property) -> Result<bool> {
        match &prop.value {
            Value::Boolean(val) => Ok(*val.value()),
            kind => Err(ArcfileUnexpectedType {
                source: self.source.clone(),
                range: prop.location.range.clone(),
                expected: String::from("Boolean"),
                found: kind.to_string(),
            }
            .into()),
        }
    }

    /// Expects the value of the given [`Property`] to be an array of type [`Value`].
    pub(crate) fn expect_prop_array<'a>(&self, prop: &'a Property) -> Result<&'a [Value]> {
        match &prop.value {
            Value::Array(val) => Ok(&val.values),
            kind => Err(ArcfileUnexpectedType {
                source: self.source.clone(),
                range: prop.location.range.clone(),
                expected: String::from("Array"),
                found: kind.to_string(),
            }
            .into()),
        }
    }

    /// Expects the value of the given [`Property`] to be of type [`Value::Block`].
    pub(crate) fn expect_prop_block<'a>(&self, value: impl Into<&'a Value>) -> Result<&'a Block> {
        let value = value.into();

        match value {
            Value::Block(val) => Ok(val.as_ref()),
            kind => Err(ArcfileUnexpectedType {
                source: self.source.clone(),
                range: value.location().clone(),
                expected: String::from("Array"),
                found: kind.to_string(),
            }
            .into()),
        }
    }

    /// Expects the value of the given [`Property`] to be of type [`Value::String`].
    pub(crate) fn expect_prop_string<'a>(&self, value: impl Into<&'a Value>) -> Result<&'a str> {
        let value = value.into();

        match &value {
            Value::String(str) => Ok(str.value()),
            kind => Err(ArcfileUnexpectedType {
                source: self.source.clone(),
                range: value.location().clone(),
                expected: String::from("String"),
                found: kind.to_string(),
            }
            .into()),
        }
    }
}
