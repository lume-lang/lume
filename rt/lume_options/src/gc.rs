use std::str::FromStr;

use serde::{Deserialize, Serialize};

/// Options for configuring the garbage collector.
#[derive(Serialize, Deserialize, Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct GarbageCollectorOptions {
    /// Determines the initial size of the heap within the GC.
    #[serde(deserialize_with = "size_or_variant")]
    pub heap_size: GarbageCollectorSize,
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq)]
pub enum GarbageCollectorSize {
    /// Defines that the GC heap should have a fixed size, set to the given
    /// value (in bytes). This is the preferred otion for user-defined sizes,
    /// since the is explicitly defined without modification.
    #[serde(rename = "size")]
    Static(usize),

    /// Defines that the GC heap should have a size relative to the total amount
    /// of system memory.
    ///
    /// When using the `Rooted` option, the total size of the GC heap will be
    /// equal to the total amount of system memory, bit-shifted right by the
    /// given amount.
    ///
    /// For example, passing `6` on a system with 16GB of total system memory,
    /// the heap would allocate 256MB of memory, since
    /// `16GB >> 6 = 256MB` (`17179869184 >> 6 = 268435456`).
    #[serde(rename = "factor")]
    Rooted(u8),
}

impl Default for GarbageCollectorSize {
    fn default() -> Self {
        Self::Rooted(6)
    }
}

impl FromStr for GarbageCollectorSize {
    type Err = Box<dyn std::error::Error>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let size = size_from_string(s)?;

        Ok(Self::Static(size))
    }
}

/// Deserializer for the [`GarbageCollectorOptions::heap_size`] field, allowing
/// a user to write any of the following versions:
/// ```toml
/// heap_size = 2048
/// heap_size = "1024MB"
/// heap_size = "1gb"
/// heap_size = { size = 512 },
/// heap_size = { factor = 7 },
/// ```
fn size_or_variant<'de, T, D>(deserializer: D) -> Result<T, D::Error>
where
    T: serde::Deserialize<'de> + FromStr<Err = Box<dyn std::error::Error>>,
    D: serde::Deserializer<'de>,
{
    // This is a Visitor that forwards string types to T's `FromStr` impl and
    // forwards map types to T's `Deserialize` impl. The `PhantomData` is to
    // keep the compiler from complaining about T being an unused generic type
    // parameter. We need T in order to know the Value type for the Visitor
    // impl.
    struct SizeOrVariant<T>(std::marker::PhantomData<fn() -> T>);

    impl<'de, T> serde::de::Visitor<'de> for SizeOrVariant<T>
    where
        T: Deserialize<'de> + FromStr<Err = Box<dyn std::error::Error>>,
    {
        type Value = T;

        fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
            formatter.write_str("integer, string or map")
        }

        fn visit_str<E>(self, value: &str) -> Result<T, E>
        where
            E: serde::de::Error,
        {
            Ok(FromStr::from_str(value).unwrap())
        }

        fn visit_map<M>(self, map: M) -> Result<T, M::Error>
        where
            M: serde::de::MapAccess<'de>,
        {
            // `MapAccessDeserializer` is a wrapper that turns a `MapAccess`
            // into a `Deserializer`, allowing it to be used as the input to T's
            // `Deserialize` implementation. T then deserializes itself using
            // the entries from the map visitor.
            Deserialize::deserialize(serde::de::value::MapAccessDeserializer::new(map))
        }
    }

    deserializer.deserialize_any(SizeOrVariant(std::marker::PhantomData))
}

/// Attempts to convert the given string into a size.
///
/// The string can either be a unitless number (e.x. `8`, `1024`) or a number
/// with a byte-size unit (e.x. `4G`, `128MB`, etc.).
///
/// If the string has a unit, the unit is parsed case-insensitively. The unit
/// must be one of: `G`, `GB`, `M`, `MB`, `K`, `KB`, `B`.
///
/// Note: since the unit is case-insensitive, there is no semantic difference
/// between `32MB` and `32Mb`, even though they would be different sizes in most
/// contexts.
fn size_from_string(s: &str) -> Result<usize, Box<dyn std::error::Error>> {
    if let Some(last_char) = s.chars().last()
        && last_char.is_ascii_digit()
    {
        return match s.parse::<usize>() {
            Ok(val) => Ok(val),
            Err(err) => Err(Box::new(err)),
        };
    }

    let Some(unit_start) = s.find(|c: char| c.is_ascii_alphabetic()) else {
        return Err(Box::new(std::io::Error::other("invalid size: no unit found")));
    };

    let Some((value, unit)) = s.split_at_checked(unit_start) else {
        return Err(Box::new(std::io::Error::other("invalid size: no number found")));
    };

    let size = match value.parse::<usize>() {
        Ok(val) => val,
        Err(err) => return Err(Box::new(err)),
    };

    match unit.to_ascii_lowercase().as_str() {
        "g" | "gb" => Ok(size * 1024 * 1024 * 1024),
        "m" | "mb" => Ok(size * 1024 * 1024),
        "k" | "kb" => Ok(size * 1024),
        "b" => Ok(size),
        unit => Err(Box::new(std::io::Error::other(format!("invalid unit {unit:?}")))),
    }
}
