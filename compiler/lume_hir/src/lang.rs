use std::collections::HashMap;

use lume_errors::{Result, SimpleDiagnostic};
use lume_span::NodeId;
use serde::{Deserialize, Serialize};

macro_rules! lang_items {
    ($($variant:ident $key:literal $name:literal),*) => {
        /// An enumeration of all implemented language items in the Lume standard library.
        #[derive(Serialize, Deserialize, Hash, Debug, Clone, Copy, PartialEq, Eq)]
        pub enum LangItem {
            $(
                #[doc = concat!("The `", $key, "` lang item.")]
                $variant,
            )*
        }

        impl LangItem {
            /// Gets the language item with the given name as [`Some`], if any.
            ///
            /// If no such item exists, returns [`None`].
            pub fn from_name<S: AsRef<str>>(name: S) -> Option<Self> {
                match name.as_ref() {
                    $(
                        $key => Some(LangItem::$variant),
                    )*
                    _ => None,
                }
            }
        }

        impl std::fmt::Display for LangItem {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(
                        LangItem::$variant => write!(f, $name),
                    )*
                }
            }
        }
    };
}

lang_items! {
//  Variant,                Key,                        Human-readable name

    // Standard types
    Never                   "never"                     "Never",

    // Runtime functions
    MethodLookup            "lookup_method"             "find_method_on",

    // Array operations
    Array                   "array"                     "Array",
    ArrayWithCapacity       "array_with_capacity"       "Array::with_capacity",
    ArrayPush               "array_push"                "Array::push",

    // Intrinsic trait definitions
    Add                     "add_trait"                 "Add",
    Sub                     "sub_trait"                 "Sub",
    Mul                     "mul_trait"                 "Mul",
    Div                     "div_trait"                 "Div",
    And                     "and_trait"                 "And",
    Or                      "or_trait"                  "Or",
    Not                     "not_trait"                 "Not",
    Negate                  "negate_trait"              "Negate",
    BinaryAnd               "band_trait"                "BinaryAnd",
    BinaryOr                "bor_trait"                 "BinaryOr",
    BinaryXor               "bxor_trait"                "BinaryXor",

    // Misc. traits
    Cast                    "cast_trait"                "Cast",
    Equal                   "equal_trait"               "Equal",
    Compare                 "cmp_trait"                 "Compare",
    Dispose                 "dispose_trait"             "Dispose"
}

#[derive(Serialize, Deserialize, Default, Debug, Clone, PartialEq, Eq)]
pub struct LanguageItems {
    items: HashMap<LangItem, NodeId>,
}

impl LanguageItems {
    pub fn new() -> Self {
        Self { items: HashMap::new() }
    }

    /// Adds the given language item to the list.
    ///
    /// # Errors
    ///
    /// If the item is already present within the set, returns an error.
    pub fn add(&mut self, item: LangItem, node: NodeId) -> Result<()> {
        if let Some(existing) = self.items.insert(item, node) {
            return Err(SimpleDiagnostic::new(format!(
                "language item {item:?} already exists as {existing:?} - replaced with {node:?}",
            ))
            .into());
        }

        Ok(())
    }

    /// Adds a new language item to the list, keyed by the given language item
    /// name.
    ///
    /// # Errors
    ///
    /// Returns [`Err`] if the given language item key is invalid or if the
    /// language item already exists within the set.
    pub fn add_name(&mut self, name: &str, node: NodeId) -> Result<()> {
        let Some(lang_item) = LangItem::from_name(name) else {
            return Err(SimpleDiagnostic::new(format!("no language item with name {name:?} found",)).into());
        };

        self.add(lang_item, node)
    }

    /// Gets the ID of the language item with the given name, which was previous
    /// added.
    ///
    /// If no language item with the given name exists, returns [`None`].
    pub fn get(&self, item: LangItem) -> Option<NodeId> {
        self.items.get(&item).copied()
    }
}
