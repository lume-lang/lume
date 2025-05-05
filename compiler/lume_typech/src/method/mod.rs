use levenshtein::levenshtein;
use lume_diag::Result;
use lume_span::Location;
use lume_types::{Identifier, Method, MethodId, SymbolName};

use crate::ThirBuildCtx;

mod errors;

/// Defines the maximum Levenshtein distance allowed for method name suggestions.
pub const MAX_LEVENSHTEIN_DISTANCE: usize = 3;

#[derive(Debug)]
pub(crate) enum MethodLookupResult<'a> {
    /// A successful method lookup.
    Success(&'a Method),

    /// No matching method was found.
    Failure(MethodLookupError<'a>),
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum MethodDisqualificationReason {
    /// The name of the method does not match the expected name, but it
    /// was similar enough to be considered a typo or a mistake.
    Name,

    /// The number of arguments provided does not match the expected number.
    ArgumentCount,

    /// The types of the arguments provided do not match the expected types.
    ArgumentType,

    /// The number of type parameters provided does not match the expected number.
    TypeParameterCount,
}

impl std::fmt::Display for MethodDisqualificationReason {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MethodDisqualificationReason::Name => f.write_str("name mismatch"),
            MethodDisqualificationReason::ArgumentCount => f.write_str("argument count mismatch"),
            MethodDisqualificationReason::ArgumentType => f.write_str("argument type mismatch"),
            MethodDisqualificationReason::TypeParameterCount => f.write_str("type parameter count mismatch"),
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct MethodLookupSuggestion<'a> {
    /// Defines the ID of the method definition.
    pub def: &'a Method,

    /// Defines the reason for disqualification.
    pub reason: MethodDisqualificationReason,
}

#[derive(Debug)]
pub(crate) struct MethodLookupError<'a> {
    /// Defines the name of the type on which the method was being looked up.
    pub type_name: SymbolName,

    /// Defines the name of the method that was being looked up.
    pub method_name: Identifier,

    /// Defines a list of possible suggestions for the method lookup.
    pub suggestions: Vec<MethodLookupSuggestion<'a>>,
}

impl MethodLookupError<'_> {
    /// Compound the error and inner suggestions, if any, into a suitable error.
    pub fn compound_err(self, location: Location) -> lume_diag::Error {
        let suggestions = self
            .suggestions
            .into_iter()
            .map(|suggestion| {
                let method_name = suggestion.def.name.clone();

                errors::SuggestedMethod {
                    source: method_name.location.file.clone(),
                    range: method_name.location.index.clone(),
                    type_name: self.type_name.clone(),
                    method_name: method_name.name,
                    reason: suggestion.reason,
                }
            })
            .collect();

        errors::MissingMethod {
            source: location.file,
            range: location.index,
            type_name: self.type_name,
            method_name: self.method_name,
            suggestions,
        }
        .into()
    }
}

impl<'tcx> ThirBuildCtx<'tcx> {
    pub(crate) fn method_lookup<'a>(
        &'tcx self,
        hir: &lume_hir::map::Map,
        self_ty: &'a lume_types::TypeRef,
        method_name: &'a Identifier,
        args: &'a [lume_hir::Expression],
        type_args: &'a [lume_hir::TypeArgument],
    ) -> Result<MethodLookupResult<'tcx>> {
        // Contains a list of suggestions for the method lookup, in
        // case no matching method was found.
        let mut suggestions = Vec::new();

        for method_id in self.methods_defined_on(self_ty) {
            let method = method_id.get(self.tcx());

            // No matter if all the other checks pass, if the method name does not match, we should
            // only suggest it.
            let is_qualified = &method.name.name == method_name;

            // If the name is not an exact match, or even if the Levenshtein distance is too high,
            // skip this method and continue to the next one.
            if !is_qualified && levenshtein(&method.name.name.name, &method_name.name) > MAX_LEVENSHTEIN_DISTANCE {
                continue;
            }

            if method.parameters.len() != args.len() {
                suggestions.push(MethodLookupSuggestion {
                    def: method,
                    reason: MethodDisqualificationReason::ArgumentCount,
                });

                continue;
            }

            if method.type_parameters.len() != type_args.len() {
                suggestions.push(MethodLookupSuggestion {
                    def: method,
                    reason: MethodDisqualificationReason::TypeParameterCount,
                });

                continue;
            }

            for (param, arg) in method.parameters.inner().iter().zip(args.iter()) {
                let arg_type = self.type_of(hir, arg.id)?;

                if !self.check_type_compatibility(&arg_type, &param.ty)? {
                    suggestions.push(MethodLookupSuggestion {
                        def: method,
                        reason: MethodDisqualificationReason::ArgumentType,
                    });

                    continue;
                }
            }

            if !is_qualified {
                suggestions.push(MethodLookupSuggestion {
                    def: method,
                    reason: MethodDisqualificationReason::Name,
                });

                continue;
            }

            return Ok(MethodLookupResult::Success(method));
        }

        Ok(MethodLookupResult::Failure(MethodLookupError {
            type_name: self_ty.name(self.tcx()),
            method_name: method_name.clone(),
            suggestions,
        }))
    }

    /// Returns all the methods defined directly within the given type.
    fn methods_defined_on(&self, self_ty: &'tcx lume_types::TypeRef) -> Vec<MethodId> {
        self_ty.methods(self.tcx())
    }
}
