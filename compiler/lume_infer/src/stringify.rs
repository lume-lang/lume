use lume_errors::Result;

use crate::TyInferCtx;
use crate::query::CallReference;

#[derive(Default, Hash, Debug, Clone, Copy, PartialEq, Eq)]
pub enum IncludeVisibility {
    Always,

    #[default]
    NonPrivate,
    Never,
}

impl From<bool> for IncludeVisibility {
    fn from(value: bool) -> Self {
        if value { Self::Always } else { Self::Never }
    }
}

#[derive(Default, Hash, Debug, Clone, Copy, PartialEq, Eq)]
pub enum IncludeReturnType {
    Always,

    #[default]
    NonVoid,
    Never,
}

impl From<bool> for IncludeReturnType {
    fn from(value: bool) -> Self {
        if value { Self::Always } else { Self::Never }
    }
}

#[derive(Hash, Debug, Clone, Copy, PartialEq, Eq)]
pub enum IncludeParameters {
    Show { include_names: bool, include_types: bool },

    Elipsis,
}

impl From<bool> for IncludeParameters {
    fn from(value: bool) -> Self {
        if value { Self::default() } else { Self::Elipsis }
    }
}

impl Default for IncludeParameters {
    fn default() -> Self {
        Self::Show {
            include_names: true,
            include_types: true,
        }
    }
}

pub struct Stringifier<'tcx> {
    call_reference: CallReference,
    tcx: &'tcx TyInferCtx,

    include_visibility: IncludeVisibility,
    include_fn_keyword: bool,
    include_name: bool,

    include_parameters: IncludeParameters,
    include_return_type: IncludeReturnType,
}

impl<'tcx> Stringifier<'tcx> {
    pub fn new(call_reference: CallReference, tcx: &'tcx TyInferCtx) -> Self {
        Stringifier {
            call_reference,
            tcx,
            include_visibility: IncludeVisibility::default(),
            include_fn_keyword: true,
            include_name: true,
            include_parameters: IncludeParameters::default(),
            include_return_type: IncludeReturnType::default(),
        }
    }

    pub fn include_visibility<V: Into<IncludeVisibility>>(mut self, include: V) -> Self {
        self.include_visibility = include.into();
        self
    }

    pub fn include_fn_keyword(mut self, include: bool) -> Self {
        self.include_fn_keyword = include;
        self
    }

    pub fn include_name(mut self, include: bool) -> Self {
        self.include_name = include;
        self
    }

    pub fn include_parameters<V: Into<IncludeParameters>>(mut self, include: V) -> Self {
        self.include_parameters = include.into();
        self
    }

    pub fn include_return_type<V: Into<IncludeReturnType>>(mut self, include: V) -> Self {
        self.include_return_type = include.into();
        self
    }

    pub fn stringify(self) -> Result<String> {
        let visibility = self.tcx.visibility_of(self.call_reference.id());
        let signature = self.tcx.signature_of_call_ref(self.call_reference)?;

        let visibility_str = match self.include_visibility {
            IncludeVisibility::Never => "",
            IncludeVisibility::NonPrivate if visibility.is_none_or(|vis| vis == lume_hir::Visibility::Private) => "",
            IncludeVisibility::NonPrivate | IncludeVisibility::Always => match visibility {
                Some(lume_hir::Visibility::Public) => "pub ",
                Some(lume_hir::Visibility::Internal) => "pub(internal) ",
                Some(lume_hir::Visibility::Private) => "priv ",
                None => "",
            },
        };

        let name_str = if self.include_name {
            self.tcx.hir_path_of_node(self.call_reference.id()).name().to_string()
        } else {
            String::new()
        };

        let parameters_str = match self.include_parameters {
            IncludeParameters::Elipsis => String::from("..."),
            IncludeParameters::Show {
                include_names,
                include_types,
            } => {
                let mut param_strs = Vec::with_capacity(signature.params.len());

                for parameter in &signature.params {
                    let param_name = if include_names {
                        format!("{}: ", parameter.name)
                    } else {
                        String::new()
                    };

                    let param_type = if include_types {
                        let vararg = if parameter.vararg { "..." } else { "" };
                        let type_name = self.tcx.new_named_type(&parameter.ty, false)?;

                        format!("{vararg}{type_name}")
                    } else {
                        String::new()
                    };

                    param_strs.push(format!("{param_name}{param_type}"));
                }

                param_strs.join(", ")
            }
        };

        let return_type_str = match self.include_return_type {
            IncludeReturnType::Never => String::new(),
            IncludeReturnType::NonVoid if signature.ret_ty.is_void() => String::new(),
            IncludeReturnType::NonVoid | IncludeReturnType::Always => {
                format!(" -> {}", self.tcx.new_named_type(&signature.ret_ty, false)?)
            }
        };

        Ok(format!(
            "{visibility_str}{}{name_str}({parameters_str}){return_type_str}",
            if self.include_fn_keyword { "fn " } else { "" }
        ))
    }
}

impl TyInferCtx {
    #[inline]
    pub fn stringifier(&'_ self, call_ref: CallReference) -> Stringifier<'_> {
        Stringifier::new(call_ref, self)
    }
}
