use crate::*;

const TOKEN_NAMES: &[(&str, &str)] = &[
    ("import", "import_kw"),
    ("namespace", "namespace_kw"),
    ("fn", "fn_kw"),
    ("external", "extern_kw"),
    ("struct", "struct_kw"),
    ("impl", "impl_kw"),
    ("trait", "trait_kw"),
    ("enum", "enum_kw"),
    ("use", "use_kw"),
    ("pub", "pub_kw"),
    ("internal", "internal_kw"),
    ("priv", "priv_kw"),
    ("self", "self_expr"),
    ("Self", "self_type"),
    ("let", "let_kw"),
    ("break", "break_kw"),
    ("continue", "continue_kw"),
    ("return", "return_kw"),
    ("loop", "loop_kw"),
    ("for", "for_kw"),
    ("while", "while_kw"),
    ("as", "as_kw"),
    ("else", "else_kw"),
    ("if", "if_kw"),
    ("in", "in_kw"),
    ("is", "is_kw"),
    ("switch", "switch_kw"),
    ("unsafe", "unsafe_kw"),
    // Symbols
    ("+", "add"),
    ("-", "sub"),
    ("*", "mul"),
    ("/", "div"),
    ("++", "increment"),
    ("--", "decrement"),
    (">", "greater"),
    (">=", "gequal"),
    ("<", "less"),
    ("<=", "lequal"),
    ("==", "equal"),
    ("!=", "nequal"),
    ("=", "assign"),
    ("!", "not"),
    ("&", "and"),
    ("|", "or"),
    ("^", "xor"),
    ("_", "underscore"),
    // Brackets
    ("(", "left_paren"),
    (")", "right_paren"),
    ("{", "left_brace"),
    ("}", "right_brace"),
    ("[", "left_bracket"),
    ("]", "right_bracket"),
    // Punctuation
    (":", "colon"),
    (";", "semicolon"),
    (".", "dot"),
    ("..", "dot_dot"),
    ("...", "dot_dot_dot"),
    ("->", "arrow"),
    ("=>", "big_arrow"),
    ("///", "triple_slash"),
];

fn name_of_token(tok: &str) -> Option<&'static str> {
    TOKEN_NAMES
        .iter()
        .find_map(|(key, name)| (*key == tok).then_some(*name))
}

pub(crate) fn codegen<I>(entries: I) -> proc_macro2::TokenStream
where
    I: IntoIterator<Item = Entry>,
{
    let mut generated = quote! {
        //! DO NOT EDIT MANUALLY.
        //!
        //! This file is auto-generated from `tools/grammar/lume.ungram`. To
        //! regenerate this file, run the following command:
        //!
        //! ```sh
        //! cargo run --package grammar
        //! ```

        #![cfg_attr(rustfmt, rustfmt_skip)]

        use crate::*;
        use lume_syntax::*;
    };

    for entry in entries {
        generated.extend(codegen::codegen_entry(entry));
    }

    generated
}

pub(crate) fn codegen_entry(Entry { name, format }: Entry) -> proc_macro2::TokenStream {
    let name_ident = format_ident!("{name}");
    let uppercase_ident = format_ident!("{}", as_syntax_name(&name));

    match format {
        EntryFormat::OneOf(cases) => {
            let case_idents: Vec<_> = cases.iter().map(|case| format_ident!("{case}")).collect();
            let case_matchers: Vec<_> = cases
                .iter()
                .map(|case| {
                    let case_ident = format_ident!("{case}");
                    let syntax_name = format_ident!("{}", as_syntax_name(case));

                    quote! {
                        SyntaxKind::#syntax_name => Some(Self::#case_ident(#case_ident::cast(syntax).unwrap()))
                    }
                })
                .collect();

            quote! {
                #[derive(Hash, Debug, Clone, PartialEq, Eq)]
                pub enum #name_ident {
                    #(
                        #case_idents(#case_idents)
                    ),*
                }

                impl AstNode for #name_ident {
                    fn cast(syntax: SyntaxNode) -> Option<Self> {
                        match syntax.kind() {
                            #(
                                #case_matchers,
                            )*
                            _ => None,
                        }
                    }

                    fn syntax(&self) -> &SyntaxNode {
                        match self {
                            #(
                                Self::#case_idents(it) => it.syntax(),
                            )*
                        }
                    }
                }
            }
        }
        EntryFormat::Sequence(fields) => {
            let mut tokens = Vec::new();
            let mut nodes = Vec::new();
            let mut children = Vec::new();

            for field in fields {
                match field {
                    Field::Token { name, token } => {
                        if token.starts_with(['#', '@']) {
                            continue;
                        }

                        let token_kind = name_of_token(&token).unwrap_or(&token);
                        let syntax_name = as_syntax_name(token_kind);

                        let token_name = as_node_name(token_kind);

                        let token_ident = format_ident!("{}", name.as_ref().unwrap_or(&token_name));
                        let syntax_ident = format_ident!("{syntax_name}");

                        tokens.push(quote! {
                            pub fn #token_ident(&self) -> Option<SyntaxToken> {
                                crate::support::token(self.syntax(), SyntaxKind::#syntax_ident)
                            }
                        });
                    }
                    Field::Node { name, ty } => {
                        let mut node_name = as_node_name(name.as_deref().unwrap_or(&ty));
                        if node_name == "type" {
                            node_name = String::from("ty");
                        }

                        let node_ident = format_ident!("{node_name}");
                        let type_ident = format_ident!("{ty}");

                        nodes.push(quote! {
                            pub fn #node_ident(&self) -> Option<#type_ident> {
                                crate::support::child(self.syntax())
                            }
                        });
                    }
                    Field::Children { name, ty } => {
                        let mut node_name = as_node_name(name.as_deref().unwrap_or(&ty));
                        if node_name == "type" {
                            node_name = String::from("ty");
                        }

                        let node_ident = format_ident!("{node_name}");
                        let type_ident = format_ident!("{ty}");

                        children.push(quote! {
                            pub fn #node_ident(&self) -> AstChildren<#type_ident> {
                                crate::support::children(self.syntax())
                            }
                        });
                    }
                }
            }

            quote! {
                #[derive(Hash, Debug, Clone, PartialEq, Eq)]
                pub struct #name_ident {
                    syntax: SyntaxNode,
                }

                impl #name_ident {
                    #(#tokens)*
                    #(#nodes)*
                    #(#children)*
                }

                impl AstNode for #name_ident {
                    fn cast(syntax: SyntaxNode) -> Option<Self> {
                        match syntax.kind() {
                            SyntaxKind::#uppercase_ident => Some(#name_ident { syntax }),
                            _ => None,
                        }
                    }

                    fn syntax(&self) -> &SyntaxNode {
                        &self.syntax
                    }
                }
            }
        }
    }
}

pub(crate) fn format_codegen(tt: proc_macro2::TokenStream) -> String {
    let syn_file = syn::parse_file(&tt.to_string()).unwrap();

    prettyplease::unparse(&syn_file)
}

fn as_syntax_name(s: &str) -> String {
    split_camelcase(s)
        .into_iter()
        .map(|word| word.to_ascii_uppercase())
        .collect::<Vec<_>>()
        .join("_")
}

fn as_node_name(s: &str) -> String {
    split_camelcase(s)
        .into_iter()
        .map(|word| word.to_ascii_lowercase())
        .collect::<Vec<_>>()
        .join("_")
}

fn split_camelcase(s: &str) -> Vec<&str> {
    let mut words = Vec::new();
    let mut last_boundary_end = 0;

    for (idx, char) in s.char_indices() {
        if char.is_ascii_uppercase() && last_boundary_end != idx {
            let substr = &s[last_boundary_end..idx];
            words.push(substr);

            last_boundary_end = idx;
        }
    }

    if last_boundary_end != s.len() - 1 {
        let substr = &s[last_boundary_end..];
        words.push(substr);
    }

    words
}
