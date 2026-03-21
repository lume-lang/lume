mod codegen;

use std::path::PathBuf;

use quote::{format_ident, quote};
use ungrammar::Grammar;

#[derive(Debug)]
struct Map {
    pub grammar: Grammar,
    pub nodes: Vec<ungrammar::Node>,
    pub entries: Vec<Entry>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Entry {
    pub name: String,
    pub format: EntryFormat,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum EntryFormat {
    /// The node only consists of one of the following other nodes.
    OneOf(Vec<String>),

    Sequence(Vec<Field>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Field {
    /// The field is a reference to a specific token within it's parent entry.
    Token(String),

    /// The field is a reference to a specific node type within it's parent
    /// entry.
    Node { name: Option<String>, ty: String },

    /// The field is a reference to all children of the specific node type.
    Children { name: Option<String>, ty: String },
}

pub fn generate(grammar: &str) {
    let package_manifest_dir = PathBuf::from(std::env!("CARGO_MANIFEST_PATH"));
    let compiler_root = std::fs::canonicalize(package_manifest_dir.join("../../../")).unwrap();

    let generated_dir = compiler_root.join("compiler/lume_ast/src/generated/");
    let generated_file_rs = generated_dir.join("ast.rs");

    std::fs::create_dir_all(&generated_dir).expect("failed to create `generated` dir");

    let grammar: Grammar = grammar.parse().expect("failed to parse grammar");

    let mut map = Map {
        nodes: grammar.iter().collect(),
        grammar,
        entries: Vec::new(),
    };

    extract_enums(&mut map);

    let generated = codegen::codegen(map.entries);
    let unparsed = codegen::format_codegen(generated);

    std::fs::write(generated_file_rs, unparsed).unwrap();
}

fn as_enum_rule(rule: &ungrammar::Rule) -> Option<&[ungrammar::Rule]> {
    let ungrammar::Rule::Alt(rules) = rule else {
        return None;
    };

    rules
        .iter()
        .all(|r| matches!(r, ungrammar::Rule::Node(_) | ungrammar::Rule::Token(_)))
        .then_some(rules)
}

fn extract_enums(map: &mut Map) {
    for &node in &map.nodes {
        let rule = &map.grammar[node].rule;

        if let Some(rules) = as_enum_rule(rule) {
            let names: Vec<String> = rules
                .iter()
                .map(|r| match r {
                    ungrammar::Rule::Node(node) => map.grammar[*node].name.clone(),
                    ungrammar::Rule::Token(tok) => map.grammar[*tok].name.clone(),
                    _ => unreachable!(),
                })
                .collect();

            map.entries.push(Entry {
                name: map.grammar[node].name.clone(),
                format: EntryFormat::OneOf(names),
            });
        } else {
            let mut fields = Vec::new();
            lower_rule(&mut fields, None, &map.grammar, rule);

            map.entries.push(Entry {
                name: map.grammar[node].name.clone(),
                format: EntryFormat::Sequence(fields),
            });
        }
    }
}

fn lower_rule(fields: &mut Vec<Field>, label: Option<String>, grammar: &Grammar, rule: &ungrammar::Rule) {
    match rule {
        ungrammar::Rule::Node(node_field) => {
            fields.push(Field::Node {
                name: label.clone(),
                ty: grammar[*node_field].name.clone(),
            });
        }
        ungrammar::Rule::Token(token_field) => {
            fields.push(Field::Token(grammar[*token_field].name.clone()));
        }
        ungrammar::Rule::Opt(rule) => lower_rule(fields, label, grammar, rule),
        ungrammar::Rule::Seq(rules) | ungrammar::Rule::Alt(rules) => {
            for rule in rules {
                lower_rule(fields, label.clone(), grammar, rule);
            }
        }
        ungrammar::Rule::Rep(repitition) => match &**repitition {
            ungrammar::Rule::Node(node_field) => {
                fields.push(Field::Children {
                    name: label.clone(),
                    ty: grammar[*node_field].name.clone(),
                });
            }
            ungrammar::Rule::Token(token_field) => {
                fields.push(Field::Children {
                    name: label.clone(),
                    ty: grammar[*token_field].name.clone(),
                });
            }
            ungrammar::Rule::Labeled { label, rule } => {
                lower_rule(fields, Some(label.clone()), grammar, rule);
            }
            _ => unimplemented!(),
        },
        ungrammar::Rule::Labeled { label, rule } => {
            lower_rule(fields, Some(label.clone()), grammar, rule);
        }
    }
}
