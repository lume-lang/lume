use std::fmt::Formatter;

use crate::map::Map;
use crate::*;

pub trait PrettyPrint {
    /// Pretty print the instance for use in Manifold tests.
    #[expect(clippy::missing_errors_doc)]
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result;
}

impl<T: PrettyPrint> PrettyPrint for Option<T> {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(value) = self {
            value.pretty_fmt(map, f)
        } else {
            f.write_str("None")
        }
    }
}

struct PrettyItem<'a, T> {
    value: &'a T,
    map: &'a Map,
}

impl<T: PrettyPrint> std::fmt::Debug for PrettyItem<'_, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.value.pretty_fmt(self.map, f)
    }
}

macro_rules! pretty_item {
    ($expr:expr, $map:expr) => {
        &PrettyItem {
            value: &$expr,
            map: $map,
        }
    };
}

macro_rules! pretty_list {
    ($collection:expr, $map:expr) => {
        $collection
            .iter()
            .map(|item| PrettyItem { value: item, map: $map })
            .collect::<Vec<_>>()
    };
}

impl Map {
    pub fn pretty_fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let nodes = self
            .nodes
            .values()
            .filter(|node| node.is_item())
            .map(|item| PrettyItem { value: item, map: self })
            .collect::<Vec<_>>();

        f.debug_struct("Map").field("items", &nodes).finish()
    }
}

impl PrettyPrint for Identifier {
    fn pretty_fmt(&self, _map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Identifier")
            .field("name", &self.name)
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for NodeId {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        let node = map.expect_node(*self).unwrap();

        node.pretty_fmt(map, f)
    }
}

impl PrettyPrint for Node {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Function(node) => node.pretty_fmt(map, f),
            Self::Type(node) => node.pretty_fmt(map, f),
            Self::TraitImpl(node) => node.pretty_fmt(map, f),
            Self::Impl(node) => node.pretty_fmt(map, f),
            Self::Field(node) => node.pretty_fmt(map, f),
            Self::Method(node) => node.pretty_fmt(map, f),
            Self::TraitMethodDef(node) => node.pretty_fmt(map, f),
            Self::TraitMethodImpl(node) => node.pretty_fmt(map, f),
            Self::Pattern(node) => node.pretty_fmt(map, f),
            Self::Statement(node) => node.pretty_fmt(map, f),
            Self::Expression(node) => node.pretty_fmt(map, f),
        }
    }
}

impl PrettyPrint for FunctionDefinition {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        let parameters = pretty_list!(self.parameters, map);

        f.debug_struct("FunctionDefinition")
            .field("visibility", &self.visibility)
            .field("name", &self.name)
            .field("parameters", &parameters)
            .field("type_parameters", pretty_item!(self.type_parameters, map))
            .field("return_type", pretty_item!(self.return_type, map))
            .field("block", pretty_item!(self.block, map))
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for Parameter {
    fn pretty_fmt(&self, _map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Parameter")
            .field("name", &self.name)
            .field("param_type", &self.param_type)
            .field("vararg", &self.vararg)
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for TypeDefinition {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Enum(item) => item.pretty_fmt(map, f),
            Self::Struct(item) => item.pretty_fmt(map, f),
            Self::Trait(item) => item.pretty_fmt(map, f),
        }
    }
}

impl PrettyPrint for EnumDefinition {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        let cases = pretty_list!(self.cases, map);

        f.debug_struct("EnumDefinition")
            .field("visibility", &self.visibility)
            .field("name", &self.name)
            .field("type_parameters", pretty_item!(self.type_parameters, map))
            .field("cases", &cases)
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for EnumDefinitionCase {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        let parameters = pretty_list!(self.parameters, map);

        f.debug_struct("EnumDefinitionCase")
            .field("name", &self.name)
            .field("parameters", &parameters)
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for StructDefinition {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        let fields = pretty_list!(self.fields, map);

        f.debug_struct("StructDefinition")
            .field("visibility", &self.visibility)
            .field("name", &self.name)
            .field("builtin", &self.builtin)
            .field("fields", &fields)
            .field("type_parameters", pretty_item!(self.type_parameters, map))
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for Implementation {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        let methods = pretty_list!(self.methods, map);

        f.debug_struct("Implementation")
            .field("target", &self.target)
            .field("methods", &methods)
            .field("type_parameters", pretty_item!(self.type_parameters, map))
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for Field {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Field")
            .field("visibility", &self.visibility)
            .field("name", &self.name)
            .field("field_type", &self.field_type)
            .field("default_value", pretty_item!(self.default_value, map))
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for MethodDefinition {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        let parameters = pretty_list!(self.parameters, map);

        f.debug_struct("MethodDefinition")
            .field("visibility", &self.visibility)
            .field("name", &self.name)
            .field("parameters", &parameters)
            .field("type_parameters", pretty_item!(self.type_parameters, map))
            .field("return_type", pretty_item!(self.return_type, map))
            .field("block", pretty_item!(self.block, map))
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for TraitDefinition {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        let methods = pretty_list!(self.methods, map);

        f.debug_struct("TraitDefinition")
            .field("visibility", &self.visibility)
            .field("name", &self.name)
            .field("methods", &methods)
            .field("type_parameters", pretty_item!(self.type_parameters, map))
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for TraitMethodDefinition {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        let parameters = pretty_list!(self.parameters, map);

        f.debug_struct("TraitMethodDefinition")
            .field("name", &self.name)
            .field("parameters", &parameters)
            .field("type_parameters", pretty_item!(self.type_parameters, map))
            .field("return_type", pretty_item!(self.return_type, map))
            .field("block", pretty_item!(self.block, map))
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for TraitImplementation {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        let methods = pretty_list!(self.methods, map);

        f.debug_struct("TraitImplementation")
            .field("name", &self.name)
            .field("target", &self.target)
            .field("methods", &methods)
            .field("type_parameters", pretty_item!(self.type_parameters, map))
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for TraitMethodImplementation {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        let parameters = pretty_list!(self.parameters, map);

        f.debug_struct("TraitMethodImplementation")
            .field("name", &self.name)
            .field("parameters", &parameters)
            .field("type_parameters", pretty_item!(self.type_parameters, map))
            .field("return_type", pretty_item!(self.return_type, map))
            .field("block", pretty_item!(self.block, map))
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for Block {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        let statements = pretty_list!(self.statements, map);

        f.debug_struct("Block")
            .field("statements", &statements)
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for Statement {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.kind.pretty_fmt(map, f)
    }
}

impl PrettyPrint for StatementKind {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Variable(s) => s.pretty_fmt(map, f),
            Self::Break(s) => s.pretty_fmt(map, f),
            Self::Continue(s) => s.pretty_fmt(map, f),
            Self::Final(s) => s.pretty_fmt(map, f),
            Self::Return(s) => s.pretty_fmt(map, f),
            Self::InfiniteLoop(s) => s.pretty_fmt(map, f),
            Self::IteratorLoop(s) => s.pretty_fmt(map, f),
            Self::Expression(s) => s.pretty_fmt(map, f),
        }
    }
}

impl PrettyPrint for VariableDeclaration {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("VariableDeclaration")
            .field("name", &self.name.as_str())
            .field("declared_type", &self.declared_type)
            .field("value", pretty_item!(self.value, map))
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for Break {
    fn pretty_fmt(&self, _map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Break").field("location", &self.location).finish()
    }
}

impl PrettyPrint for Continue {
    fn pretty_fmt(&self, _map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Continue").field("location", &self.location).finish()
    }
}

impl PrettyPrint for Final {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Final")
            .field("value", pretty_item!(self.value, map))
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for Return {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Return")
            .field("value", pretty_item!(self.value, map))
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for InfiniteLoop {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("InfiniteLoop")
            .field("block", pretty_item!(self.block, map))
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for IteratorLoop {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("IteratorLoop")
            .field("collection", pretty_item!(self.collection, map))
            .field("block", pretty_item!(self.block, map))
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for Expression {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.kind.pretty_fmt(map, f)
    }
}

impl PrettyPrint for ExpressionKind {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Assignment(e) => e.pretty_fmt(map, f),
            Self::Binary(e) => e.pretty_fmt(map, f),
            Self::Cast(e) => e.pretty_fmt(map, f),
            Self::Construct(e) => e.pretty_fmt(map, f),
            Self::StaticCall(e) => e.pretty_fmt(map, f),
            Self::InstanceCall(e) => e.pretty_fmt(map, f),
            Self::IntrinsicCall(e) => e.pretty_fmt(map, f),
            Self::If(e) => e.pretty_fmt(map, f),
            Self::Is(e) => e.pretty_fmt(map, f),
            Self::Literal(e) => e.pretty_fmt(map, f),
            Self::Logical(e) => e.pretty_fmt(map, f),
            Self::Member(e) => e.pretty_fmt(map, f),
            Self::Scope(e) => e.pretty_fmt(map, f),
            Self::Switch(e) => e.pretty_fmt(map, f),
            Self::Variable(e) => e.pretty_fmt(map, f),
            Self::Variant(e) => e.pretty_fmt(map, f),
        }
    }
}

impl PrettyPrint for Assignment {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Assignment")
            .field("target", pretty_item!(self.target, map))
            .field("value", pretty_item!(self.value, map))
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for Binary {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Binary")
            .field("op", &self.op.kind)
            .field("lhs", pretty_item!(self.lhs, map))
            .field("rhs", pretty_item!(self.rhs, map))
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for Cast {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Cast")
            .field("source", pretty_item!(self.source, map))
            .field("target", &self.target)
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for Construct {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        let fields = pretty_list!(self.fields, map);

        f.debug_struct("Construct")
            .field("path", &self.path)
            .field("fields", &fields)
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for ConstructorField {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ConstructorField")
            .field("name", &self.name.as_str())
            .field("value", pretty_item!(self.value, map))
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for StaticCall {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        let arguments = pretty_list!(self.arguments, map);

        f.debug_struct("StaticCall")
            .field("name", &self.name)
            .field("arguments", &arguments)
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for InstanceCall {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        let arguments = pretty_list!(self.arguments, map);

        f.debug_struct("InstanceCall")
            .field("name", &self.name)
            .field("callee", pretty_item!(self.callee, map))
            .field("arguments", &arguments)
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for IntrinsicCall {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        let arguments = pretty_list!(self.arguments, map);

        f.debug_struct("IntrinsicCall")
            .field("name", &self.name)
            .field("arguments", &arguments)
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for If {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        let cases = pretty_list!(self.cases, map);

        f.debug_struct("If")
            .field("cases", &cases)
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for Is {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Is")
            .field("target", pretty_item!(self.target, map))
            .field("pattern", pretty_item!(self.pattern, map))
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for Condition {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Condition")
            .field("condition", pretty_item!(self.condition, map))
            .field("block", pretty_item!(self.block, map))
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for Literal {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Literal")
            .field("kind", pretty_item!(self.kind, map))
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for LiteralKind {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(l) => l.pretty_fmt(map, f),
            Self::Float(l) => l.pretty_fmt(map, f),
            Self::String(l) => l.pretty_fmt(map, f),
            Self::Boolean(l) => l.pretty_fmt(map, f),
        }
    }
}

impl PrettyPrint for IntLiteral {
    fn pretty_fmt(&self, _map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("IntLiteral")
            .field("value", &self.value)
            .field("kind", &self.kind)
            .finish()
    }
}

impl PrettyPrint for FloatLiteral {
    fn pretty_fmt(&self, _map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FloatLiteral")
            .field("value", &self.value)
            .field("kind", &self.kind)
            .finish()
    }
}

impl PrettyPrint for StringLiteral {
    fn pretty_fmt(&self, _map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("StringLiteral").field("value", &self.value).finish()
    }
}

impl PrettyPrint for BooleanLiteral {
    fn pretty_fmt(&self, _map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BooleanLiteral").field("value", &self.value).finish()
    }
}

impl PrettyPrint for Logical {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Logical")
            .field("op", &self.op.kind)
            .field("lhs", pretty_item!(self.lhs, map))
            .field("rhs", pretty_item!(self.rhs, map))
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for Member {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Member")
            .field("callee", pretty_item!(self.callee, map))
            .field("name", &self.name)
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for PatternField {
    fn pretty_fmt(&self, _map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("PatternField")
            .field("pattern", &self.pattern)
            .field("field", &self.field)
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for Scope {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        let body = pretty_list!(self.body, map);

        f.debug_struct("Scope")
            .field("body", &body)
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for Switch {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        let cases = pretty_list!(self.cases, map);

        f.debug_struct("Switch")
            .field("operand", pretty_item!(self.operand, map))
            .field("cases", &cases)
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for SwitchCase {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SwitchCase")
            .field("pattern", pretty_item!(self.pattern, map))
            .field("branch", pretty_item!(self.branch, map))
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for Variable {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Variable")
            .field("name", &self.name)
            .field("reference", pretty_item!(self.reference, map))
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for VariableSource {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Parameter(src) => src.pretty_fmt(map, f),
            Self::Variable(src) => src.pretty_fmt(map, f),
            Self::Pattern(src) => src.pretty_fmt(map, f),
        }
    }
}

impl PrettyPrint for Variant {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        let arguments = pretty_list!(self.arguments, map);

        f.debug_struct("Variant")
            .field("name", &self.name)
            .field("args", &arguments)
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for Pattern {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Pattern")
            .field("kind", pretty_item!(self.kind, map))
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for PatternKind {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(pat) => pat.pretty_fmt(map, f),
            Self::Identifier(pat) => pat.pretty_fmt(map, f),
            Self::Variant(pat) => pat.pretty_fmt(map, f),
            Self::Wildcard(pat) => pat.pretty_fmt(map, f),
        }
    }
}

impl PrettyPrint for LiteralPattern {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LiteralPattern")
            .field("literal", pretty_item!(self.literal, map))
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for IdentifierPattern {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("IdentifierPattern")
            .field("name", pretty_item!(self.name, map))
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for VariantPattern {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        let fields = pretty_list!(self.fields, map);

        f.debug_struct("VariantPattern")
            .field("name", &self.name)
            .field("fields", &fields)
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for WildcardPattern {
    fn pretty_fmt(&self, _map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("WildcardPattern")
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for TypeParameters {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut list = f.debug_list();

        for param in &self.inner {
            list.entry(pretty_item!(param.to_owned(), map));
        }

        list.finish()
    }
}

impl PrettyPrint for TypeParameter {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        let constraints = pretty_list!(self.constraints, map);

        f.debug_struct("TypeParameter")
            .field("name", pretty_item!(self.name, map))
            .field("constraints", &constraints)
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for Type {
    fn pretty_fmt(&self, _map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Type")
            .field("name", &self.name)
            .field("location", &self.location)
            .finish()
    }
}
