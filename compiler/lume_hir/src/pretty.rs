use std::fmt::Formatter;

use crate::map::Map;
use crate::*;

pub trait PrettyPrint {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result;
}

impl Map {
    pub fn pretty_fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Map")
            .field_with("items", |f| {
                let mut list = f.debug_list();

                for item in self.items.values() {
                    list.entry_with(|f| item.pretty_fmt(self, f));
                }

                list.finish()
            })
            .finish()
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

impl PrettyPrint for Item {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Function(item) => item.pretty_fmt(map, f),
            Self::Type(item) => item.pretty_fmt(map, f),
            Self::TraitImpl(item) => item.pretty_fmt(map, f),
            Self::Impl(item) => item.pretty_fmt(map, f),
        }
    }
}

impl PrettyPrint for FunctionDefinition {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FunctionDefinition")
            .field("visibility", &self.visibility)
            .field("name", &self.name)
            .field_with("parameters", |f| {
                let mut list = f.debug_list();

                for param in &self.parameters {
                    list.entry_with(|f| param.pretty_fmt(map, f));
                }

                list.finish()
            })
            .field_with("type_parameters", |f| self.type_parameters.pretty_fmt(map, f))
            .field_with("return_type", |f| self.return_type.pretty_fmt(map, f))
            .field_with("block", |f| {
                if let Some(block) = &self.block {
                    block.pretty_fmt(map, f)
                } else {
                    f.write_str("None")
                }
            })
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
        f.debug_struct("EnumDefinition")
            .field("visibility", &self.visibility)
            .field("name", &self.name)
            .field_with("type_parameters", |f| self.type_parameters.pretty_fmt(map, f))
            .field_with("cases", |f| {
                let mut list = f.debug_list();

                for case in &self.cases {
                    list.entry_with(|f| case.pretty_fmt(map, f));
                }

                list.finish()
            })
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for EnumDefinitionCase {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("EnumDefinitionCase")
            .field("name", &self.name)
            .field_with("parameters", |f| {
                let mut list = f.debug_list();

                for param in &self.parameters {
                    list.entry_with(|f| param.pretty_fmt(map, f));
                }

                list.finish()
            })
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for StructDefinition {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("StructDefinition")
            .field("visibility", &self.visibility)
            .field("name", &self.name)
            .field("builtin", &self.builtin)
            .field_with("fields", |f| {
                let mut list = f.debug_list();

                for field in &self.fields {
                    list.entry_with(|f| field.pretty_fmt(map, f));
                }

                list.finish()
            })
            .field_with("type_parameters", |f| self.type_parameters.pretty_fmt(map, f))
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for Implementation {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Implementation")
            .field("target", &self.target)
            .field_with("methods", |f| {
                let mut list = f.debug_list();

                for method in &self.methods {
                    list.entry_with(|f| method.pretty_fmt(map, f));
                }

                list.finish()
            })
            .field_with("type_parameters", |f| self.type_parameters.pretty_fmt(map, f))
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
            .field_with("default_value", |f| {
                if let Some(default_value) = self.default_value {
                    default_value.pretty_fmt(map, f)
                } else {
                    f.write_str("None")
                }
            })
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for MethodDefinition {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MethodDefinition")
            .field("visibility", &self.visibility)
            .field("name", &self.name)
            .field_with("parameters", |f| {
                let mut list = f.debug_list();

                for param in &self.parameters {
                    list.entry_with(|f| param.pretty_fmt(map, f));
                }

                list.finish()
            })
            .field_with("type_parameters", |f| self.type_parameters.pretty_fmt(map, f))
            .field_with("return_type", |f| self.return_type.pretty_fmt(map, f))
            .field_with("block", |f| {
                if let Some(block) = &self.block {
                    block.pretty_fmt(map, f)
                } else {
                    f.write_str("None")
                }
            })
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for TraitDefinition {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TraitDefinition")
            .field("visibility", &self.visibility)
            .field("name", &self.name)
            .field_with("methods", |f| {
                let mut list = f.debug_list();

                for method in &self.methods {
                    list.entry_with(|f| method.pretty_fmt(map, f));
                }

                list.finish()
            })
            .field_with("type_parameters", |f| self.type_parameters.pretty_fmt(map, f))
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for TraitMethodDefinition {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TraitMethodDefinition")
            .field("visibility", &self.visibility)
            .field("name", &self.name)
            .field_with("parameters", |f| {
                let mut list = f.debug_list();

                for param in &self.parameters {
                    list.entry_with(|f| param.pretty_fmt(map, f));
                }

                list.finish()
            })
            .field_with("type_parameters", |f| self.type_parameters.pretty_fmt(map, f))
            .field_with("return_type", |f| self.return_type.pretty_fmt(map, f))
            .field_with("block", |f| {
                if let Some(block) = &self.block {
                    block.pretty_fmt(map, f)
                } else {
                    f.write_str("None")
                }
            })
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for TraitImplementation {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TraitDefinition")
            .field("visibility", &self.visibility)
            .field("name", &self.name)
            .field("target", &self.target)
            .field_with("methods", |f| {
                let mut list = f.debug_list();

                for method in &self.methods {
                    list.entry_with(|f| method.pretty_fmt(map, f));
                }

                list.finish()
            })
            .field_with("type_parameters", |f| self.type_parameters.pretty_fmt(map, f))
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for TraitMethodImplementation {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TraitMethodImplementation")
            .field("visibility", &self.visibility)
            .field("name", &self.name)
            .field_with("parameters", |f| {
                let mut list = f.debug_list();

                for param in &self.parameters {
                    list.entry_with(|f| param.pretty_fmt(map, f));
                }

                list.finish()
            })
            .field_with("type_parameters", |f| self.type_parameters.pretty_fmt(map, f))
            .field_with("return_type", |f| self.return_type.pretty_fmt(map, f))
            .field_with("block", |f| self.block.pretty_fmt(map, f))
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for Block {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Block")
            .field_with("statements", |f| {
                let mut list = f.debug_list();

                for stmt in &self.statements {
                    list.entry_with(|f| stmt.pretty_fmt(map, f));
                }

                list.finish()
            })
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for StatementId {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        let stmt = map.expect_statement(*self).unwrap();

        stmt.pretty_fmt(map, f)
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
            .field_with("value", |f| self.value.pretty_fmt(map, f))
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
            .field_with("value", |f| self.value.pretty_fmt(map, f))
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for Return {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Return")
            .field_with("value", |f| {
                if let Some(value) = self.value {
                    value.pretty_fmt(map, f)
                } else {
                    f.write_str("None")
                }
            })
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for InfiniteLoop {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("InfiniteLoop")
            .field_with("block", |f| self.block.pretty_fmt(map, f))
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for IteratorLoop {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("IteratorLoop")
            .field_with("collection", |f| self.collection.pretty_fmt(map, f))
            .field_with("block", |f| self.block.pretty_fmt(map, f))
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for ExpressionId {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        let expr = map.expect_expression(*self).unwrap();

        expr.pretty_fmt(map, f)
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
            Self::Literal(e) => e.pretty_fmt(map, f),
            Self::Logical(e) => e.pretty_fmt(map, f),
            Self::Member(e) => e.pretty_fmt(map, f),
            Self::Field(e) => e.pretty_fmt(map, f),
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
            .field_with("target", |f| self.target.pretty_fmt(map, f))
            .field_with("value", |f| self.value.pretty_fmt(map, f))
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for Binary {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Binary")
            .field("op", &self.op.kind)
            .field_with("lhs", |f| self.lhs.pretty_fmt(map, f))
            .field_with("rhs", |f| self.rhs.pretty_fmt(map, f))
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for Cast {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Cast")
            .field_with("source", |f| self.source.pretty_fmt(map, f))
            .field("target", &self.target)
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for Construct {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Construct")
            .field("path", &self.path)
            .field_with("fields", |f| {
                let mut list = f.debug_list();

                for field in &self.fields {
                    list.entry_with(|f| field.pretty_fmt(map, f));
                }

                list.finish()
            })
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for ConstructorField {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ConstructorField")
            .field("name", &self.name.as_str())
            .field_with("value", |f| self.value.pretty_fmt(map, f))
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for StaticCall {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("StaticCall")
            .field("name", &self.name)
            .field_with("arguments", |f| {
                let mut list = f.debug_list();

                for arg in &self.arguments {
                    list.entry_with(|f| arg.pretty_fmt(map, f));
                }

                list.finish()
            })
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for InstanceCall {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("InstanceCall")
            .field("name", &self.name)
            .field_with("callee", |f| self.callee.pretty_fmt(map, f))
            .field_with("arguments", |f| {
                let mut list = f.debug_list();

                for arg in &self.arguments {
                    list.entry_with(|f| arg.pretty_fmt(map, f));
                }

                list.finish()
            })
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for IntrinsicCall {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("IntrinsicCall")
            .field("name", &self.name)
            .field_with("arguments", |f| {
                let mut list = f.debug_list();

                for arg in &self.arguments {
                    list.entry_with(|f| arg.pretty_fmt(map, f));
                }

                list.finish()
            })
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for If {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("If")
            .field_with("cases", |f| {
                let mut list = f.debug_list();

                for case in &self.cases {
                    list.entry_with(|f| case.pretty_fmt(map, f));
                }

                list.finish()
            })
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for Condition {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Condition")
            .field_with("condition", |f| {
                if let Some(condition) = self.condition {
                    condition.pretty_fmt(map, f)
                } else {
                    f.write_str("None")
                }
            })
            .field_with("block", |f| self.block.pretty_fmt(map, f))
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for Literal {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Literal")
            .field_with("kind", |f| self.kind.pretty_fmt(map, f))
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
            .field_with("lhs", |f| self.lhs.pretty_fmt(map, f))
            .field_with("rhs", |f| self.rhs.pretty_fmt(map, f))
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for Member {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Member")
            .field_with("callee", |f| self.callee.pretty_fmt(map, f))
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
        f.debug_struct("Scope")
            .field_with("body", |f| {
                let mut list = f.debug_list();

                for stmt in &self.body {
                    list.entry_with(|f| stmt.pretty_fmt(map, f));
                }

                list.finish()
            })
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for Switch {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Switch")
            .field_with("operand", |f| self.operand.pretty_fmt(map, f))
            .field_with("cases", |f| {
                let mut list = f.debug_list();

                for case in &self.cases {
                    list.entry_with(|f| case.pretty_fmt(map, f));
                }

                list.finish()
            })
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for SwitchCase {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SwitchCase")
            .field_with("pattern", |f| self.pattern.pretty_fmt(map, f))
            .field_with("branch", |f| self.branch.pretty_fmt(map, f))
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for Variable {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Variable")
            .field("name", &self.name)
            .field_with("reference", |f| self.reference.pretty_fmt(map, f))
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
        f.debug_struct("Variant")
            .field("name", &self.name)
            .field_with("args", |f| {
                let mut list = f.debug_list();

                for arg in &self.arguments {
                    list.entry_with(|f| arg.pretty_fmt(map, f));
                }

                list.finish()
            })
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for Pattern {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Pattern")
            .field_with("kind", |f| self.kind.pretty_fmt(map, f))
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
            .field_with("literal", |f| self.literal.pretty_fmt(map, f))
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for IdentifierPattern {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("IdentifierPattern")
            .field_with("name", |f| self.name.pretty_fmt(map, f))
            .field("location", &self.location)
            .finish()
    }
}

impl PrettyPrint for VariantPattern {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("VariantPattern")
            .field_with("fields", |f| {
                let mut list = f.debug_list();

                for field in &self.fields {
                    list.entry_with(|f| field.pretty_fmt(map, f));
                }

                list.finish()
            })
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
            list.entry_with(|f| param.pretty_fmt(map, f));
        }

        list.finish()
    }
}

impl PrettyPrint for TypeParameter {
    fn pretty_fmt(&self, map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TypeParameter")
            .field_with("name", |f| self.name.pretty_fmt(map, f))
            .field_with("constraints", |f| {
                let mut list = f.debug_list();

                for constraint in &self.constraints {
                    list.entry_with(|f| constraint.pretty_fmt(map, f));
                }

                list.finish()
            })
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
