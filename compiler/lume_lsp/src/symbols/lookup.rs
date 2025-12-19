use indexmap::IndexSet;
use lume_errors::Result;
use lume_hir::{Visitor, WithLocation as _, traverse};
use lume_infer::query::CallReference;
use lume_span::{Location, NodeId};

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub(crate) struct SymbolEntry {
    pub location: Location,
    pub kind: SymbolKind,
}

impl PartialOrd for SymbolEntry {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for SymbolEntry {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.location.file.package.cmp(&other.location.file.package) {
            std::cmp::Ordering::Less => return std::cmp::Ordering::Less,
            std::cmp::Ordering::Greater => return std::cmp::Ordering::Greater,
            std::cmp::Ordering::Equal => {}
        }

        match self.location.file.id.1.cmp(&other.location.file.id.1) {
            std::cmp::Ordering::Less => std::cmp::Ordering::Less,
            std::cmp::Ordering::Greater => std::cmp::Ordering::Greater,
            std::cmp::Ordering::Equal => self.location.index.start.cmp(&other.location.index.start),
        }
    }
}

#[derive(Hash, Debug, Clone, PartialEq, Eq)]
pub(crate) enum SymbolKind {
    /// Symbol refers to a generic type with a pathed name.
    Type { name: lume_hir::Path },

    /// Symbol refers to some callable - method, function or intrinsic.
    Callable { reference: CallReference },

    /// Symbol refers to a field within a structure.
    Field { id: NodeId },

    /// Symbol refers to a variant, modelled after some enum case.
    Variant { name: lume_hir::Path },

    /// Symbol refers to a generic pattern.
    Pattern { id: NodeId },

    /// Symbol refers to a call expression.
    Call { id: NodeId },

    /// Symbol refers to a literal expression.
    Literal { id: NodeId },

    /// Symbol refers to a member expression.
    Member {
        callee: NodeId,
        field: lume_hir::Identifier,
    },

    /// Symbol refers to a variable declaration.
    VariableDeclaration { id: NodeId },

    /// Symbol refers to a variable reference.
    VariableReference { id: NodeId },
}

#[derive(Default)]
pub(crate) struct SymbolLookup {
    symbols: IndexSet<SymbolEntry>,
}

impl SymbolLookup {
    pub fn from_hir(hir: &lume_hir::Map) -> Self {
        let mut visitor = LocationVisitor::default();
        traverse(hir, &mut visitor).expect("HIR traversal");

        Self {
            symbols: visitor.symbols,
        }
    }

    pub fn extend(&mut self, other: SymbolLookup) {
        self.symbols.extend(other.symbols);
    }

    pub fn lookup_position(&self, location: Location) -> Option<&SymbolEntry> {
        let idx = location.index.start;

        let symbols_within_range = self.symbols.iter().filter(|sym| {
            sym.location.file.id == location.file.id && sym.location.start() <= idx && sym.location.end() >= idx
        });

        if let Some(sym) = symbols_within_range.min_by_key(|sym| sym.location.index.len()) {
            return Some(sym);
        }

        None
    }
}

#[derive(Default)]
struct LocationVisitor {
    symbols: IndexSet<SymbolEntry>,
}

impl Visitor for LocationVisitor {
    fn visit_type(&mut self, ty: &lume_hir::Type) -> Result<()> {
        self.symbols.insert_sorted(SymbolEntry {
            kind: SymbolKind::Type { name: ty.name.clone() },
            location: ty.location,
        });

        Ok(())
    }

    fn visit_node(&mut self, node: &lume_hir::Node) -> Result<()> {
        match node {
            lume_hir::Node::Function(func) => {
                self.symbols.insert_sorted(SymbolEntry {
                    kind: SymbolKind::Callable {
                        reference: CallReference::Function(func.id),
                    },
                    location: func.name.location,
                });
            }
            lume_hir::Node::Method(method) => {
                self.symbols.insert_sorted(SymbolEntry {
                    kind: SymbolKind::Callable {
                        reference: CallReference::Method(method.id),
                    },
                    location: method.name.location,
                });
            }
            lume_hir::Node::TraitMethodDef(method) => {
                self.symbols.insert_sorted(SymbolEntry {
                    kind: SymbolKind::Callable {
                        reference: CallReference::Method(method.id),
                    },
                    location: method.name.location,
                });
            }
            lume_hir::Node::TraitMethodImpl(method) => {
                self.symbols.insert_sorted(SymbolEntry {
                    kind: SymbolKind::Callable {
                        reference: CallReference::Method(method.id),
                    },
                    location: method.name.location,
                });
            }
            lume_hir::Node::Type(type_def) => match type_def {
                lume_hir::TypeDefinition::Struct(struct_def) => {
                    self.symbols.insert_sorted(SymbolEntry {
                        kind: SymbolKind::Type {
                            name: struct_def.name.clone(),
                        },
                        location: struct_def.name().location(),
                    });
                }
                lume_hir::TypeDefinition::Trait(trait_def) => {
                    self.symbols.insert_sorted(SymbolEntry {
                        kind: SymbolKind::Type {
                            name: trait_def.name.clone(),
                        },
                        location: trait_def.name().location(),
                    });
                }
                lume_hir::TypeDefinition::Enum(enum_def) => {
                    self.symbols.insert_sorted(SymbolEntry {
                        kind: SymbolKind::Type {
                            name: enum_def.name.clone(),
                        },
                        location: enum_def.name().location(),
                    });
                }
                lume_hir::TypeDefinition::TypeParameter(type_param) => {
                    self.symbols.insert_sorted(SymbolEntry {
                        kind: SymbolKind::Type {
                            name: lume_hir::Path::rooted(lume_hir::PathSegment::ty(type_param.name.clone())),
                        },
                        location: type_param.location(),
                    });
                }
            },
            lume_hir::Node::Field(field) => {
                self.symbols.insert_sorted(SymbolEntry {
                    kind: SymbolKind::Field { id: field.id },
                    location: field.name.location,
                });
            }
            _ => {}
        }

        Ok(())
    }

    fn visit_stmt(&mut self, stmt: &lume_hir::Statement) -> Result<()> {
        match &stmt.kind {
            lume_hir::StatementKind::Variable(stmt) => {
                self.symbols.insert(SymbolEntry {
                    kind: SymbolKind::VariableDeclaration { id: stmt.id },
                    location: stmt.name.location,
                });
            }
            _ => {}
        };

        Ok(())
    }

    fn visit_expr(&mut self, expr: &lume_hir::Expression) -> Result<()> {
        match &expr.kind {
            lume_hir::ExpressionKind::Assignment(_) => {}
            lume_hir::ExpressionKind::Cast(_) => {}
            lume_hir::ExpressionKind::Construct(expr) => {
                self.symbols.insert(SymbolEntry {
                    kind: SymbolKind::Type {
                        name: expr.path.clone(),
                    },
                    location: expr.path.name().location,
                });
            }
            lume_hir::ExpressionKind::StaticCall(expr) => {
                self.symbols.insert(SymbolEntry {
                    kind: SymbolKind::Call { id: expr.id },
                    location: expr.name.name().location,
                });
            }
            lume_hir::ExpressionKind::InstanceCall(expr) => {
                self.symbols.insert(SymbolEntry {
                    kind: SymbolKind::Call { id: expr.id },
                    location: expr.name.location(),
                });
            }
            lume_hir::ExpressionKind::IntrinsicCall(expr) => {
                self.symbols.insert(SymbolEntry {
                    kind: SymbolKind::Call { id: expr.id },
                    location: expr.location(),
                });
            }
            lume_hir::ExpressionKind::If(_) => {}
            lume_hir::ExpressionKind::Is(_) => {}
            lume_hir::ExpressionKind::Member(expr) => {
                self.symbols.insert(SymbolEntry {
                    location: expr.name.location,
                    kind: SymbolKind::Member {
                        callee: expr.callee,
                        field: expr.name.clone(),
                    },
                });
            }
            lume_hir::ExpressionKind::Scope(_) => {}
            lume_hir::ExpressionKind::Switch(_) => {}
            lume_hir::ExpressionKind::Variant(expr) => {
                self.symbols.insert(SymbolEntry {
                    location: expr.name.location(),
                    kind: SymbolKind::Variant {
                        name: expr.name.clone(),
                    },
                });
            }
            lume_hir::ExpressionKind::Variable(expr) => {
                self.symbols.insert(SymbolEntry {
                    kind: SymbolKind::VariableReference { id: expr.id },
                    location: expr.location,
                });
            }
            lume_hir::ExpressionKind::Literal(expr) => {
                self.symbols.insert(SymbolEntry {
                    kind: SymbolKind::Literal { id: expr.id },
                    location: expr.location,
                });
            }
        }

        Ok(())
    }

    fn visit_path(&mut self, path: &lume_hir::Path) -> Result<()> {
        let mut current = Some(path.clone());

        while let Some(parent) = current {
            if let lume_hir::PathSegment::Type { location, .. } = &parent.name {
                self.symbols.insert(SymbolEntry {
                    kind: SymbolKind::Type { name: parent.clone() },
                    location: *location,
                });
            }

            current = parent.parent();
        }

        Ok(())
    }

    fn visit_pattern(&mut self, pattern: &lume_hir::Pattern) -> Result<()> {
        match &pattern.kind {
            lume_hir::PatternKind::Variant(expr) => {
                self.symbols.insert(SymbolEntry {
                    location: expr.name.location(),
                    kind: SymbolKind::Variant {
                        name: expr.name.clone(),
                    },
                });
            }
            lume_hir::PatternKind::Identifier(_)
            | lume_hir::PatternKind::Literal(_)
            | lume_hir::PatternKind::Wildcard(_) => {
                self.symbols.insert(SymbolEntry {
                    location: pattern.location,
                    kind: SymbolKind::Pattern { id: pattern.id },
                });
            }
        }

        Ok(())
    }
}
