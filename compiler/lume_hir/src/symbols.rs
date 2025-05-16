use std::collections::HashMap;

use crate::*;

/// Defines a single frame within the symbol table. Each frame can contain multiple entries.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct SymbolTableFrame {
    /// Defines all the symbols defined within the frame.
    entries: HashMap<String, VariableDeclaration>,
}

impl SymbolTableFrame {
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
        }
    }
}

/// Defines a single frame within the symbol table. Each frame can be either be symbols or a boundary.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum SymbolTableEntry {
    /// Frames indicate a scope with zero-or-more symbols defined within it.
    /// The symbol table can jump frames to access symbols defined in parent scopes.
    Frame(SymbolTableFrame),

    /// Boundaries are only meant to be implemented when all symbols in the local scope
    /// should be hidden. This is most often the case when calling a function or method,
    /// where the local scope is hidden.
    ///
    /// The symbol table cannot jump boundaries to access parent scopes.
    Boundary,
}

impl SymbolTableEntry {
    pub fn symbol() -> Self {
        Self::Frame(SymbolTableFrame::new())
    }

    pub fn boundary() -> Self {
        Self::Boundary
    }
}

/// The symbol table keeps track of all symbols which are available within any given scope.
///
/// This table binds variable references to their declarations, so code such as this:
///
/// ```lm
/// let a = 1;
/// let b = a;
/// ```
///
/// The expression `let a = 1` declares a new variable `a` and assigns it the value `1`. When the
/// expression `let b = a` is visited, it needs to know which variable is being referenced, when seeing `a`.
///
/// Since this table needs to contain a lookup table of variables, it also needs to know which scope each
/// variable is declared in. Because of this, this table also inherently handles invalid variable references,
/// such as referencing a variable that has not been declared or variables which are out of scope. For example:
///
/// ```lm
/// let a = b;
/// ```
///
/// Would fail because `b` has not been declared yet. Similarly:
///
/// ```lm
/// fn test()
///   b = 1;
/// end
///
/// test();
///
/// let a = b;
/// ```
///
/// Would fail because `b` is out of scope when `a` is declared, even though `b` was defined within `test`,
/// which was called before `a` was declared.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct SymbolTable {
    /// Defines all the entries within the table, ordered by the declaration order.
    symbols: Vec<SymbolTableEntry>,
}

impl SymbolTable {
    /// Creates a new symbol table, without any content.
    pub fn new() -> Self {
        let mut table = Self { symbols: Vec::new() };

        // The first frame functions as a global scope, so it should always be present.
        table.push_frame();

        table
    }

    /// Creates a new symbol table, without any content, without any global scope.
    #[allow(dead_code)]
    pub fn new_without_global() -> Self {
        let mut table = Self::new();

        // Push a boundary, so global scope cannot be accessed from within the table.
        table.push_boundary();

        table
    }

    /// Appends a new frame to the current symbol scope.
    ///
    /// This is usually called when a function is invoked or block scope starts.
    pub(crate) fn push_frame(&mut self) {
        self.symbols.push(SymbolTableEntry::symbol());
    }

    /// Pops the current symbol scope from the symbol table.
    ///
    /// This is usually called when a function or block scope ends.
    pub(crate) fn pop_frame(&mut self) {
        // The last frame is the global scope and cannot be popped.
        if self.symbols.len() == 1 {
            return;
        }

        self.symbols.pop();
    }

    /// Appends a new named symbol to the current symbol scope.
    ///
    /// This is usually called when a new variable is introduced within an existing block scope.
    pub(crate) fn define(&mut self, decl: VariableDeclaration) {
        if let SymbolTableEntry::Frame(frame) = self.symbols.last_mut().unwrap() {
            frame.entries.insert(decl.name.name.clone(), decl);
        }
    }

    /// Retrieves a named symbol from the current symbol scope. If not found, it will search the global scope.
    ///
    /// This is usually called when a variable is referenced within an existing block scope.
    /// To retrieve the symbol, the table will iterate, in reverse order, up until a symbol with the same
    /// name is found, inside of the current scope. If the iterator reaches a boundary, it will stop searching
    /// for local symbols and continue searching in the global scope.
    pub(crate) fn retrieve(&self, name: &str) -> Option<&VariableDeclaration> {
        if let Some(symbol) = self.retrieve_scoped(name) {
            return Some(symbol);
        }

        if let Some(symbol) = self.retrieve_global(name) {
            return Some(symbol);
        }

        None
    }

    /// Push a new boundary onto the symbol table, scoping it to only newly added symbols.
    pub(crate) fn push_boundary(&mut self) {
        self.symbols.push(SymbolTableEntry::boundary());

        // Push a new frame onto the stack, so new symbols can be registered.
        self.push_frame();
    }

    /// Pops a boundary off the symbol table, removing all symbols added since the last boundary.
    pub(crate) fn pop_boundary(&mut self) {
        // Pops the symbol frame off the stack.
        self.pop_frame();

        // Pops the boundary off the stack.
        self.pop_frame();
    }

    /// Attempts to retrieve a symbol of the given name from the current scope.
    fn retrieve_scoped(&self, name: &str) -> Option<&VariableDeclaration> {
        for entry in self.symbols.iter().rev() {
            match entry {
                SymbolTableEntry::Frame(f) => {
                    if let Some(v) = f.entries.get(name) {
                        return Some(v);
                    }
                }
                SymbolTableEntry::Boundary => break,
            }
        }

        None
    }

    /// Attempts to retrieve a symbol of the given name from the global scope.
    fn retrieve_global(&self, name: &str) -> Option<&VariableDeclaration> {
        if let Some(SymbolTableEntry::Frame(f)) = self.symbols.first() {
            if let Some(v) = f.entries.get(name) {
                return Some(v);
            }
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn var(name: &str) -> VariableDeclaration {
        let statement_id = StatementId::empty();

        VariableDeclaration {
            id: statement_id,
            name: Identifier {
                name: name.to_string(),
                location: Location::empty(),
            },
            declared_type: None,
            value: Expression {
                id: ExpressionId::empty(),
                kind: ExpressionKind::Literal(Box::new(Literal {
                    id: ExpressionId::empty(),
                    kind: LiteralKind::Int(Box::new(IntLiteral {
                        id: ExpressionId::empty(),
                        value: 0,
                        kind: IntKind::I32,
                    })),
                    location: Location::empty(),
                })),
                location: Location::empty(),
            },
        }
    }

    #[test]
    fn test_empty_symbol_table() {
        let table = SymbolTable::new();

        assert_eq!(table.retrieve("var"), None);
    }

    #[test]
    fn test_retrieve_symbol() {
        let mut table = SymbolTable::new();
        let var = var("var");

        table.define(var.clone());

        assert_eq!(table.retrieve("var"), Some(&var));
    }

    #[test]
    fn test_retrieve_diff_symbol() {
        let mut table = SymbolTable::new();
        let var1 = var("var1");
        let var2 = var("var2");

        table.define(var1.clone());
        table.define(var2.clone());

        assert_eq!(table.retrieve("var1"), Some(&var1));
        assert_eq!(table.retrieve("var2"), Some(&var2));
    }

    #[test]
    fn test_retrieve_bounded_symbol() {
        let mut table = SymbolTable::new_without_global();

        let var = var("var");
        table.define(var.clone());
        table.push_boundary();

        assert_eq!(table.retrieve("var"), None);
    }

    #[test]
    fn test_retrieve_unbounded_symbol() {
        let mut table = SymbolTable::new();

        let var1 = var("var1");
        table.define(var1.clone());

        table.push_boundary();

        let var2 = var("var2");
        table.define(var2.clone());

        assert_eq!(table.retrieve("var2"), Some(&var2));
    }

    #[test]
    fn test_dropped_boundary_drops_vars() {
        let mut table = SymbolTable::new_without_global();

        let var1 = var("var1");
        table.define(var1.clone());

        let var2 = var("var2");
        table.define(var2.clone());

        table.push_boundary();

        let var3 = var("var3");
        table.define(var3.clone());

        let var4 = var("var4");
        table.define(var4.clone());

        assert_eq!(table.retrieve("var1"), None);
        assert_eq!(table.retrieve("var2"), None);
        assert_eq!(table.retrieve("var3"), Some(&var3));
        assert_eq!(table.retrieve("var4"), Some(&var4));

        table.pop_boundary();

        assert_eq!(table.retrieve("var1"), Some(&var1));
        assert_eq!(table.retrieve("var2"), Some(&var2));
        assert_eq!(table.retrieve("var3"), None);
        assert_eq!(table.retrieve("var4"), None);
    }

    #[test]
    fn test_retrieve_popped_boundary_symbol() {
        let mut table = SymbolTable::new_without_global();

        let var1 = var("var1");
        table.define(var1.clone());

        table.push_boundary();

        let var2 = var("var2");
        table.define(var2.clone());

        assert_eq!(table.retrieve("var1"), None);

        table.pop_boundary();

        assert_eq!(table.retrieve("var1"), Some(&var1));
    }

    #[test]
    fn test_retrieve_global_symbol() {
        let mut table = SymbolTable::new();

        let global = var("global");
        table.define(global.clone());

        table.push_boundary();

        let non_global = var("non_global");
        table.define(non_global.clone());

        table.push_boundary();
        table.push_boundary();
        table.push_boundary();

        let var = var("var");
        table.define(var.clone());

        assert_eq!(table.retrieve("global"), Some(&global));
        assert_eq!(table.retrieve("non_global"), None);
        assert_eq!(table.retrieve("var"), Some(&var));
    }
}
