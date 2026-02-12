use super::symbol::Symbol;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ScopeContext {
    Root, Conditional, Function
}

#[derive(Debug, Clone, PartialEq)]
pub struct Scope {
    pub symbols: Vec<Symbol>,
    pub ctx: ScopeContext
}

impl Scope {
    pub fn new(ctx: ScopeContext) -> Self {
        Self { symbols: Vec::new(), ctx }
    }

    pub fn insert(&mut self, symbol: Symbol) {
        if !self.symbols.contains(&symbol) {
            self.symbols.push(symbol);
        }
    }
}