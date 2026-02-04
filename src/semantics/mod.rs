pub mod ty;
pub mod symbol;

use crate::diag::Diagnostic;
use crate::parser::ast::{Node, NodeKind};
use ty::Type;

fn analyze(path: &str, ast: &mut [Node]) -> Result<(), Vec<Diagnostic>> {
    let mut analyzer = SemanticAnalyzer::new(path);
    analyzer.analyze(ast)
}

struct SemanticAnalyzer {
    path: String,
}

impl SemanticAnalyzer {
    fn new(path: &str) -> Self {
        Self {
            path: path.to_string(),
        }
    }

    fn analyze(&mut self, ast: &mut [Node]) -> Result<(), Vec<Diagnostic>> {
        let mut errors = Vec::new();

        for node in ast {
            if let Err(err) = self.analyze_node(node) {
                errors.extend(err);
            }
        }

        if !errors.is_empty() {
            return Err(errors)
        }

        Ok(())
    }

    fn analyze_node(&mut self, node: &mut Node) -> Result<Type, Vec<Diagnostic>> {
        match &mut node.kind {
            NodeKind::Integer(_) => Ok(Type::Int),
            NodeKind::Float(_) => Ok(Type::Float),
            _ => todo!()
        }
    }
}