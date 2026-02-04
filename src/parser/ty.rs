use crate::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum ParseTypeKind {
    Identifier(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParseType {
    pub kind: ParseTypeKind,
    pub span: Span,
}