use crate::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum ParseTypeKind {
    Identifier(String),
    Array {
        inner: Box<ParseType>,
        size: Option<usize>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParseType {
    pub kind: ParseTypeKind,
    pub span: Span,
}