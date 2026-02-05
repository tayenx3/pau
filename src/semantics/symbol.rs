use crate::span::Span;
use super::ty::Type;

#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    pub name: String,
    pub ty: Type,
    pub mutability: bool,
    pub defined_at: Span,
}