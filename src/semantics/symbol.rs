use super::ty::Type;

#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    name: String,
    ty: Type,
    mutability: bool,
}