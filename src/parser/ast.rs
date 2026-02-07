use super::ty::ParseType;
use crate::operator::Operator;
use crate::semantics::ty::Type;
use crate::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum NodeKind {
    Integer(i64), Float(f64), Identifier(String),
    Semi(Box<Node>),
    BinaryOp {
        op: Operator,
        lhs: Box<Node>,
        rhs: Box<Node>,
        ty_cache: Option<Type>,
    },
    UnaryOp {
        op: Operator,
        operand: Box<Node>,
        ty_cache: Option<Type>,
    },
    Declaration {
        name: String,
        ty: Option<ParseType>,
        resolved_ty: Option<Type>,
        init: Option<Box<Node>>,
        mutability: bool,
    },
}


#[derive(Debug, Clone, PartialEq)]
pub struct Node {
    pub kind: NodeKind,
    pub span: Span,
}