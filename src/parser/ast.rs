use super::ty::ParseType;
use crate::operator::Operator;
use crate::semantics::ty::Type;
use crate::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum NodeKind {
    Integer(isize), UnsignedInt(usize), Float(f64), Identifier(String),
    Boolean(bool),
    I8(i8), I16(i16), I32(i32), I64(i64),
    U8(u8), U16(u16), U32(u32), U64(u64),
    F32(f32), F64(f64),
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
    IfCondition {
        condition: Box<Node>,
        then_body: Vec<Node>,
        else_body: Option<Vec<Node>>,
        ty_cache: Option<Type>,
    },
    WhileLoop {
        condition: Box<Node>,
        body: Vec<Node>,
    },
    FunctionDef {
        name: String,
        params: Vec<Param>,
        return_ty: Option<ParseType>,
        body: Vec<Node>,
        ty_cache: Option<Type>,
    },
    FunctionCall {
        callee: String,
        args: Vec<Node>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub name: String,
    pub ty: ParseType,
    pub span: Span,
    pub ty_cache: Option<Type>
}

#[derive(Debug, Clone, PartialEq)]
pub struct Node {
    pub kind: NodeKind,
    pub span: Span,
}