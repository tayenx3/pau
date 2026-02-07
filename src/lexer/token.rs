use std::fmt;

use crate::operator::Operator;
use crate::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Integer(i64), Float(f64), Identifier(String),
    I8(i8), I16(i16), I32(i32), I64(i64),
    U8(u8), U16(u16), U32(u32), U64(u64),
    F32(f32), F64(f64),
    Operator(Operator),
    Let, Var,
    LParen, RParen, Assign,
    Colon, Comma, Semicolon,
}

#[derive(Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub span: Span,
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.kind)
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "`{}`", self.lexeme)
    }
}