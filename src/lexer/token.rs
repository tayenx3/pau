use std::fmt;

use crate::operator::Operator;
use crate::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Integer(isize), UnsignedInt(usize), Float(f64), Identifier(String),
    I8(i8), I16(i16), I32(i32), I64(i64),
    U8(u8), U16(u16), U32(u32), U64(u64),
    F32(f32), F64(f64),
    Operator(Operator),
    Let, Var, If, Else, Then, End, While, Do, Def,
    True, False,
    LParen, RParen, LBracket, RBracket,
    Assign,
    Colon, Comma, Semicolon, Dot,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Integer(n) => write!(f, "{n}"),
            Self::UnsignedInt(n) => write!(f, "{n}"),
            Self::Float(n) => write!(f, "{n}"),
            Self::Identifier(n) => write!(f, "{n}"),
            Self::I8(n) => write!(f, "{n}"),
            Self::I16(n) => write!(f, "{n}"),
            Self::I32(n) => write!(f, "{n}"),
            Self::I64(n) => write!(f, "{n}"),
            Self::U8(n) => write!(f, "{n}"),
            Self::U16(n) => write!(f, "{n}"),
            Self::U32(n) => write!(f, "{n}"),
            Self::U64(n) => write!(f, "{n}"),
            Self::F32(n) => write!(f, "{n}"),
            Self::F64(n) => write!(f, "{n}"),
            Self::Operator(n) => write!(f, "{n}"),
            Self::Let => write!(f, "let"),
            Self::Var => write!(f, "var"),
            Self::If => write!(f, "if"),
            Self::Else => write!(f, "else"),
            Self::Then => write!(f, "then"),
            Self::End => write!(f, "end"),
            Self::While => write!(f, "while"),
            Self::Do => write!(f, "do"),
            Self::Def => write!(f, "def"),
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::LParen => write!(f, "("),
            Self::RParen => write!(f, ")"),
            Self::LBracket => write!(f, "["),
            Self::RBracket => write!(f, "]"),
            Self::Assign => write!(f, "="),
            Self::Colon => write!(f, ":"),
            Self::Comma => write!(f, ","),
            Self::Semicolon => write!(f, ";"),
            Self::Dot => write!(f, "."),
        }
    }
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
        write!(f, "`{}`", self.kind)
    }
}