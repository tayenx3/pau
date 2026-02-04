pub mod token;

use token::{Token, TokenKind};
use crate::operator::Operator;
use crate::diag::Diagnostic;
use crate::span::Span;

pub fn tokenize(path: &str, source: &str) -> Result<Vec<Token>, Diagnostic> {
    let mut tokens = Vec::new();
    let mut chars = source.char_indices().rev().collect::<Vec<_>>();

    while let Some((pos, ch)) = chars.pop() {
        match ch {
            ' ' | '\t' | '\r' | '\n' => continue,
            ':' => if let Some(&(end, '=')) = chars.last() {
                chars.pop();
                tokens.push(Token {
                    kind: TokenKind::Operator(Operator::Walrus),
                    lexeme: ":=".to_string(),
                    span: Span { start: pos, end: end + 1 },
                });
            } else {
                tokens.push(Token {
                    kind: TokenKind::Colon,
                    lexeme: ch.to_string(),
                    span: Span { start: pos, end: pos + 1 },
                });
            },
            ',' => tokens.push(Token {
                kind: TokenKind::Comma,
                lexeme: ch.to_string(),
                span: Span { start: pos, end: pos + 1 },
            }),
            ';' => tokens.push(Token {
                kind: TokenKind::Semicolon,
                lexeme: ch.to_string(),
                span: Span { start: pos, end: pos + 1 },
            }),
            '=' => tokens.push(Token {
                kind: TokenKind::Assign,
                lexeme: ch.to_string(),
                span: Span { start: pos, end: pos + 1 },
            }),
            '(' => tokens.push(Token {
                kind: TokenKind::LParen,
                lexeme: ch.to_string(),
                span: Span { start: pos, end: pos + 1 },
            }),
            ')' => tokens.push(Token {
                kind: TokenKind::RParen,
                lexeme: ch.to_string(),
                span: Span { start: pos, end: pos + 1 },
            }),
            '+' => tokens.push(Token {
                kind: TokenKind::Operator(Operator::Plus),
                lexeme: ch.to_string(),
                span: Span { start: pos, end: pos + 1 },
            }),
            '-' => tokens.push(Token {
                kind: TokenKind::Operator(Operator::Minus),
                lexeme: ch.to_string(),
                span: Span { start: pos, end: pos + 1 },
            }),
            '*' => tokens.push(Token {
                kind: TokenKind::Operator(Operator::Star),
                lexeme: ch.to_string(),
                span: Span { start: pos, end: pos + 1 },
            }),
            '/' => tokens.push(Token {
                kind: TokenKind::Operator(Operator::Slash),
                lexeme: ch.to_string(),
                span: Span { start: pos, end: pos + 1 },
            }),
            '%' => tokens.push(Token {
                kind: TokenKind::Operator(Operator::Modulo),
                lexeme: ch.to_string(),
                span: Span { start: pos, end: pos + 1 },
            }),
            '0'..='9' => {
                let mut acc = ch.to_string();
                let mut is_float = false;
                let mut end = pos + 1;

                while let Some(&(pos, ch)) = chars.last() {
                    match ch {
                        '0'..='9' => acc.push(ch),
                        '_' => (),
                        '.' => if is_float {
                            end = pos;
                            break
                        } else {
                            acc.push(ch);
                            is_float = true;
                        },
                        _ => {
                            end = pos;
                            break
                        },
                    }
                    chars.pop();
                }

                if is_float {
                    tokens.push(Token {
                        kind: TokenKind::Float(acc.parse().unwrap()),
                        lexeme: acc,
                        span: Span { start: pos, end },
                    });
                } else {
                    tokens.push(Token {
                        kind: TokenKind::Integer(acc.parse().unwrap()),
                        lexeme: acc,
                        span: Span { start: pos, end },
                    });
                }
            },
            'a'..='z' | 'A'..='Z' | '_' => {
                let mut acc = ch.to_string();
                let mut end = pos + 1;

                while let Some(&(pos, ch)) = chars.last() {
                    match ch {
                        'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => acc.push(ch),
                        _ => {
                            end = pos;
                            break
                        },
                    }
                    chars.pop();
                }

                match &*acc {
                    "let" => tokens.push(Token {
                        kind: TokenKind::Let,
                        lexeme: acc,
                        span: Span { start: pos, end },
                    }),
                    "var" => tokens.push(Token {
                        kind: TokenKind::Var,
                        lexeme: acc,
                        span: Span { start: pos, end },
                    }),
                    _ => tokens.push(Token {
                        kind: TokenKind::Identifier(acc.clone()),
                        lexeme: acc,
                        span: Span { start: pos, end },
                    }),
                }
            },
            _ => return Err(Diagnostic {
                path: path.to_string(),
                primary_err: format!("Unrecognized character: `{ch}`"),
                primary_span: Span { start: pos, end: pos + 1 },
                secondary_messages: Vec::new(),
            }),
        }
    }

    Ok(tokens)
}