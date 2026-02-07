//! # Lexical Analysis/Lexer

pub mod token;

use token::{Token, TokenKind};
use crate::operator::Operator;
use crate::diag::Diagnostic;
use crate::span::Span;

pub fn tokenize(path: &str, source: &str) -> Result<Vec<Token>, Diagnostic> {
    let mut tokens = Vec::new();
    let mut chars = source.char_indices().rev().collect::<Vec<_>>();

    'lex_loop : while let Some((pos, ch)) = chars.pop() {
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
                        'i' | 'u' => if is_float {
                            end = pos;
                            break
                        } else {
                            let t = ch;

                            end = pos;
                            if chars.len() >= 2 {
                                let l = match chars.len().checked_sub(3) {
                                    Some(n) => chars.get(n).map(|x| !x.1.is_alphanumeric()).unwrap(),
                                    None => true,
                                };
                                if Some('8') == chars.get(chars.len()-2).map(|x| x.1)
                                    && l
                                {
                                    chars.pop();
                                    chars.pop();
                                    if acc.parse::<i64>().unwrap() > (t == 'i').then(|| i8::MAX as i64).unwrap_or(u8::MAX as i64) {
                                        return Err(Diagnostic {
                                            path: path.to_string(),
                                            primary_err: format!("literal `{acc}` doesn't fit in i8"),
                                            primary_span: Span { start: pos, end },
                                            secondary_messages: Vec::new(),
                                        });
                                    }
                                    tokens.push(Token {
                                        kind: match t {
                                            'i' => TokenKind::I8(acc.parse().unwrap()),
                                            'u' => TokenKind::U8(acc.parse().unwrap()),
                                            _ => unreachable!(),
                                        },
                                        lexeme: format!("{}{t}8", acc),
                                        span: Span { start: pos, end: end + 2 },
                                    });
                                    continue 'lex_loop;
                                }
                            }
                            
                            if chars.len() >= 3 {
                                let l = match chars.len().checked_sub(4) {
                                    Some(n) => chars.get(n).map(|x| !x.1.is_alphanumeric()).unwrap(),
                                    None => true,
                                };
                                if !l {
                                    break
                                }

                                if Some('1') == chars.get(chars.len()-2).map(|x| x.1)
                                    && Some('6') == chars.get(chars.len()-3).map(|x| x.1)
                                {
                                    chars.pop();
                                    chars.pop();
                                    chars.pop();
                                    if acc.parse::<i64>().unwrap() > i16::MAX as i64 {
                                        return Err(Diagnostic {
                                            path: path.to_string(),
                                            primary_err: format!("literal `{acc}` doesn't fit in i16"),
                                            primary_span: Span { start: pos, end },
                                            secondary_messages: Vec::new(),
                                        });
                                    }
                                    tokens.push(Token {
                                        kind: match t {
                                            'i' => TokenKind::I16(acc.parse().unwrap()),
                                            'u' => TokenKind::U16(acc.parse().unwrap()),
                                            _ => unreachable!(),
                                        },
                                        lexeme: format!("{}{t}16", acc),
                                        span: Span { start: pos, end },
                                    });
                                    continue 'lex_loop;
                                } else if Some('3') == chars.get(chars.len()-2).map(|x| x.1)
                                    && Some('2') == chars.get(chars.len()-3).map(|x| x.1)
                                {
                                    chars.pop();
                                    chars.pop();
                                    chars.pop();
                                    if acc.parse::<i64>().unwrap() > i32::MAX as i64 {
                                        return Err(Diagnostic {
                                            path: path.to_string(),
                                            primary_err: format!("literal `{acc}` doesn't fit in i32"),
                                            primary_span: Span { start: pos, end },
                                            secondary_messages: Vec::new(),
                                        });
                                    }
                                    tokens.push(Token {
                                        kind: match t {
                                            'i' => TokenKind::I32(acc.parse().unwrap()),
                                            'u' => TokenKind::U32(acc.parse().unwrap()),
                                            _ => unreachable!(),
                                        },
                                        lexeme: format!("{}{t}32", acc),
                                        span: Span { start: pos, end },
                                    });
                                    continue 'lex_loop;
                                } else if Some('6') == chars.get(chars.len()-2).map(|x| x.1)
                                    && Some('4') == chars.get(chars.len()-3).map(|x| x.1)
                                {
                                    chars.pop();
                                    chars.pop();
                                    chars.pop();
                                    tokens.push(Token {
                                        kind: match t {
                                            'i' => TokenKind::I64(acc.parse().unwrap()),
                                            'u' => TokenKind::U64(acc.parse().unwrap()),
                                            _ => unreachable!(),
                                        },
                                        lexeme: format!("{}{t}64", acc),
                                        span: Span { start: pos, end },
                                    });
                                    continue 'lex_loop;
                                } else {
                                    break
                                }
                            }
                        },
                        'f' => if !is_float {
                            end = pos;
                            break
                        } else {
                            end = pos;
                            if chars.len() > 3 {
                                if Some('3') == chars.get(chars.len()-2).map(|x| x.1)
                                    && Some('2') == chars.get(chars.len()-3).map(|x| x.1)
                                {
                                    chars.pop();
                                    chars.pop();
                                    chars.pop();
                                    tokens.push(Token {
                                        kind: TokenKind::F32(acc.parse().unwrap()),
                                        lexeme: format!("{}f32", acc),
                                        span: Span { start: pos, end },
                                    });
                                    continue 'lex_loop;
                                } else if Some('6') == chars.get(chars.len()-2).map(|x| x.1)
                                    && Some('4') == chars.get(chars.len()-3).map(|x| x.1)
                                {
                                    chars.pop();
                                    chars.pop();
                                    chars.pop();
                                    tokens.push(Token {
                                        kind: TokenKind::F64(acc.parse().unwrap()),
                                        lexeme: format!("{}f64", acc),
                                        span: Span { start: pos, end },
                                    });
                                    continue 'lex_loop;
                                } else {
                                    break
                                }
                            } else {
                                break
                            }
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
                primary_err: format!("unrecognized character: `{ch}`"),
                primary_span: Span { start: pos, end: pos + 1 },
                secondary_messages: Vec::new(),
            }),
        }
    }

    Ok(tokens)
}