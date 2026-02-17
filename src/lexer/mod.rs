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
            '.' => tokens.push(Token {
                kind: TokenKind::Dot,
                lexeme: ch.to_string(),
                span: Span { start: pos, end: pos + 1 },
            }),
            '=' => if let Some(&(end, '=')) = chars.last() {
                chars.pop();
                tokens.push(Token {
                    kind: TokenKind::Operator(Operator::Eq),
                    lexeme: "==".to_string(),
                    span: Span { start: pos, end: end + 1 },
                });
            } else {
                tokens.push(Token {
                    kind: TokenKind::Assign,
                    lexeme: ch.to_string(),
                    span: Span { start: pos, end: pos + 1 },
                });
            },
            '>' => if let Some(&(end, '=')) = chars.last() {
                chars.pop();
                tokens.push(Token {
                    kind: TokenKind::Operator(Operator::Ge),
                    lexeme: ">=".to_string(),
                    span: Span { start: pos, end: end + 1 },
                });
            } else {
                tokens.push(Token {
                    kind: TokenKind::Operator(Operator::Gt),
                    lexeme: ch.to_string(),
                    span: Span { start: pos, end: pos + 1 },
                });
            },
            '<' => if let Some(&(end, '=')) = chars.last() {
                chars.pop();
                tokens.push(Token {
                    kind: TokenKind::Operator(Operator::Le),
                    lexeme: "<=".to_string(),
                    span: Span { start: pos, end: end + 1 },
                });
            } else {
                tokens.push(Token {
                    kind: TokenKind::Operator(Operator::Lt),
                    lexeme: ch.to_string(),
                    span: Span { start: pos, end: pos + 1 },
                });
            },
            '!' => if let Some(&(end, '=')) = chars.last() {
                chars.pop();
                tokens.push(Token {
                    kind: TokenKind::Operator(Operator::Ne),
                    lexeme: "!=".to_string(),
                    span: Span { start: pos, end: end + 1 },
                });
            } else {
                tokens.push(Token {
                    kind: TokenKind::Operator(Operator::Bang),
                    lexeme: ch.to_string(),
                    span: Span { start: pos, end: pos + 1 },
                });
            },
            '(' => if let Some(&(pos, ':')) = chars.last() {
                chars.pop();
                let mut terminated = false;
                while let Some((_, ch)) = chars.pop() {
                    if ch == ':' {
                        if let Some((_, ')')) = chars.pop() {
                            terminated = true;
                            break;
                        }
                    }
                }

                if !terminated {
                    return Err(Diagnostic {
                        path: path.to_string(),
                        primary_err: "unterminated block comment".to_string(),
                        primary_span: Span { start: pos, end: chars.len() },
                        secondary_messages: Vec::new(),
                    });
                }
            } else {
                tokens.push(Token {
                    kind: TokenKind::LParen,
                    lexeme: ch.to_string(),
                    span: Span { start: pos, end: pos + 1 },
                });
            },
            ')' => tokens.push(Token {
                kind: TokenKind::RParen,
                lexeme: ch.to_string(),
                span: Span { start: pos, end: pos + 1 },
            }),
            '[' => tokens.push(Token {
                kind: TokenKind::LBracket,
                lexeme: ch.to_string(),
                span: Span { start: pos, end: pos + 1 },
            }),
            ']' => tokens.push(Token {
                kind: TokenKind::RBracket,
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
            '/' => if let Some((_, ':')) = chars.last() {
                while let Some((_, ch)) = chars.pop() {
                    if ch == '\n' {
                        break
                    }
                }
            } else {
                tokens.push(Token {
                    kind: TokenKind::Operator(Operator::Slash),
                    lexeme: ch.to_string(),
                    span: Span { start: pos, end: pos + 1 },
                })
            },
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

                            if chars.len() == 1 {
                                chars.pop();
                                
                                tokens.push(Token {
                                    kind: match t {
                                        'i' => {
                                            let result  = acc.parse().map_err(|_| Diagnostic {
                                                path: path.to_string(),
                                                primary_err: format!("literal `{acc}` doesn't fit in `int`"),
                                                primary_span: Span { start: pos, end },
                                                secondary_messages: Vec::new(),
                                            })?;
                                            TokenKind::Integer(result)
                                        },
                                        'u' => {
                                            let result  = acc.parse().map_err(|_| Diagnostic {
                                                path: path.to_string(),
                                                primary_err: format!("literal `{acc}` doesn't fit in `uint`"),
                                                primary_span: Span { start: pos, end },
                                                secondary_messages: Vec::new(),
                                            })?;
                                            TokenKind::UnsignedInt(result)
                                        },
                                        _ => unreachable!(),
                                    },
                                    lexeme: format!("{}{t}", acc),
                                    span: Span { start: pos, end: end + 1 },
                                });
                                continue 'lex_loop;
                            }

                            if chars.len() >= 2 {
                                if !chars[chars.len()-2].1.is_alphanumeric() {
                                    chars.pop();
                                    
                                    tokens.push(Token {
                                        kind: match t {
                                            'i' => {
                                                let result  = acc.parse().map_err(|_| Diagnostic {
                                                    path: path.to_string(),
                                                    primary_err: format!("literal `{acc}` doesn't fit in `int`"),
                                                    primary_span: Span { start: pos, end },
                                                    secondary_messages: Vec::new(),
                                                })?;
                                                TokenKind::Integer(result)
                                            },
                                            'u' => {
                                                let result  = acc.parse().map_err(|_| Diagnostic {
                                                    path: path.to_string(),
                                                    primary_err: format!("literal `{acc}` doesn't fit in `uint`"),
                                                    primary_span: Span { start: pos, end },
                                                    secondary_messages: Vec::new(),
                                                })?;
                                                TokenKind::UnsignedInt(result)
                                            },
                                            _ => unreachable!(),
                                        },
                                        lexeme: format!("{}{t}", acc),
                                        span: Span { start: pos, end: end + 1 },
                                    });
                                    continue 'lex_loop;
                                }

                                let l = match chars.len().checked_sub(3) {
                                    Some(n) => chars.get(n).map(|x| !x.1.is_alphanumeric()).unwrap(),
                                    None => true,
                                };
                                if chars[chars.len()-2].1 == '8' && l {
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
                                            'i' => {
                                                let result  = acc.parse().map_err(|_| Diagnostic {
                                                    path: path.to_string(),
                                                    primary_err: format!("literal `{acc}` doesn't fit in `i8`"),
                                                    primary_span: Span { start: pos, end },
                                                    secondary_messages: Vec::new(),
                                                })?;
                                                TokenKind::I8(result)
                                            },
                                            'u' => {
                                                let result  = acc.parse().map_err(|_| Diagnostic {
                                                    path: path.to_string(),
                                                    primary_err: format!("literal `{acc}` doesn't fit in `u8`"),
                                                    primary_span: Span { start: pos, end },
                                                    secondary_messages: Vec::new(),
                                                })?;
                                                TokenKind::U8(result)
                                            },
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

                                if chars[chars.len()-2].1 == '1' && chars[chars.len()-3].1 == '6' {
                                    chars.pop();
                                    chars.pop();
                                    chars.pop();
                                    tokens.push(Token {
                                        kind: match t {
                                            'i' => {
                                                let result  = acc.parse().map_err(|_| Diagnostic {
                                                    path: path.to_string(),
                                                    primary_err: format!("literal `{acc}` doesn't fit in `i16`"),
                                                    primary_span: Span { start: pos, end },
                                                    secondary_messages: Vec::new(),
                                                })?;
                                                TokenKind::I16(result)
                                            },
                                            'u' => {
                                                let result  = acc.parse().map_err(|_| Diagnostic {
                                                    path: path.to_string(),
                                                    primary_err: format!("literal `{acc}` doesn't fit in `u16`"),
                                                    primary_span: Span { start: pos, end },
                                                    secondary_messages: Vec::new(),
                                                })?;
                                                TokenKind::U16(result)
                                            },
                                            _ => unreachable!(),
                                        },
                                        lexeme: format!("{}{t}16", acc),
                                        span: Span { start: pos, end },
                                    });
                                    continue 'lex_loop;
                                } else if chars[chars.len()-2].1 == '3' && chars[chars.len()-3].1 == '2' {
                                    chars.pop();
                                    chars.pop();
                                    chars.pop();
                                    tokens.push(Token {
                                        kind: match t {
                                            'i' => {
                                                let result  = acc.parse().map_err(|_| Diagnostic {
                                                    path: path.to_string(),
                                                    primary_err: format!("literal `{acc}` doesn't fit in `i32`"),
                                                    primary_span: Span { start: pos, end },
                                                    secondary_messages: Vec::new(),
                                                })?;
                                                TokenKind::I32(result)
                                            },
                                            'u' => {
                                                let result  = acc.parse().map_err(|_| Diagnostic {
                                                    path: path.to_string(),
                                                    primary_err: format!("literal `{acc}` doesn't fit in `u32`"),
                                                    primary_span: Span { start: pos, end },
                                                    secondary_messages: Vec::new(),
                                                })?;
                                                TokenKind::U32(result)
                                            },
                                            _ => unreachable!(),
                                        },
                                        lexeme: format!("{}{t}32", acc),
                                        span: Span { start: pos, end },
                                    });
                                    continue 'lex_loop;
                                } else if chars[chars.len()-2].1 == '6' && chars[chars.len()-3].1 == '4' {
                                    chars.pop();
                                    chars.pop();
                                    chars.pop();
                                    tokens.push(Token {
                                        kind: match t {
                                            'i' => {
                                                let result  = acc.parse().map_err(|_| Diagnostic {
                                                    path: path.to_string(),
                                                    primary_err: format!("literal `{acc}` doesn't fit in `i64`"),
                                                    primary_span: Span { start: pos, end },
                                                    secondary_messages: Vec::new(),
                                                })?;
                                                TokenKind::I64(result)
                                            },
                                            'u' => {
                                                let result  = acc.parse().map_err(|_| Diagnostic {
                                                    path: path.to_string(),
                                                    primary_err: format!("literal `{acc}` doesn't fit in `u64`"),
                                                    primary_span: Span { start: pos, end },
                                                    secondary_messages: Vec::new(),
                                                })?;
                                                TokenKind::U64(result)
                                            },
                                            _ => unreachable!(),
                                        },
                                        lexeme: format!("{}{t}64", acc),
                                        span: Span { start: pos, end },
                                    });
                                    continue 'lex_loop;
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
                    let result = acc.parse().map_err(|_| Diagnostic {
                        path: path.to_string(),
                        primary_err: format!("literal `{acc}` doesn't fit in `int`"),
                        primary_span: Span { start: pos, end },
                        secondary_messages: Vec::new(),
                    })?;
                    tokens.push(Token {
                        kind: TokenKind::Integer(result),
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
                    "if" => tokens.push(Token {
                        kind: TokenKind::If,
                        lexeme: acc,
                        span: Span { start: pos, end },
                    }),
                    "else" => tokens.push(Token {
                        kind: TokenKind::Else,
                        lexeme: acc,
                        span: Span { start: pos, end },
                    }),
                    "then" => tokens.push(Token {
                        kind: TokenKind::Then,
                        lexeme: acc,
                        span: Span { start: pos, end },
                    }),
                    "end" => tokens.push(Token {
                        kind: TokenKind::End,
                        lexeme: acc,
                        span: Span { start: pos, end },
                    }),
                    "true" => tokens.push(Token {
                        kind: TokenKind::True,
                        lexeme: acc,
                        span: Span { start: pos, end },
                    }),
                    "false" => tokens.push(Token {
                        kind: TokenKind::False,
                        lexeme: acc,
                        span: Span { start: pos, end },
                    }),
                    "while" => tokens.push(Token {
                        kind: TokenKind::While,
                        lexeme: acc,
                        span: Span { start: pos, end },
                    }),
                    "do" => tokens.push(Token {
                        kind: TokenKind::Do,
                        lexeme: acc,
                        span: Span { start: pos, end },
                    }),
                    "def" => tokens.push(Token {
                        kind: TokenKind::Def,
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