//! # Syntactic Analysis/Parser

pub mod ast;
pub mod ty;

use crate::lexer::token::{Token, TokenKind};
use crate::diag::Diagnostic;
use ast::{Node, NodeKind, Param};
use ty::{ParseType, ParseTypeKind};

pub fn parse(path: &str, tokens: &[Token]) -> Result<Vec<Node>, Diagnostic> {
    let mut parser = Parser::new(path, tokens);
    parser.parse()
}

struct Parser<'a> {
    path: String,
    tokens: &'a [Token],
    pos: usize,
}

impl<'a> Parser<'a> {
    fn new(path: &str, tokens: &'a [Token]) -> Self {
        Self {
            path: path.to_string(),
            tokens: tokens,
            pos: 0,
        }
    }

    fn expect(&mut self, expected: TokenKind) -> Result<&Token, Diagnostic> {
        match self.tokens.get(self.pos) {
            Some(tok) => if tok.kind == expected {
                self.pos += 1;
                Ok(tok)
            } else {
                Err(Diagnostic {
                    path: self.path.clone(),
                    primary_err: format!("expected `{expected}`, found {tok}"),
                    primary_span: tok.span,
                    secondary_messages: Vec::new(),
                })
            },
            None => return Err(Diagnostic {
                path: self.path.clone(),
                primary_err: format!("expected `{expected}`, found end of input"),
                primary_span: self.tokens.last().unwrap().span.splat_to_end(),
                secondary_messages: Vec::new(),
            }),
        }
    }

    fn expect_ident(&mut self) -> Result<&Token, Diagnostic> {
        match self.tokens.get(self.pos) {
            Some(tok) if matches!(tok.kind, TokenKind::Identifier(_)) => {
                self.pos += 1;
                Ok(tok)
            },
            Some(tok) => {
                Err(Diagnostic {
                    path: self.path.clone(),
                    primary_err: format!("expected identifier, found {tok}"),
                    primary_span: tok.span,
                    secondary_messages: Vec::new(),
                })
            },
            None => return Err(Diagnostic {
                path: self.path.clone(),
                primary_err: format!("expected identifier, found end of input"),
                primary_span: self.tokens.last().unwrap().span.splat_to_end(),
                secondary_messages: Vec::new(),
            }),
        }
    }
    
    fn expect_int(&mut self) -> Result<&Token, Diagnostic> {
        match self.tokens.get(self.pos) {
            Some(tok) if matches!(tok.kind, TokenKind::Integer(_)) => {
                self.pos += 1;
                Ok(tok)
            },
            Some(tok) => {
                Err(Diagnostic {
                    path: self.path.clone(),
                    primary_err: format!("expected uint, found {tok}"),
                    primary_span: tok.span,
                    secondary_messages: Vec::new(),
                })
            },
            None => return Err(Diagnostic {
                path: self.path.clone(),
                primary_err: format!("expected uint, found end of input"),
                primary_span: self.tokens.last().unwrap().span.splat_to_end(),
                secondary_messages: Vec::new(),
            }),
        }
    }

    fn parse(&mut self) -> Result<Vec<Node>, Diagnostic> {
        let mut stmts = Vec::new();

        while self.tokens.get(self.pos).is_some() {
            stmts.push(self.parse_root_item()?);
        }

        Ok(stmts)
    }

    fn parse_root_item(&mut self) -> Result<Node, Diagnostic> {
        let tok = match self.tokens.get(self.pos) {
            Some(tok) => tok,
            None => return Err(Diagnostic {
                path: self.path.clone(),
                primary_err: "expected item, found end of input".to_string(),
                primary_span: self.tokens.last().unwrap().span.splat_to_end(),
                secondary_messages: Vec::new(),
            }),
        };

        match &tok.kind {
            TokenKind::Def => self.parse_def(),
            _ => Err(Diagnostic {
                path: self.path.clone(),
                primary_err: format!("expected item, found {}", tok),
                primary_span: tok.span,
                secondary_messages: Vec::new(),
            }),
        }
    }

    fn parse_def(&mut self) -> Result<Node, Diagnostic> {
        let mut stmt_span = self.tokens.get(self.pos).unwrap().span;
        self.pos += 1;
        
        let name = self.expect_ident()?.lexeme.clone();

        self.expect(TokenKind::LParen)?;

        let mut params = Vec::new();
        while let Some(tok) = self.tokens.get(self.pos) {
            if tok.kind == TokenKind::RParen {
                break
            }

            let ident = self.expect_ident()?;
            let mut param_span = ident.span;
            let param_name = ident.lexeme.clone();
            self.expect(TokenKind::Colon)?;
            let param_ty = self.parse_type()?;
            param_span.end = param_ty.span.end;

            params.push(Param {
                name: param_name,
                ty: param_ty,
                span: param_span,
                ty_cache: None,
            });

            if self.expect(TokenKind::Comma).is_err() {
                break
            }
        }

        self.expect(TokenKind::RParen)?;

        let return_ty = if self.expect(TokenKind::Colon).is_ok() {
            Some(self.parse_type()?)
        } else {
            None
        };

        let mut body = Vec::new();
        while self.tokens.get(self.pos).is_some() {
            if let Some(Token { kind: TokenKind::End, .. }) = self.tokens.get(self.pos) {
                break
            }
            body.push(self.parse_statement()?);
        }
        stmt_span.end = self.expect(TokenKind::End)?.span.end;

        Ok(Node {
            kind: NodeKind::FunctionDef { name, params, return_ty, body, ty_cache: None, errored: false },
            span: stmt_span,
            ty: None,
        })
    }

    fn parse_statement(&mut self) -> Result<Node, Diagnostic> {
        let mut node = match self.tokens.get(self.pos) {
            Some(Token { kind: TokenKind::While, .. }) => self.parse_while()?,
            _ => self.parse_expression(0)?,
        };

        match self.tokens.get(self.pos) {
            Some(Token { kind: TokenKind::Semicolon, span, .. }) => {
                let span = node.span.connect(span);
                node = Node {
                    kind: NodeKind::Semi(Box::new(node)),
                    span,
                    ty: None,
                };
                self.pos += 1;
            },
            _ => (),
        }

        Ok(node)
    }

    fn parse_expression(&mut self, mbp: usize) -> Result<Node, Diagnostic> {
        let mut lhs = self.parse_primary()?;

        while let Some(tok) = self.tokens.get(self.pos) {
            match tok {
                Token { kind: TokenKind::LBracket, .. } => {
                    self.pos += 1;
                    let index = Box::new(self.parse_expression(0)?);
                    let span = lhs.span.connect(&self.expect(TokenKind::RBracket)?.span);
                    lhs = Node {
                        kind: NodeKind::Index {
                            collection: Box::new(lhs),
                            index,
                        },
                        span,
                        ty: None,
                    };
                },
                Token { kind: TokenKind::Operator(op), .. } => {
                    let bp = if op.is_infix() {
                        let (lbp, rbp) = op.prec();
                        if lbp < mbp { break }

                        rbp
                    } else { break };
                    self.pos += 1;

                    let rhs = self.parse_expression(bp)?;
                    let span = lhs.span.connect(&rhs.span);

                    lhs = Node {
                        kind: NodeKind::BinaryOp {
                            op: *op,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        span,
                        ty: None,
                    };
                },
                _ => break,
            }
        }

        Ok(lhs)
    }

    fn parse_primary(&mut self) -> Result<Node, Diagnostic> {
        let tok = match self.tokens.get(self.pos) {
            Some(tok) => tok,
            None => return Err(Diagnostic {
                path: self.path.clone(),
                primary_err: "expected expression, found end of input".to_string(),
                primary_span: self.tokens.last().unwrap().span.splat_to_end(),
                secondary_messages: Vec::new(),
            }),
        };

        match &tok.kind {
            TokenKind::Integer(n) => {
                self.pos += 1;
                Ok(Node {
                    kind: NodeKind::Integer(*n),
                    span: tok.span,
                    ty: None,
                })
            },
            TokenKind::UnsignedInt(n) => {
                self.pos += 1;
                Ok(Node {
                    kind: NodeKind::UnsignedInt(*n),
                    span: tok.span,
                    ty: None,
                })
            },
            TokenKind::Float(n) => {
                self.pos += 1;
                Ok(Node {
                    kind: NodeKind::Float(*n),
                    span: tok.span,
                    ty: None,
                })
            },
            TokenKind::Identifier(n) => {
                self.pos += 1;
                if self.expect(TokenKind::LParen).is_ok() {
                    let mut args = Vec::new();
                    while self.tokens.get(self.pos).is_some() {
                        let arg = self.parse_expression(0)?;
                        args.push(arg);

                        if self.expect(TokenKind::Comma).is_err() {
                            break
                        }
                    }
                    let mut span = self.expect(TokenKind::RParen)?.span;
                    span.start = tok.span.start;
                    Ok(Node {
                        kind: NodeKind::FunctionCall {
                            callee: n.clone(),
                            args
                        },
                        span,
                        ty: None,
                    })
                } else {
                    Ok(Node {
                        kind: NodeKind::Identifier(n.clone()),
                        span: tok.span,
                        ty: None,
                    })
                }
            },
            TokenKind::I8(n) => {
                self.pos += 1;
                Ok(Node {
                    kind: NodeKind::I8(*n),
                    span: tok.span,
                    ty: None,
                })
            },
            TokenKind::I16(n) => {
                self.pos += 1;
                Ok(Node {
                    kind: NodeKind::I16(*n),
                    span: tok.span,
                    ty: None,
                })
            },
            TokenKind::I32(n) => {
                self.pos+= 1;
                Ok(Node {
                    kind: NodeKind::I32(*n),
                    span: tok.span,
                    ty: None,
                })
            },
            TokenKind::I64(n) => {
                self.pos += 1;
                Ok(Node {
                    kind: NodeKind::I64(*n),
                    span: tok.span,
                    ty: None,
                })
            },
            TokenKind::U8(n) => {
                self.pos += 1;
                Ok(Node {
                    kind: NodeKind::U8(*n),
                    span: tok.span,
                    ty: None,
                })
            },
            TokenKind::U16(n) => {
                self.pos += 1;
                Ok(Node {
                    kind: NodeKind::U16(*n),
                    span: tok.span,
                    ty: None,
                })
            },
            TokenKind::U32(n) => {
                self.pos+= 1;
                Ok(Node {
                    kind: NodeKind::U32(*n),
                    span: tok.span,
                    ty: None,
                })
            },
            TokenKind::U64(n) => {
                self.pos += 1;
                Ok(Node {
                    kind: NodeKind::U64(*n),
                    span: tok.span,
                    ty: None,
                })
            },
            TokenKind::F32(n) => {
                self.pos+= 1;
                Ok(Node {
                    kind: NodeKind::F32(*n),
                    span: tok.span,
                    ty: None,
                })
            },
            TokenKind::F64(n) => {
                self.pos += 1;
                Ok(Node {
                    kind: NodeKind::F64(*n),
                    span: tok.span,
                    ty: None,
                })
            },
            TokenKind::True => {
                self.pos += 1;
                Ok(Node {
                    kind: NodeKind::Boolean(true),
                    span: tok.span,
                    ty: None,
                })
            },
            TokenKind::False => {
                self.pos += 1;
                Ok(Node {
                    kind: NodeKind::Boolean(false),
                    span: tok.span,
                    ty: None,
                })
            },
            TokenKind::Operator(op) if op.is_prefix() => {
                self.pos += 1;
                let inner = self.parse_primary()?;
                let span = tok.span.connect(&inner.span);
                Ok(Node {
                    kind: NodeKind::UnaryOp {
                        op: *op,
                        operand: Box::new(inner),
                    },
                    span,
                    ty: None,
                })
            },
            TokenKind::LParen => {
                self.pos += 1;
                let inner = self.parse_expression(0)?;
                self.expect(TokenKind::RParen)?;
                Ok(inner)
            },
            TokenKind::LBracket => {
                let mut span = tok.span;
                self.pos += 1;
                let mut items = Vec::new();
                while let Some(tok) = self.tokens.get(self.pos) {
                    if tok.kind == TokenKind::RBracket { break }
                    items.push(self.parse_expression(0)?);
                    if self.expect(TokenKind::Comma).is_err() { break }
                }
                span.end = self.expect(TokenKind::RBracket)?.span.end;

                Ok(Node {
                    kind: NodeKind::Array(items),
                    span,
                    ty: None,
                })
            },
            TokenKind::Let => self.parse_decl(false),
            TokenKind::Var => self.parse_decl(true),
            TokenKind::If => self.parse_if(),
            _ => Err(Diagnostic {
                path: self.path.clone(),
                primary_err: format!("expected expression, found {}", tok),
                primary_span: tok.span,
                secondary_messages: Vec::new(),
            }),
        }
    }

    fn parse_decl(&mut self, mutability: bool) -> Result<Node, Diagnostic> {
        let mut stmt_span = self.tokens.get(self.pos).unwrap().span;
        self.pos += 1;

        let tok = self.expect_ident()?;
        let name = tok.lexeme.clone();
        stmt_span.end = tok.span.end;

        let ty = if self.expect(TokenKind::Colon).is_ok() {
            let ty = self.parse_type()?;
            stmt_span.end = ty.span.end;
            Some(ty)
        } else {
            None
        };

        let init = if self.expect(TokenKind::Assign).is_ok() {
            let expr = self.parse_expression(0)?;
            stmt_span.end = expr.span.end;
            Some(Box::new(expr))
        } else {
            None
        };

        Ok(Node {
            kind: NodeKind::Declaration {
                name,
                ty,
                init,
                mutability
            },
            span: stmt_span,
            ty: None,
        })
    }

    fn parse_if(&mut self) -> Result<Node, Diagnostic> {
        let mut stmt_span = self.tokens.get(self.pos).unwrap().span;
        self.pos += 1;

        let condition = Box::new(self.parse_expression(0)?);
        self.expect(TokenKind::Then)?;

        let mut then_body = Vec::new();
        while self.tokens.get(self.pos).is_some() {
            then_body.push(self.parse_statement()?);
            if let Some(Token { kind: TokenKind::End | TokenKind::Else, .. })
                = self.tokens.get(self.pos) {
                break
            }
        }
        let else_body = match self.expect(TokenKind::Else) {
            Ok(_) => {
                let mut else_body = Vec::new();
                while self.tokens.get(self.pos).is_some() {
                    else_body.push(self.parse_statement()?);
                    if let Some(Token { kind: TokenKind::End, .. }) = self.tokens.get(self.pos) {
                        break
                    }
                }
                stmt_span.end = else_body.last().unwrap().span.end;
                Some(else_body)
            },
            Err(_) => {
                None
            },
        };
        stmt_span.end = self.expect(TokenKind::End)?.span.end;

        Ok(Node {
            kind: NodeKind::IfCondition { condition, then_body, else_body },
            span: stmt_span,
            ty: None,
        })
    }

    fn parse_while(&mut self) -> Result<Node, Diagnostic> {
        let mut stmt_span = self.tokens.get(self.pos).unwrap().span;
        self.pos += 1;

        let condition = Box::new(self.parse_expression(0)?);
        stmt_span.end = self.expect(TokenKind::Do)?.span.end;
        let mut body = Vec::new();
        while self.tokens.get(self.pos).is_some() {
            body.push(self.parse_statement()?);
            if let Some(Token { kind: TokenKind::End, .. }) = self.tokens.get(self.pos) {
                break
            }
        }
        stmt_span.end = self.expect(TokenKind::End)?.span.end;

        Ok(Node {
            kind: NodeKind::WhileLoop { condition, body },
            span: stmt_span,
            ty: None,
        })

    }

    fn parse_type(&mut self) -> Result<ParseType, Diagnostic> {
        let tok = match self.tokens.get(self.pos) {
            Some(tok) => tok,
            None => return Err(Diagnostic {
                path: self.path.clone(),
                primary_err: "expected type, found end of input".to_string(),
                primary_span: self.tokens.last().unwrap().span.splat_to_end(),
                secondary_messages: Vec::new(),
            }),
        };

        match &tok.kind {
            TokenKind::Identifier(n) => {
                self.pos += 1;
                Ok(ParseType {
                    kind: ParseTypeKind::Identifier(n.clone()),
                    span: tok.span,
                })
            },
            TokenKind::LBracket => {
                self.pos += 1;
                let inner = Box::new(self.parse_type()?);
                let size = if self.expect(TokenKind::Semicolon).is_ok() {
                    if let TokenKind::Integer(n) = self.expect_int()?.kind {
                        Some(n as usize)
                    } else {
                        None
                    }
                } else {
                    None
                };
                let rbracket_span = self.expect(TokenKind::RBracket)?.span;
                Ok(ParseType {
                    kind: ParseTypeKind::Array { inner, size },
                    span: tok.span.connect(&rbracket_span),
                })
            }
            _ => Err(Diagnostic {
                path: self.path.clone(),
                primary_err: format!("expected type, found {}", tok),
                primary_span: tok.span,
                secondary_messages: Vec::new(),
            }),
        }
    }
}