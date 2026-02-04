pub mod ast;
pub mod ty;

use crate::lexer::token::{Token, TokenKind};
use crate::diag::Diagnostic;
use ast::{Node, NodeKind};
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

    fn expect(&mut self, expected: &str) -> Result<(), Diagnostic> {
        match self.tokens.get(self.pos) {
            Some(tok) => if tok.to_string() == format!("`{}`", expected) {
                self.pos += 1;
                Ok(())
            } else {
                Err(Diagnostic {
                    path: self.path.clone(),
                    primary_err: format!("Expected `{expected}`, found {tok}"),
                    primary_span: self.tokens.last().unwrap().span.splat_to_end(),
                    secondary_messages: Vec::new(),
                })
            },
            None => return Err(Diagnostic {
                path: self.path.clone(),
                primary_err: format!("Expected `{expected}`, found end of input"),
                primary_span: self.tokens.last().unwrap().span.splat_to_end(),
                secondary_messages: Vec::new(),
            }),
        }
    }

    fn expect_ident(&mut self) -> Result<Token, Diagnostic> {
        match self.tokens.get(self.pos) {
            Some(tok) if matches!(tok.kind, TokenKind::Identifier(_)) => {
                self.pos += 1;
                Ok(tok.clone())
            },
            Some(tok) => {
                Err(Diagnostic {
                    path: self.path.clone(),
                    primary_err: format!("Expected identifier, found {tok}"),
                    primary_span: self.tokens.last().unwrap().span.splat_to_end(),
                    secondary_messages: Vec::new(),
                })
            },
            None => return Err(Diagnostic {
                path: self.path.clone(),
                primary_err: format!("Expected identifier, found end of input"),
                primary_span: self.tokens.last().unwrap().span.splat_to_end(),
                secondary_messages: Vec::new(),
            }),
        }
    }

    fn parse(&mut self) -> Result<Vec<Node>, Diagnostic> {
        let mut stmts = Vec::new();

        while self.tokens.get(self.pos).is_some() {
            stmts.push(self.parse_statement()?);
        }

        Ok(stmts)
    }

    fn parse_statement(&mut self) -> Result<Node, Diagnostic> {
        let mut node = self.parse_expression(0)?;

        match self.tokens.get(self.pos) {
            Some(Token { kind: TokenKind::Semicolon, span, .. }) => {
                let span = node.span.connect(span);
                node = Node {
                    kind: NodeKind::Semi(Box::new(node)),
                    span
                };
                self.pos += 1;
            },
            _ => (),
        }

        Ok(node)
    }

    fn parse_expression(&mut self, mbp: usize) -> Result<Node, Diagnostic> {
        let mut lhs = self.parse_primary()?;

        while let Some(Token { kind: TokenKind::Operator(op), .. }) = self.tokens.get(self.pos) {
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
                    ty_cache: None,
                },
                span,
            };
        }

        Ok(lhs)
    }

    fn parse_primary(&mut self) -> Result<Node, Diagnostic> {
        let tok = match self.tokens.get(self.pos) {
            Some(tok) => tok,
            None => return Err(Diagnostic {
                path: self.path.clone(),
                primary_err: "Expected expression, found end of input".to_string(),
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
                })
            },
            TokenKind::Float(n) => {
                self.pos += 1;
                Ok(Node {
                    kind: NodeKind::Float(*n),
                    span: tok.span,
                })
            },
            TokenKind::Identifier(n) => {
                self.pos += 1;
                Ok(Node {
                    kind: NodeKind::Identifier(n.clone()),
                    span: tok.span,
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
                        ty_cache: None,
                    },
                    span,
                })
            },
            TokenKind::LParen => {
                self.pos += 1;
                let inner = self.parse_expression(0)?;
                self.expect(")")?;
                Ok(inner)
            },
            TokenKind::Let => self.parse_decl(false),
            TokenKind::Var => self.parse_decl(true),
            _ => Err(Diagnostic {
                path: self.path.clone(),
                primary_err: format!("Expected expression, found {}", tok),
                primary_span: tok.span,
                secondary_messages: Vec::new(),
            }),
        }
    }

    fn parse_decl(&mut self, mutability: bool) -> Result<Node, Diagnostic> {
        let mut stmt_span = self.tokens.get(self.pos).unwrap().span;
        self.pos += 1;

        let tok = self.expect_ident()?;
        let name = tok.lexeme;
        stmt_span.end = tok.span.end;

        let ty = if self.expect(":").is_ok() {
            let ty = self.parse_type()?;
            stmt_span.end = ty.span.end;
            Some(ty)
        } else {
            None
        };

        let init = if self.expect("=").is_ok() {
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
        })
    }

    fn parse_type(&mut self) -> Result<ParseType, Diagnostic> {
        let tok = match self.tokens.get(self.pos) {
            Some(tok) => tok,
            None => return Err(Diagnostic {
                path: self.path.clone(),
                primary_err: "Expected type, found end of input".to_string(),
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
            _ => Err(Diagnostic {
                path: self.path.clone(),
                primary_err: format!("Expected expression, found {}", tok),
                primary_span: tok.span,
                secondary_messages: Vec::new(),
            }),
        }
    }
}