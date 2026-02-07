//! # Semantics Analysis

pub mod symbol;
pub mod ty;

use std::collections::HashMap;
use crate::diag::Diagnostic;
use crate::operator::Operator;
use crate::parser::ast::{Node, NodeKind};
use crate::parser::ty::{ParseType, ParseTypeKind};
use crate::span::Span;
use colored::Colorize;
use symbol::{Symbol, InitState};
use ty::Type;

use strsim::jaro_winkler;

pub fn analyze(path: &str, ast: &mut [Node]) -> Result<(), Vec<Diagnostic>> {
    let mut analyzer = SemanticAnalyzer::new(path);
    analyzer.analyze(ast)
}

struct SemanticAnalyzer {
    path: String,
    scope: Vec<Vec<Symbol>>,
    type_registry: HashMap<String, Type>,
}

impl SemanticAnalyzer {
    fn new(path: &str) -> Self {
        let mut s = Self {
            path: path.to_string(),
            scope: vec![Vec::new()],
            type_registry: HashMap::new(),
        };

        s.type_registry.insert("int".to_string(), Type::Int);
        s.type_registry.insert("float".to_string(), Type::Float);

        s
    }

    fn define_identifier(
        &mut self,
        name: &str,
        ty: Type,
        mutability: bool,
        defined_at: Span,
        init_state: InitState,
    ) {
        self.scope.last_mut().unwrap().push(Symbol {
            name: name.to_string(), ty, mutability, defined_at, init_state
        });
    }

    fn find_identifier(&self, name: &str, span: Span) -> Result<&Symbol, Diagnostic> {
        let mut candidate_score: f64 = 0.0;
        let mut candidate = None;

        for frame in self.scope.iter().rev() {
            for symbol in frame {
                let score = jaro_winkler(name, &symbol.name);
                if score == 1.0 {
                    if symbol.init_state == InitState::Nope {
                        return Err(Diagnostic {
                            path: self.path.clone(),
                            primary_err: format!("`{name}` is uninitialized"),
                            primary_span: span,
                            secondary_messages: vec![(
                                Some(format!(
                                    "{}: identifier defined here:",
                                    "help".bright_blue().bold()
                                )),
                                Some(symbol.defined_at),
                            )],
                        });
                    }
                    if symbol.init_state == InitState::Maybe {
                        return Err(Diagnostic {
                            path: self.path.clone(),
                            primary_err: format!("`{name}` may be uninitialized"),
                            primary_span: span,
                            secondary_messages: vec![(
                                Some(format!(
                                    "{}: identifier defined here:",
                                    "help".bright_blue().bold()
                                )),
                                Some(symbol.defined_at),
                            )],
                        });
                    }
                    
                    return Ok(symbol);
                }

                candidate_score = candidate_score.max(score);
                candidate = Some(symbol);
            }
        }

        if candidate_score > 0.7 {
            return Err(Diagnostic {
                path: self.path.clone(),
                primary_err: format!("cannot find `{name}` in scope"),
                primary_span: span,
                secondary_messages: vec![(
                    Some(format!(
                        "{}: did you mean `{}`?",
                        "help".bright_blue().bold(),
                        candidate.unwrap().name
                    )),
                    None,
                )],
            });
        }

        Err(Diagnostic {
            path: self.path.clone(),
            primary_err: format!("cannot find `{name}` in scope"),
            primary_span: span,
            secondary_messages: vec![],
        })
    }

    fn find_identifier_mut(&mut self, name: &str, span: Span) -> Result<&mut Symbol, Diagnostic> {
        let mut candidate_score: f64 = 0.0;
        let mut candidate = None;

        for frame in self.scope.iter_mut().rev() {
            for symbol in frame {
                let score = jaro_winkler(name, &symbol.name);
                if score == 1.0 {
                    if symbol.init_state == InitState::Nope {
                        return Err(Diagnostic {
                            path: self.path.clone(),
                            primary_err: format!("`{name}` is uninitialized"),
                            primary_span: span,
                            secondary_messages: vec![(
                                Some(format!(
                                    "{}: identifier defined here:",
                                    "help".bright_blue().bold()
                                )),
                                Some(symbol.defined_at),
                            )],
                        });
                    }
                    if symbol.init_state == InitState::Maybe {
                        return Err(Diagnostic {
                            path: self.path.clone(),
                            primary_err: format!("`{name}` may be uninitialized"),
                            primary_span: span,
                            secondary_messages: vec![(
                                Some(format!(
                                    "{}: identifier defined here:",
                                    "help".bright_blue().bold()
                                )),
                                Some(symbol.defined_at),
                            )],
                        });
                    }

                    return Ok(symbol);
                }

                candidate_score = candidate_score.max(score);
                candidate = Some(&symbol.name);
            }
        }

        if candidate_score > 0.7 {
            return Err(Diagnostic {
                path: self.path.clone(),
                primary_err: format!("cannot find `{name}` in scope"),
                primary_span: span,
                secondary_messages: vec![(
                    Some(format!(
                        "{}: did you mean `{}`?",
                        "help".bright_blue().bold(),
                        candidate.unwrap()
                    )),
                    None,
                )],
            });
        }

        Err(Diagnostic {
            path: self.path.clone(),
            primary_err: format!("cannot find `{name}` in scope"),
            primary_span: span,
            secondary_messages: vec![],
        })
    }

    fn mutate_symbol(&mut self, name: &str, span: Span, val_ty: Type) -> Result<(), Diagnostic> {
        let path = self.path.clone();

        let symbol = self.find_identifier_mut(name, span)?;

        if !symbol.mutability && symbol.ty != Type::Unknown {
            return Err(Diagnostic {
                path,
                primary_err: format!("identifier `{name}` is immutable"),
                primary_span: span,
                secondary_messages: vec![(
                    Some(format!(
                        "{}: identifier defined here:",
                        "help".bright_blue().bold()
                    )),
                    Some(symbol.defined_at),
                )],
            });
        }

        if symbol.ty == Type::Unknown {
            symbol.ty = val_ty;
        } else if symbol.ty != val_ty {
            return Err(Diagnostic {
                path,
                primary_err: format!(
                    "identifier `{name}` is defined as `{}` but found `{val_ty}`",
                    symbol.ty,
                ),
                primary_span: span,
                secondary_messages: vec![(
                    Some(format!(
                        "{}: identifier defined here:",
                        "help".bright_blue().bold()
                    )),
                    Some(symbol.defined_at),
                )],
            });
        }

        Ok(())
    }

    fn resolve_type(&mut self, ty: &ParseType) -> Result<Type, Diagnostic> {
        match &ty.kind {
            ParseTypeKind::Identifier(n) => self.type_registry.get(n).ok_or_else(|| {
                let mut candidate_score: f64 = 0.0;
                let mut candidate = None;
                for (name, _) in &self.type_registry {
                    let score = jaro_winkler(n, &name);

                    candidate_score = candidate_score.max(score);
                    candidate = Some(name);
                }
                if candidate_score > 0.7 {
                    Diagnostic {
                        path: self.path.clone(),
                        primary_err: format!("unknown identifier type `{n}`"),
                        primary_span: ty.span,
                        secondary_messages: vec![(
                            Some(format!(
                                "{}: did you mean `{}`?",
                                "help".bright_blue().bold(),
                                candidate.unwrap()
                            )),
                            None,
                        )],
                    }
                } else {
                    Diagnostic {
                        path: self.path.clone(),
                        primary_err: format!("unknown identifier type `{n}`"),
                        primary_span: ty.span,
                        secondary_messages: vec![],
                    }
                }
            }).cloned(),
        }
    }

    fn analyze(&mut self, ast: &mut [Node]) -> Result<(), Vec<Diagnostic>> {
        let mut errors = Vec::new();

        for node in ast {
            if let Err(err) = self.analyze_node(node) {
                errors.extend(err);
            }
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        Ok(())
    }

    fn analyze_node(&mut self, node: &mut Node) -> Result<Type, Vec<Diagnostic>> {
        match &mut node.kind {
            NodeKind::Integer(_) => Ok(Type::Int),
            NodeKind::Float(_) => Ok(Type::Float),
            NodeKind::Identifier(n) => self
                .find_identifier(n, node.span)
                .map(|symbol| symbol.ty.clone())
                .map_err(|err| vec![err]),
            NodeKind::Semi(stmt) => {
                self.analyze_node(stmt)?;
                Ok(Type::Unit)
            }
            NodeKind::BinaryOp {
                op,
                lhs,
                rhs,
                ty_cache,
            } => {
                if *op == Operator::Walrus {
                    if let NodeKind::Identifier(n) = &lhs.kind {
                        let val_ty = self.analyze_node(rhs)?;
                        self.mutate_symbol(n, node.span, val_ty)
                            .map_err(|err| vec![err])?;
                        return Ok(Type::Unit);
                    } else {
                        return Err(vec![Diagnostic {
                            path: self.path.clone(),
                            primary_err: "can only mutate variables".to_string(),
                            primary_span: lhs.span,
                            secondary_messages: Vec::new(),
                        }]);
                    }
                }
                
                let lhs_ty = self.analyze_node(lhs)?;
                let rhs_ty = self.analyze_node(rhs)?;

                let ty = op.infix_output_ty(&lhs_ty, &rhs_ty).ok_or_else(|| {
                    vec![Diagnostic {
                        path: self.path.clone(),
                        primary_err: format!("cannot do `{op}` binary op on types `{lhs_ty}` and `{rhs_ty}`"),
                        primary_span: node.span,
                        secondary_messages: Vec::new(),
                    }]
                })?;

                *ty_cache = Some(lhs_ty);

                Ok(ty)
            },
            NodeKind::UnaryOp {
                op,
                operand,
                ty_cache,
            } => {
                let operand_ty = self.analyze_node(operand)?;

                let ty = op.prefix_output_ty(&operand_ty).ok_or_else(|| {
                    vec![Diagnostic {
                        path: self.path.clone(),
                        primary_err: format!("cannot do `{op}` unary op on type `{operand_ty}`"),
                        primary_span: node.span,
                        secondary_messages: Vec::new(),
                    }]
                })?;

                *ty_cache = Some(operand_ty);

                Ok(ty)
            },
            NodeKind::Declaration {
                name,
                ty,
                resolved_ty,
                init,
                mutability,
            } => {
                let init_state = init.is_some()
                    .then(|| InitState::Definitely)
                    .unwrap_or(InitState::Nope);

                let ty = match ty {
                    Some(ty) => self.resolve_type(ty)
                        .map_err(|err| vec![err])?,
                    None => Type::Unknown,
                };

                let init_ty = match init {
                    Some(init) => {
                        let t = self.analyze_node(init)?;

                        if ty != t && ty != Type::Unknown {
                            return Err(vec![Diagnostic {
                                path: self.path.clone(),
                                primary_err: format!("`{name}` is declared as `{ty}` but initialized as `{t}`"),
                                primary_span: init.span,
                                secondary_messages: Vec::new(),
                            }]);
                        }

                        t
                    },
                    None => Type::Unknown,
                };

                let final_ty = if ty != Type::Unknown {
                    ty
                } else if init_ty != Type::Unknown {
                    init_ty.clone()
                } else {
                    Type::Unknown
                };

                if final_ty == Type::Unknown {
                    return Err(vec![Diagnostic {
                        path: self.path.clone(),
                        primary_err: "type must be defined at declaration".to_string(),
                        primary_span: node.span,
                        secondary_messages: Vec::new(),
                    }]);
                }

                *resolved_ty = Some(final_ty.clone());

                self.define_identifier(name, final_ty, *mutability, node.span, init_state);

                Ok(init_ty)
            },
        }
    }
}
