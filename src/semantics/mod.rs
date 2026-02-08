//! # Semantics Analysis

pub mod symbol;
pub mod ty;
pub mod scope;

use std::collections::HashMap;
use crate::diag::Diagnostic;
use crate::operator::Operator;
use crate::parser::ast::{Node, NodeKind};
use crate::parser::ty::{ParseType, ParseTypeKind};
use crate::span::Span;
use colored::Colorize;
use symbol::{Symbol, InitState};
use ty::Type;
use scope::{Scope, ScopeContext};

use strsim::jaro_winkler;

pub fn analyze(path: &str, ast: &mut [Node]) -> Result<(), Vec<Diagnostic>> {
    let mut analyzer = SemanticAnalyzer::new(path);
    analyzer.analyze(ast)
}

struct SemanticAnalyzer {
    path: String,
    scope: Vec<Scope>,
    type_registry: HashMap<String, Type>,
}

impl SemanticAnalyzer {
    fn new(path: &str) -> Self {
        let mut s = Self {
            path: path.to_string(),
            scope: vec![Scope::new(ScopeContext::Root)],
            type_registry: HashMap::new(),
        };

        s.type_registry.insert("int".to_string(), Type::Int);
        s.type_registry.insert("uint".to_string(), Type::UInt);
        s.type_registry.insert("float".to_string(), Type::Float);
        s.type_registry.insert("bool".to_string(), Type::Bool);
        s.type_registry.insert("i8".to_string(), Type::I8);
        s.type_registry.insert("i16".to_string(), Type::I16);
        s.type_registry.insert("i32".to_string(), Type::I32);
        s.type_registry.insert("i64".to_string(), Type::I64);
        s.type_registry.insert("u8".to_string(), Type::U8);
        s.type_registry.insert("u16".to_string(), Type::U16);
        s.type_registry.insert("u32".to_string(), Type::U32);
        s.type_registry.insert("u64".to_string(), Type::U64);
        s.type_registry.insert("f32".to_string(), Type::F32);
        s.type_registry.insert("f64".to_string(), Type::F64);

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
        self.scope.last_mut().unwrap().insert(Symbol {
            name: name.to_string(), ty, mutability, defined_at, init_state
        });
    }

    fn find_identifier(&self, name: &str, span: Span) -> Result<&Symbol, Diagnostic> {
        let mut candidate_score: f64 = 0.0;
        let mut candidate = None;

        for frame in self.scope.iter().rev() {
            for symbol in &frame.symbols {
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
                                    "note".bright_blue().bold()
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
                                    "note".bright_blue().bold()
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
    
    fn mutate_symbol(&mut self, name: &str, span: Span, val: &mut Node) -> Result<Type, Vec<Diagnostic>> {
        fn aux<'a>(a: &'a mut SemanticAnalyzer, name: &str, span: Span) -> Result<&'a mut Symbol, Diagnostic> {
            let mut candidate_score: f64 = 0.0;
            let mut candidate = None;

            for frame in a.scope.iter_mut().rev() {
                for symbol in &mut frame.symbols {
                    let score = jaro_winkler(name, &symbol.name);
                    if score == 1.0 {
                        return Ok(symbol);
                    }

                    candidate_score = candidate_score.max(score);
                    candidate = Some(&symbol.name);
                }
            }

            if candidate_score > 0.7 {
                return Err(Diagnostic {
                    path: a.path.clone(),
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
                path: a.path.clone(),
                primary_err: format!("cannot find `{name}` in scope"),
                primary_span: span,
                secondary_messages: vec![],
            })
        }

        let path = self.path.clone();

        let mut errors = Vec::new();

        let val_ty = match self.analyze_node(val) {
            Ok(n) => Some(n),
            Err(e) => { errors.extend(e); None },
        };

        let scope_ctx = self.scope.last().unwrap().ctx;

        let symbol = aux(self, name, span).map_err(|err| vec![err])?;

        if !symbol.mutability {
            errors.push(Diagnostic {
                path: path.clone(),
                primary_err: format!("identifier `{name}` is immutable"),
                primary_span: span,
                secondary_messages: vec![(
                    Some(format!(
                        "{}: identifier defined here:",
                        "note".bright_blue().bold()
                    )),
                    Some(symbol.defined_at),
                )],
            });
        }

        if let Some(ref val_ty) = val_ty {
            if symbol.ty != *val_ty {
                errors.push(Diagnostic {
                    path,
                    primary_err: format!(
                        "identifier `{name}` is defined as `{}` but found `{val_ty}`",
                        symbol.ty,
                    ),
                    primary_span: span,
                    secondary_messages: vec![(
                        Some(format!(
                            "{}: identifier defined here:",
                            "note".bright_blue().bold()
                        )),
                        Some(symbol.defined_at),
                    )],
                });
            }

            symbol.ty = val_ty.clone();
        }

        if scope_ctx == ScopeContext::Conditional && symbol.init_state == InitState::Nope {
            symbol.init_state = InitState::Maybe;
        }

        if errors.is_empty() { Ok(val_ty.unwrap()) } else { Err(errors) }
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
            NodeKind::UnsignedInt(_) => Ok(Type::UInt),
            NodeKind::Float(_) => Ok(Type::Float),
            NodeKind::Boolean(_) => Ok(Type::Bool),
            NodeKind::I8(_) => Ok(Type::I8),
            NodeKind::I16(_) => Ok(Type::I16),
            NodeKind::I32(_) => Ok(Type::I32),
            NodeKind::I64(_) => Ok(Type::I64),
            NodeKind::U8(_) => Ok(Type::U8),
            NodeKind::U16(_) => Ok(Type::U16),
            NodeKind::U32(_) => Ok(Type::U32),
            NodeKind::U64(_) => Ok(Type::U64),
            NodeKind::F32(_) => Ok(Type::F32),
            NodeKind::F64(_) => Ok(Type::F64),
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
                    return
                        if let NodeKind::Identifier(n) = &lhs.kind {
                            self.mutate_symbol(n, node.span, rhs)
                        } else {
                            Err(vec![Diagnostic {
                                path: self.path.clone(),
                                primary_err: "can only mutate variables".to_string(),
                                primary_span: lhs.span,
                                secondary_messages: Vec::new(),
                            }])
                        };
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
                    Some(ty) => Some(self.resolve_type(ty)
                        .map_err(|err| vec![err])?),
                    None => None,
                };

                let init_ty = match init {
                    Some(init) => {
                        let t = self.analyze_node(init)?;

                        if ty != None {
                            if ty.as_ref().unwrap() != &t {
                                return Err(vec![Diagnostic {
                                    path: self.path.clone(),
                                    primary_err: format!("`{name}` is declared as `{}` but initialized as `{t}`", ty.unwrap()),
                                    primary_span: init.span,
                                    secondary_messages: Vec::new(),
                                }]);
                            }
                        }

                        Some(t)
                    },
                    None => None,
                };

                let final_ty = if ty != None {
                    ty
                } else if init_ty != None {
                    init_ty.clone()
                } else {
                    None
                };

                if let Some(final_ty) = final_ty {
                    *resolved_ty = Some(final_ty.clone());

                    self.define_identifier(name, final_ty.clone(), *mutability, node.span, init_state);

                    Ok(final_ty)
                } else {
                    Err(vec![Diagnostic {
                        path: self.path.clone(),
                        primary_err: "type must be defined at declaration".to_string(),
                        primary_span: node.span,
                        secondary_messages: Vec::new(),
                    }])
                }
            },
            NodeKind::IfCondition {
                condition,
                then_body,
                else_body,
                ty_cache
            } => {
                let mut errors = Vec::new();
                match self.analyze_node(condition) {
                    Ok(ty) => if ty != Type::Bool {
                        errors.push(Diagnostic {
                            path: self.path.clone(),
                            primary_err: format!("expected `bool`, found `{ty}`"),
                            primary_span: condition.span,
                            secondary_messages: Vec::new(),
                        });
                    },
                    Err(err) => errors.extend(err),
                }

                let mut then_ty = None;

                self.scope.push(Scope::new(ScopeContext::Conditional));
                for node in &mut *then_body {
                    then_ty = match self.analyze_node(node) {
                        Ok(ty) => Some(ty),
                        Err(err) => {
                            errors.extend(err);
                            None
                        },
                    };
                }
                self.scope.pop();

                if then_ty.is_none() {
                    return Err(errors);
                }

                let then_ty = then_ty.unwrap();

                match else_body {
                    Some(else_body) => {
                        self.scope.push(Scope::new(ScopeContext::Conditional));
                        let mut else_ty = None;

                        for node in &mut *else_body {
                            else_ty = match self.analyze_node(node) {
                                Ok(ty) => Some(ty),
                                Err(err) => {
                                    errors.extend(err);
                                    None
                                },
                            };
                        }
                        if else_ty.is_none() {
                            return Err(errors);
                        }
                        self.scope.pop();
                        let start = else_body.first().unwrap().span;
                        let primary_span = start.connect(&else_body.last().unwrap().span);

                        let start = then_body.first().unwrap().span;
                        let then_body_span = start.connect(&then_body.last().unwrap().span);
                        if else_ty.unwrap() != then_ty {
                            errors.push(Diagnostic {
                                path: self.path.clone(),
                                primary_err: format!("expected type `{}`", then_ty),
                                primary_span,
                                secondary_messages: vec![(
                                    Some(format!("type `{then_ty}` was inferred here:")),
                                    Some(then_body_span)
                                )],
                            });
                        }
                    },
                    None => {
                        if then_ty != Type::Unit {
                            errors.push(Diagnostic {
                                path: self.path.clone(),
                                primary_err: format!("missing `else` clause that evaluates to `{then_ty}`"),
                                primary_span: node.span,
                                secondary_messages: Vec::new(),
                            });
                        }
                    }
                }

                *ty_cache = Some(then_ty.clone());

                if errors.is_empty() { Ok(then_ty) } else { Err(errors) }
            },
            NodeKind::WhileLoop {
                condition,
                body,
            } => {
                let mut errors = Vec::new();
                match self.analyze_node(condition) {
                    Ok(ty) => if ty != Type::Bool {
                        errors.push(Diagnostic {
                            path: self.path.clone(),
                            primary_err: format!("expected `bool`, found `{ty}`"),
                            primary_span: condition.span,
                            secondary_messages: Vec::new(),
                        });
                    },
                    Err(err) => errors.extend(err),
                }

                for node in body {
                    if let Err(e) = self.analyze_node(node) {
                        errors.extend(e);
                    }
                }

                if errors.is_empty() { Ok(Type::Unit) } else { Err(errors) }
            },
        }
    }
}
