//! # Semantics Analysis

pub mod scope;
pub mod symbol;
pub mod ty;

use crate::diag::Diagnostic;
use crate::operator::Operator;
use crate::parser::ast::{Node, NodeKind};
use crate::parser::ty::{ParseType, ParseTypeKind};
use crate::span::Span;
use colored::Colorize;
use scope::{Scope, ScopeContext};
use std::collections::HashMap;
use symbol::{InitState, Symbol};
use ty::Type;

const CANDIDATE_SCORE_THRESHOLD: f64 = 0.8;

use strsim::jaro_winkler;

pub fn analyze(path: &str, ast: &mut [Node], source_len: usize) -> Result<SemanticAnalyzer, Vec<Diagnostic>> {
    let mut analyzer = SemanticAnalyzer::new(path, source_len);
    analyzer.analyze(ast)?;
    Ok(analyzer)
}

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
pub struct StructID(pub u32);

#[derive(Debug, Clone, PartialEq)]
pub struct StructData {
    pub fields: Box<[(String, Type)]>,
}

pub struct SemanticAnalyzer {
    path: String,
    scope: Vec<Scope>,
    type_registry: HashMap<String, Type>,
    source_len: usize,
    pub ids: HashMap<String, (StructID, Span)>,
    pub structs: HashMap<StructID, Option<StructData>>,
    next_struct_id: u32,
}

impl SemanticAnalyzer {
    fn new(path: &str, source_len: usize) -> Self {
        let mut s = Self {
            path: path.to_string(),
            scope: vec![Scope::new(ScopeContext::Root)],
            type_registry: HashMap::new(),
            source_len,
            ids: HashMap::new(),
            structs: HashMap::new(),
            next_struct_id: 0,
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

    fn collect_struct_names(&mut self, node: &Node) -> Result<(), Diagnostic> {
        match &node.kind {
            NodeKind::StructDef {
                name,
                ..
            } => {
                if let Some((_, span)) = self.ids.get(name) {
                    return Err(Diagnostic {
                        path: self.path.clone(),
                        primary_err: format!("duplicate struct definitions of `{name}`"),
                        primary_span: node.span,
                        secondary_messages: vec![(
                            Some(format!("{}: `{name}` defined here:", "note".bright_blue().bold())),
                            Some(*span),
                        )]
                    });
                }
                self.ids.insert(name.clone(), (StructID(self.next_struct_id), node.span));
                self.next_struct_id += 1;
            },
            _ => {},
        }

        Ok(())
    }

    fn collect_struct_definitions(&mut self, node: &mut Node) -> Result<(), Vec<Diagnostic>> {
        match &mut node.kind {
            NodeKind::StructDef {
                name,
                fields,
            } => {
                let mut errors = Vec::new();
                let mut resolved_fields = Vec::new();

                for field in fields {
                    let mut s: Span = Span { start: 0, end: 0}; // this value won't get used
                    if resolved_fields.iter().any(|(n, _, sp)| {
                        let f = *n == field.name;
                        if f {
                            s = *sp
                        }
                        f
                    }) {
                        errors.push(Diagnostic {
                            path: self.path.clone(),
                            primary_err: format!("duplicate field `{}`", field.name),
                            primary_span: field.span,
                            secondary_messages: vec![
                                (
                                    Some(format!("{}: field `{}` was defined here:", "note".bright_blue().bold(), field.name)),
                                    Some(s)
                                )
                            ]
                        })
                    }
                    match self.resolve_type(&field.ty) {
                        Ok(ty) => resolved_fields.push((field.name.clone(), ty, field.span)),
                        Err(err) => errors.push(err),
                    }
                }

                let (id, _) = self.ids[name];
                self.structs.insert(id, Some(StructData {
                    fields: resolved_fields
                        .clone()
                        .into_iter()
                        .map(|(name, ty, _)| (name, ty))
                        .collect::<Vec<_>>()
                        .into_boxed_slice(),
                }));
                let f = resolved_fields
                    .into_iter()
                    .map(|(_, ty, _)| ty)
                    .collect();
                self.define_identifier(
                    name,
                    Type::Function(f, Box::new(Type::Struct(id, name.to_string()))),
                    false,
                    node.span,
                    InitState::Definitely,
                );
            },
            _ => {},
        }

        Ok(())
    }

    fn collect_function(&mut self, node: &mut Node) -> Result<(), Vec<Diagnostic>> {
        match &mut node.kind {
            NodeKind::FunctionDef {
                name,
                params,
                return_ty,
                ty_cache,
                errored,
                ..
            } => {
                (|| {
                    let mut param_tys = Vec::new();
                    for param in params {
                        let r = self.resolve_type(&param.ty)?;
                        param.ty_cache = Some(r.clone());
                        param_tys.push(r);
                    }
                    let return_ty = match return_ty {
                        Some(ty) => self.resolve_type(ty)?,
                        None => Type::Unit,
                    };
                    *ty_cache = Some(return_ty.clone());
                    let ty = Type::Function(param_tys, Box::new(return_ty));
                    if let Ok(symbol) = self.find_identifier(name, node.span) {
                        return Err(Diagnostic {
                            path: self.path.clone(),
                            primary_err: format!("`{name}` defined multiple times"),
                            primary_span: node.span,
                            secondary_messages: vec![(
                                Some(format!("`{name}` was defined here:")),
                                Some(symbol.defined_at),
                            )]
                        });
                    }
                    self.define_identifier(name, ty, false, node.span, InitState::Definitely);
                    Ok::<(), Diagnostic>(())
                })().inspect_err(|_| *errored = true)
                .map_err(|err| vec![err])?;
            }
            _ => {}
        }

        Ok(())
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
            name: name.to_string(),
            ty,
            mutability,
            defined_at,
            init_state,
        });
    }

    fn find_identifier(&self, name: &str, span: Span) -> Result<&Symbol, Diagnostic> {
        let mut candidate_score: f64 = 0.0;
        let mut candidate = None;

        for frame in self.scope.iter().rev() {
            for symbol in &frame.symbols {
                if symbol.name == name {
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
                let score = jaro_winkler(name, &symbol.name);

                candidate_score = candidate_score.max(score);
                candidate = Some(symbol);
            }
        }

        if candidate_score > CANDIDATE_SCORE_THRESHOLD {
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

    fn find_identifier_mut(
        &mut self,
        name: &str,
        span: Span,
    ) -> Result<&mut Symbol, Diagnostic> {
        let mut candidate_score: f64 = 0.0;
        let mut candidate = None;

        for frame in self.scope.iter_mut().rev() {
            for symbol in &mut frame.symbols {
                if symbol.name == name {
                    return Ok(symbol);
                }
                let score = jaro_winkler(name, &symbol.name);

                candidate_score = candidate_score.max(score);
                candidate = Some(&symbol.name);
            }
        }

        if candidate_score > CANDIDATE_SCORE_THRESHOLD {
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

    fn find_mutation_target(
        &mut self,
        node: &mut Node,
        span: Span,
    ) -> Result<&mut Symbol, Diagnostic> {
        match &mut node.kind {
            NodeKind::Identifier(n) => self.find_identifier_mut(n, span),
            NodeKind::Index { collection, .. } => {
                let path = self.path.clone();
                let symbol = self.find_mutation_target(collection, collection.span)?;

                if let Type::Array(inner, _) = &symbol.ty {
                    node.ty = Some((**inner).clone());
                } else {
                    return Err(Diagnostic {
                        path,
                        primary_err: "can only index into collections".to_string(),
                        primary_span: collection.span,
                        secondary_messages: Vec::new(),
                    });
                }

                Ok(symbol)
            },
            _ => Err(Diagnostic {
                path: self.path.clone(),
                primary_err: "invalid mutation target".to_string(),
                primary_span: node.span,
                secondary_messages: Vec::new(),
            })
        }
    }

    fn mutate_symbol(
        &mut self,
        node: &mut Node,
        span: Span,
        val: &mut Node,
    ) -> Result<Type, Vec<Diagnostic>> {
        let path = self.path.clone();

        let mut errors = Vec::new();

        let mut val_ty = match self.analyze_node(val) {
            Ok(n) => Some(n),
            Err(e) => {
                errors.extend(e);
                None
            }
        };

        let scope_ctx = self.scope.last().unwrap().ctx;

        match &mut node.kind {
            NodeKind::Identifier(name) => {
                let symbol = self.find_identifier_mut(name, span).map_err(|err| vec![err])?;

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

                if let Some(ref val_type) = val_ty {
                    if symbol.ty != *val_type {
                        errors.push(Diagnostic {
                            path,
                            primary_err: format!(
                                "identifier `{name}` is defined as `{}` but found `{val_type}`",
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

                    val_ty = Some(symbol.ty.clone());
                }

                if scope_ctx == ScopeContext::Conditional && symbol.init_state == InitState::Nope {
                    symbol.init_state = InitState::Maybe;
                }
            },
            NodeKind::Index {
                index,
                ..
            } => {
                let ty = self.analyze_node(index)
                    .map_err(|err| errors.extend(err))
                    .ok();
                if let Some(ty) = &ty {
                    if *ty != Type::UInt {
                        errors.push(Diagnostic {
                            path: self.path.clone(),
                            primary_err: format!("expected uint, found `{ty}`"),
                            primary_span: index.span,
                            secondary_messages: Vec::new(),
                        });
                    }
                }

                let symbol = self.find_mutation_target(node, node.span)
                    .map_err(|err| vec![err])?;
                if !symbol.mutability {
                    errors.push(Diagnostic {
                        path: path.clone(),
                        primary_err: format!("collection is immutable"),
                        primary_span: span,
                        secondary_messages: vec![(
                            Some(format!(
                                "{}: collection defined here:",
                                "note".bright_blue().bold()
                            )),
                            Some(symbol.defined_at),
                        )],
                    });
                }
                match &symbol.ty {
                    Type::Array(inner, _) => {
                        if let Some(ref val_type) = val_ty {
                            if **inner != *val_type {
                                errors.push(Diagnostic {
                                    path,
                                    primary_err: format!("collection has inner type `{inner}` but found `{val_type}`"),
                                    primary_span: span,
                                    secondary_messages: vec![(
                                        Some(format!(
                                            "{}: collection defined here:",
                                            "note".bright_blue().bold()
                                        )),
                                        Some(symbol.defined_at),
                                    )],
                                });
                            }
                        }
                        val_ty = Some((**inner).clone());

                    },
                    _ => errors.push(Diagnostic {
                        path,
                        primary_err: "invalid mutation target".to_string(),
                        primary_span: node.span,
                        secondary_messages: Vec::new(),
                    }),
                }
            },
            NodeKind::FieldAccess { object, field, id, .. } => {
                let path = self.path.clone();
                let symbol = self.find_mutation_target(object, object.span)
                    .map_err(|err| vec![err])?.clone();

                if let Type::Struct(sid, _) = &symbol.ty {
                    'a: loop {
                        *id = Some(*sid);
                        let data = self.structs[sid].as_ref().unwrap();
                        let mut candidate = None;
                        let mut candidate_score = 0.0;
                        for (name, ty) in &data.fields {
                            if *name == field.0 {
                                if let Some(ref val_type) = val_ty {
                                    if ty != val_type {
                                        errors.push(Diagnostic {
                                            path: path.clone(),
                                            primary_err: format!("object has field `{}` of type `{ty}` but found `{val_type}`", field.0),
                                            primary_span: span,
                                            secondary_messages: vec![(
                                                Some(format!(
                                                    "{}: object defined here:",
                                                    "note".bright_blue().bold()
                                                )),
                                                Some(symbol.defined_at),
                                            )],
                                        });
                                    }
                                }
                                val_ty = Some(ty.clone());
                                break 'a;
                            }

                            candidate_score = jaro_winkler(name, &field.0);
                            candidate = Some(name);
                        }

                        if candidate_score > CANDIDATE_SCORE_THRESHOLD {
                            return Err(vec![Diagnostic {
                                path: self.path.clone(),
                                primary_err: format!("cannot find field `{}` in object of type `{}`", field.0, symbol.ty),
                                primary_span: field.1,
                                secondary_messages: vec![(
                                    Some(format!("{}: did you mean `{}`", "help".bright_blue().bold(), candidate.unwrap())),
                                    None
                                ), (
                                    Some(format!("{}: `{}` has fields:\n{}",
                                        "note".bright_blue().bold(),
                                        symbol.ty,
                                        data.fields.iter()
                                            .map(|(name, ty)| format!("\t+ {name}: {ty}"))
                                            .collect::<Vec<_>>().join("\n")
                                    )),
                                    None
                                )],
                            }]);
                        }

                        return Err(vec![Diagnostic {
                            path: self.path.clone(),
                            primary_err: format!("cannot find field `{}` in object of type `{}`", field.0, symbol.ty),
                            primary_span: field.1,
                            secondary_messages: vec![(
                                Some(format!("{}: did you mean `{}`", "help".bright_blue().bold(), candidate.unwrap())),
                                None
                            ), (
                                Some(format!("{}: `{}` has fields:\n{}",
                                    "note".bright_blue().bold(),
                                    symbol.ty,
                                    data.fields.iter()
                                        .map(|(name, ty)| format!("\t+ {name}: {ty}"))
                                        .collect::<Vec<_>>().join("\n")
                                )),
                                None
                            )],
                        }]);
                    }
                } else {
                    return Err(vec![Diagnostic {
                        path,
                        primary_err: "can only access into object".to_string(),
                        primary_span: object.span,
                        secondary_messages: Vec::new(),
                    }]);
                }

                if !symbol.mutability {
                    errors.push(Diagnostic {
                        path: path.clone(),
                        primary_err: format!("object is immutable"),
                        primary_span: span,
                        secondary_messages: vec![(
                            Some(format!(
                                "{}: object defined here:",
                                "note".bright_blue().bold()
                            )),
                            Some(symbol.defined_at),
                        )],
                    });
                }
            },
            _ => {
                errors.push(Diagnostic {
                    path,
                    primary_err: "invalid mutation target".to_string(),
                    primary_span: node.span,
                    secondary_messages: Vec::new(),
                });
            }
        }

        if errors.is_empty() {
            let ty = val_ty.unwrap();
            val.ty = Some(ty.clone());
            return Ok(ty);
        }
        Err(errors)
    }

    fn resolve_type(&mut self, ty: &ParseType) -> Result<Type, Diagnostic> {
        match &ty.kind {
            ParseTypeKind::Identifier(n) => {
                let mut candidate_score: f64 = 0.0;
                let mut candidate = None;
                for (name, ty) in &self.type_registry {
                    if name == n {
                        return Ok(ty.clone());
                    }
                    let score = jaro_winkler(n, &name);

                    candidate_score = candidate_score.max(score);
                    candidate = Some(name);
                }
                
                for (name, (id, _)) in &self.ids {
                    if name == n {
                        return Ok(Type::Struct(*id, name.clone()));
                    }
                    let score = jaro_winkler(n, &name);

                    candidate_score = candidate_score.max(score);
                    candidate = Some(name);
                }

                if candidate_score > CANDIDATE_SCORE_THRESHOLD {
                    Err(Diagnostic {
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
                    })
                } else {
                    Err(Diagnostic {
                        path: self.path.clone(),
                        primary_err: format!("unknown identifier type `{n}`"),
                        primary_span: ty.span,
                        secondary_messages: vec![],
                    })
                }
            },
            ParseTypeKind::Array {
                inner,
                size
            } => {
                Ok(Type::Array(Box::new(self.resolve_type(inner)?), *size))
            },
        }
    }

    fn analyze(&mut self, ast: &mut [Node]) -> Result<(), Vec<Diagnostic>> {
        let mut errors = Vec::new();

        for node in &mut *ast {
            if let Err(err) = self.collect_struct_names(node) {
                errors.push(err);
            }
        }

        if !errors.is_empty() {
            return Err(errors)
        }

        for node in &mut *ast {
            if let Err(err) = self.collect_struct_definitions(node) {
                errors.extend(err);
            }
        }

        if !errors.is_empty() {
            return Err(errors)
        }

        for node in &mut *ast {
            if let Err(err) = self.collect_function(node) {
                errors.extend(err);
            }
        }

        if !errors.is_empty() {
            return Err(errors)
        }

        for node in ast {
            match self.analyze_node(node) {
                Ok(ty) => node.ty = Some(ty),
                Err(err) => errors.extend(err),
            }
        }

        match self.find_identifier("main", Span { start: 0, end: 0 }) {
            Ok(sy) => {
                let expected_ty = Type::Function(Vec::new(), Box::new(Type::Int));
                if sy.ty != expected_ty {
                    errors.push(Diagnostic {
                        path: self.path.clone(),
                        primary_err: format!("`main` function must have signature `{expected_ty}`"),
                        primary_span: sy.defined_at,
                        secondary_messages: Vec::new(),
                    }); 
                }
            }
            Err(_) => errors.push(Diagnostic {
                path: self.path.clone(),
                primary_err: format!("expected `main` function"),
                primary_span: Span {
                    start: self.source_len.saturating_sub(1),
                    end: self.source_len,
                },
                secondary_messages: Vec::new(),
            }),
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
                let ty = self.analyze_node(stmt)?;
                stmt.ty = Some(ty);
                Ok(Type::Unit)
            }
            NodeKind::BinaryOp {
                op,
                lhs,
                rhs,
            } => {
                if *op == Operator::Walrus {
                    return self.mutate_symbol(lhs, node.span, rhs);
                }

                let lhs_ty = self.analyze_node(lhs)?;
                let rhs_ty = self.analyze_node(rhs)?;

                let ty = op.infix_output_ty(&lhs_ty, &rhs_ty).ok_or_else(|| {
                    vec![Diagnostic {
                        path: self.path.clone(),
                        primary_err: format!(
                            "cannot do `{op}` binary op on types `{lhs_ty}` and `{rhs_ty}`"
                        ),
                        primary_span: node.span,
                        secondary_messages: Vec::new(),
                    }]
                })?;

                lhs.ty = Some(lhs_ty);
                rhs.ty = Some(rhs_ty);

                Ok(ty)
            }
            NodeKind::UnaryOp {
                op,
                operand,
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

                operand.ty = Some(operand_ty);

                Ok(ty)
            }
            NodeKind::Declaration {
                name,
                ty,
                init,
                mutability,
            } => {
                let init_state = init
                    .is_some()
                    .then(|| InitState::Definitely)
                    .unwrap_or(InitState::Nope);

                let ty = match ty {
                    Some(ty) => Some(self.resolve_type(ty).map_err(|err| vec![err])?),
                    None => None,
                };

                let init_ty = match init {
                    Some(init) => {
                        let t = self.analyze_node(init)?;

                        if ty != None {
                            if ty.as_ref().unwrap() != &t {
                                return Err(vec![Diagnostic {
                                    path: self.path.clone(),
                                    primary_err: format!(
                                        "`{name}` is declared as `{}` but initialized as `{t}`",
                                        ty.unwrap()
                                    ),
                                    primary_span: init.span,
                                    secondary_messages: Vec::new(),
                                }]);
                            }
                        }

                        Some(t)
                    }
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
                    self.define_identifier(
                        name,
                        final_ty.clone(),
                        *mutability,
                        node.span,
                        init_state,
                    );

                    if let Some(init) = init {
                        init.ty = Some(final_ty.clone());
                    }
                    Ok(final_ty)
                } else {
                    Err(vec![Diagnostic {
                        path: self.path.clone(),
                        primary_err: "type must be defined at declaration".to_string(),
                        primary_span: node.span,
                        secondary_messages: Vec::new(),
                    }])
                }
            }
            NodeKind::IfCondition {
                condition,
                then_body,
                else_body,
            } => {
                let mut errors = Vec::new();
                match self.analyze_node(condition) {
                    Ok(ty) => {
                        if ty != Type::Bool {
                            errors.push(Diagnostic {
                                path: self.path.clone(),
                                primary_err: format!("expected `bool`, found `{ty}`"),
                                primary_span: condition.span,
                                secondary_messages: Vec::new(),
                            });
                        }
                    }
                    Err(err) => errors.extend(err),
                }

                let mut then_ty = None;

                self.scope.push(Scope::new(ScopeContext::Conditional));
                for node in &mut *then_body {
                    then_ty = match self.analyze_node(node) {
                        Ok(ty) => {
                            node.ty = Some(ty.clone());
                            Some(ty)
                        },
                        Err(err) => {
                            errors.extend(err);
                            None
                        }
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
                                Ok(ty) => {
                                    node.ty = Some(ty.clone());
                                    Some(ty)
                                },
                                Err(err) => {
                                    errors.extend(err);
                                    None
                                }
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
                                    Some(then_body_span),
                                )],
                            });
                        }
                    }
                    None => {
                        if then_ty != Type::Unit {
                            errors.push(Diagnostic {
                                path: self.path.clone(),
                                primary_err: format!(
                                    "missing `else` clause that evaluates to `{then_ty}`"
                                ),
                                primary_span: node.span,
                                secondary_messages: Vec::new(),
                            });
                        }
                    }
                }

                if errors.is_empty() {
                    Ok(then_ty)
                } else {
                    Err(errors)
                }
            }
            NodeKind::WhileLoop { condition, body } => {
                let mut errors = Vec::new();
                match self.analyze_node(condition) {
                    Ok(ty) => {
                        if ty != Type::Bool {
                            errors.push(Diagnostic {
                                path: self.path.clone(),
                                primary_err: format!("expected `bool`, found `{ty}`"),
                                primary_span: condition.span,
                                secondary_messages: Vec::new(),
                            });
                        }
                    }
                    Err(err) => errors.extend(err),
                }

                self.scope.push(Scope::new(ScopeContext::Conditional));
                for node in body {
                    match self.analyze_node(node) {
                        Ok(ty) => node.ty = Some(ty),
                        Err(err) => errors.extend(err),
                    }
                }
                self.scope.pop();

                if errors.is_empty() {
                    Ok(Type::Unit)
                } else {
                    Err(errors)
                }
            }
            NodeKind::FunctionDef {
                params,
                body,
                ty_cache,
                errored,
                ..
            } => {
                if !*errored {
                    let mut errors = Vec::new();
                    self.scope.push(Scope::new(ScopeContext::Function));

                    for param in params {
                        self.define_identifier(
                            &param.name,
                            param.ty_cache.clone().unwrap(),
                            false,
                            param.span,
                            InitState::Definitely,
                        );
                    }

                    let mut body_ty = Type::Unit;
                    for node in body {
                        body_ty = match self.analyze_node(node) {
                            Ok(ty) => {
                                node.ty = Some(ty.clone());
                                ty
                            },
                            Err(err) => {
                                errors.extend(err);
                                body_ty
                            }
                        };
                    }
                    self.scope.pop();

                    if !errors.is_empty() {
                        return Err(errors);
                    }

                    let ret = ty_cache.as_ref().unwrap();

                    if *ret != body_ty {
                        errors.push(Diagnostic {
                            path: self.path.clone(),
                            primary_err: format!(
                                "function defined with return type `{ret}` but returns `{body_ty}`"
                            ),
                            primary_span: node.span,
                            secondary_messages: Vec::new(),
                        });
                    }
                    if !errors.is_empty() {
                        return Err(errors);
                    }
                }

                Ok(Type::Unit)
            }
            NodeKind::FunctionCall { callee, args } => {
                let mut errors = Vec::new();

                let mut arg_tys = Vec::new();

                for arg in &mut *args {
                    match self.analyze_node(arg) {
                        Ok(expr) => {
                            arg.ty = Some(expr.clone());
                            arg_tys.push(expr)
                        },
                        Err(e) => errors.extend(e),
                    }
                }

                match self.find_identifier(callee, node.span) {
                    Ok(func) => {
                        match &func.ty {
                            Type::Function(param_tys, return_ty) => {
                                if !errors.is_empty() {
                                    return Err(errors);
                                }

                                let found_len = arg_tys.len();
                                let expected_len = param_tys.len();

                                if found_len != expected_len {
                                    errors.push(Diagnostic {
                                        path: self.path.clone(),
                                        primary_err: format!("`{callee}` expects {expected_len} arguments but only found {found_len} arguments"),
                                        primary_span: node.span,
                                        secondary_messages: Vec::new(),
                                    })
                                }

                                if !errors.is_empty() {
                                    return Err(errors);
                                }

                                for (arg_idx, (a, p)) in arg_tys.iter().zip(param_tys).enumerate() {
                                    if *a != *p {
                                        errors.push(Diagnostic {
                                            path: self.path.clone(),
                                            primary_err: format!("`{callee}`'s parameter #{} expects `{p}` but found `{a}`", arg_idx + 1),
                                            primary_span: args[arg_idx].span,
                                            secondary_messages: Vec::new(),
                                        })
                                    }
                                }

                                if !errors.is_empty() {
                                    return Err(errors);
                                }

                                return Ok(*return_ty.clone());
                            }
                            ty => errors.push(Diagnostic {
                                path: self.path.clone(),
                                primary_err: format!("expected function type, found `{ty}`"),
                                primary_span: node.span,
                                secondary_messages: Vec::new(),
                            }),
                        }
                    }
                    Err(err) => {
                        errors.push(err);
                    }
                }

                return Err(errors);
            },
            NodeKind::Array(items) => {
                let mut infer_spans = Vec::new();
                let mut item_type = None;
                for item in &mut *items {
                    let ty = self.analyze_node(item)?;
                    item.ty = Some(ty.clone());
                    if let Some(item_ty) = item_type {
                        if ty != item_ty {
                            let mut secondary_messages: Vec<(Option<String>, Option<Span>)> = infer_spans.iter().map(|&x| (None, Some(x))).collect();
                            secondary_messages[0].0 = Some(format!("{}: type `{item_ty}` inferred here:", "note".bright_blue().bold()));
                            return Err(vec![Diagnostic {
                                path: self.path.clone(),
                                primary_err: format!("mismatched types `{item_ty}` and `{ty}`"),
                                primary_span: item.span,
                                secondary_messages
                            }]);
                        }
                    }
                    item_type = Some(ty);
                    infer_spans.push(item.span);
                }

                Ok(Type::Array(Box::new(item_type.unwrap_or(Type::Unknown)), Some(items.len())))
            },
            NodeKind::Index {
                collection,
                index,
            } => {
                let mut errors = Vec::new();
                let collection_ty = self.analyze_node(collection)?;
                let ty = self.analyze_node(index)
                    .map_err(|err| errors.extend(err))
                    .ok();
                if let Some(ty) = &ty {
                    if *ty != Type::UInt {
                        errors.push(Diagnostic {
                            path: self.path.clone(),
                            primary_err: format!("expected uint, found `{ty}`"),
                            primary_span: index.span,
                            secondary_messages: Vec::new(),
                        });
                    }
                }
                match collection_ty {
                    Type::Array(inner, _) => {
                        if let Some(ty) = ty {
                            if ty == Type::UInt {
                                return Ok(*inner);
                            }
                        }
                    },
                    _ => errors.push(Diagnostic {
                        path: self.path.clone(),
                        primary_err: "can only index into collections".to_string(),
                        primary_span: collection.span,
                        secondary_messages: Vec::new(),
                    }),
                }

                return Err(errors);
            },
            NodeKind::StructDef { .. } => Ok(Type::Unit),
            NodeKind::FieldAccess { object, field, id } => {
                let object_ty = self.analyze_node(object)?;
                match &object_ty {
                    Type::Struct(sid, _) => {
                        *id = Some(*sid);
                        let data = self.structs[sid].as_ref().unwrap();
                        let mut candidate = None;
                        let mut candidate_score = 0.0;
                        for (name, ty) in &data.fields {
                            if *name == field.0 {
                                return Ok(ty.clone());
                            }

                            candidate_score = jaro_winkler(name, &field.0);
                            candidate = Some(name);
                        }

                        if candidate_score > CANDIDATE_SCORE_THRESHOLD {
                            return Err(vec![Diagnostic {
                                path: self.path.clone(),
                                primary_err: format!("cannot find field `{}` in object of type `{object_ty}`", field.0),
                                primary_span: field.1,
                                secondary_messages: vec![(
                                    Some(format!("{}: did you mean `{}`", "help".bright_blue().bold(), candidate.unwrap())),
                                    None
                                ), (
                                    Some(format!("{}: `{object_ty}` has fields:\n{}",
                                        "note".bright_blue().bold(),
                                        data.fields.iter()
                                            .map(|(name, ty)| format!("\t+ {name}: {ty}"))
                                            .collect::<Vec<_>>().join("\n")
                                    )),
                                    None
                                )],
                            }]);
                        }

                        Err(vec![Diagnostic {
                            path: self.path.clone(),
                            primary_err: format!("cannot find field `{}` in object of type `{object_ty}`", field.0),
                            primary_span: field.1,
                            secondary_messages: vec![(
                                Some(format!("{}: did you mean `{}`", "help".bright_blue().bold(), candidate.unwrap())),
                                None
                            ), (
                                Some(format!("{}: `{object_ty}` has fields:\n{}",
                                    "note".bright_blue().bold(),
                                    data.fields.iter()
                                        .map(|(name, ty)| format!("\t+ {name}: {ty}"))
                                        .collect::<Vec<_>>().join("\n")
                                )),
                                None
                            )],
                        }])
                    },
                    _ => Err(vec![Diagnostic {
                        path: self.path.clone(),
                        primary_err: format!("expected object type, found `{object_ty}`"),
                        primary_span: object.span,
                        secondary_messages: Vec::new()
                    }])
                }
            }
        }
    }
}