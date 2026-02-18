use std::{collections::HashMap, fmt};
use cranelift::prelude::types;
use crate::{diag::Diagnostic, semantics::StructData, span::Span};

use super::StructID;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int, UInt, Float, Bool, Unknown,
    I8, I16, I32, I64,
    U8, U16, U32, U64,
    F32, F64,
    Function(Vec<Type>, Box<Type>),
    Array(Box<Type>, Option<usize>),
    Unit,
    Struct(StructID, String),
}

impl Type {
    pub fn is_numeric(&self) -> bool {
        matches!(
            self,
            Self::Int | Self::Float | Self::UInt | Self::I8
            | Self::I16 | Self::I32 | Self::I64 | Self::U8
            | Self::U16 | Self::U32 | Self::U64 | Self::F32
            | Self::F64
        )
    }

    pub fn is_int(&self) -> bool {
        matches!(
            self,
            Self::Int | Self::UInt | Self::I8 | Self::I16
            | Self::I32 | Self::I64 | Self::U8 | Self::U16
            | Self::U32 | Self::U64
        )
    }

    pub fn is_signed(&self) -> bool {
        matches!(
            self,
            Self::Int | Self::I8 | Self::I16
            | Self::I32 | Self::I64
        )
    }

    pub fn is_unsigned(&self) -> bool {
        matches!(
            self,
            Self::UInt | Self::U8 | Self::U16
            | Self::U32 | Self::U64
        )
    }

    pub fn is_float(&self) -> bool {
        matches!(
            self,
            Self::Float | Self::F32 | Self::F64
        )
    }

    pub fn to_clif_ty(&self) -> cranelift::prelude::Type {
        match self {
            Self::Int | Self::UInt | Self::Function(_, _)
            | Self::Array(_, _)  | Self::Struct(_, _) => match size_of::<usize>() {
                4 => types::I32,
                8 => types::I64,
                _ => unreachable!()
            },
            Self::Float => match size_of::<usize>() {
                4 => types::F32,
                8 => types::F64,
                _ => unreachable!()
            },
            Self::I8 | Self::U8 | Self::Unit | Self::Unknown | Self::Bool => types::I8,
            Self::I16 | Self::U16 => types::I16,
            Self::I32 | Self::U32 => types::I32,
            Self::I64 | Self::U64 => types::I64,
            Self::F32 => types::F32,
            Self::F64 => types::F64,
        }
    }

    pub fn size(&self, structs: &HashMap<StructID, StructData>, path: &str, span: Span) -> Result<u32, Diagnostic> {
        Ok(match self {
            Self::Int | Self::UInt | Self::Function(_, _) | Self::Float => size_of::<usize>() as u32,
            Self::I8 | Self::U8 | Self::Unit | Self::Unknown | Self::Bool => 1,
            Self::I16 | Self::U16 => 2,
            Self::I32 | Self::U32 | Self::F32 => 4,
            Self::I64 | Self::U64 | Self::F64 => 8,
            Self::Array(inner, size) => if let Some(size) = size {
                inner.size(structs, path, span)? * *size as u32
            } else {
                return Err(Diagnostic {
                    path: path.to_string(),
                    primary_err: format!("unsized type `{self}`"),
                    primary_span: span,
                    secondary_messages: Vec::new(),
                });
            },
            Self::Struct(id, _) => {
                let s = &structs[id];
                let mut total = 0;
                for (offset, (_, field, _)) in s.fields.iter().enumerate() {
                    total += field.physical_size();
                    let align = field.align(structs, path, span) as usize;
                    total += ((align - (offset % align)) % align) as u32;
                }
                total
            },
        })
    }

    pub fn physical_size(&self) -> u32 {
        match self {
            Self::Int | Self::UInt | Self::Function(_, _) | Self::Float => size_of::<usize>() as u32,
            Self::I8 | Self::U8 | Self::Unit | Self::Unknown | Self::Bool => 1,
            Self::I16 | Self::U16 => 2,
            Self::I32 | Self::U32 | Self::F32 => 4,
            Self::I64 | Self::U64 | Self::F64 => 8,
            Self::Array(_, _) | Self::Struct(_, _) => size_of::<usize>() as u32,
        }
    }

    pub fn align(&self, structs: &HashMap<StructID, StructData>, path: &str, span: Span) -> u8 {
        match self {
            Self::Array(inner, _) => inner.align(structs, path, span),
            Self::Struct(id, _) => {
                let data = &structs[id];
                data.fields.iter()
                    .map(|(_, ty, _)| ty.align(structs, path, span))
                    .max()
                    .unwrap_or(1)
            },
            _ => self.size(structs, path, span).unwrap() as u8,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int => write!(f, "int"),
            Self::UInt => write!(f, "uint"),
            Self::Float => write!(f, "float"),
            Self::Bool => write!(f, "bool"),
            Self::I8 => write!(f, "i8"),
            Self::I16 => write!(f, "i16"),
            Self::I32 => write!(f, "i32"),
            Self::I64 => write!(f, "i64"),
            Self::U8 => write!(f, "u8"),
            Self::U16 => write!(f, "u16"),
            Self::U32 => write!(f, "u32"),
            Self::U64 => write!(f, "u64"),
            Self::F32 => write!(f, "f32"),
            Self::F64 => write!(f, "f64"),
            Self::Unit => write!(f, "()"),
            Self::Function(params, return_ty) => {
                let mut acc = String::new();

                for (idx, param) in params.iter().enumerate() {
                    acc.push_str(&param.to_string());

                    if idx < params.len() - 1 {
                        acc.push_str(", ");
                    }
                }
                
                write!(f, "def({acc}): {return_ty}")
            },
            Self::Array(inner, size) => write!(
                f,
                "[{inner}{}]",
                size
                    .map(|size| format!("; {size}"))
                    .unwrap_or("".to_string())
            ),
            Self::Unknown => write!(f, "<unknown>"),
            Self::Struct(_, name) => write!(f, "{}", name),
        }
    }
}