use std::fmt;
use cranelift::prelude::types;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int, UInt, Float, Bool,
    I8, I16, I32, I64,
    U8, U16, U32, U64,
    F32, F64,
    Function(Vec<Type>, Box<Type>),
    Unit,
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
            Self::Int | Self::UInt => match size_of::<usize>() {
                4 => types::I32,
                8 => types::I64,
                _ => unreachable!()
            },
            Self::Float => match size_of::<usize>() {
                4 => types::F32,
                8 => types::F64,
                _ => unreachable!()
            },
            Self::I8 | Self::U8 | Self::Unit | Self::Bool => types::I8,
            Self::I16 | Self::U16 => types::I16,
            Self::I32 | Self::U32 => types::I32,
            Self::I64 | Self::U64 => types::I64,
            Self::F32 => types::F32,
            Self::F64 => types::F64,
            Self::Function(_, _) => todo!("Function types"),
        }
    }

    pub fn size(&self) -> u32 {
        match self {
            Self::Int | Self::UInt | Self::Float => size_of::<usize>() as u32,
            Self::I8 | Self::U8 | Self::Unit | Self::Bool => 1,
            Self::I16 | Self::U16 => 2,
            Self::I32 | Self::U32 | Self::F32 => 4,
            Self::I64 | Self::U64 | Self::F64 => 8,
            Self::Function(_, _) => todo!("Function types"),
        }
    }

    pub fn align(&self) -> u8 {
        // specially aligned types will be made later
        match self {
            _ => self.size() as u8,
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
                    acc.push_str(&format!("{}", param));

                    if idx < params.len() - 1 {
                        acc.push_str(", ");
                    }
                }
                
                write!(f, "def({acc}): {return_ty}")
            },
        }
    }
}