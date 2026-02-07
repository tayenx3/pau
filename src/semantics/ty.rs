use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int, Float, Unit,
    Unknown,
}

impl Type {
    pub fn is_numeric(&self) -> bool {
        matches!(self, Self::Int | Self::Float)
    }

    pub fn to_clif_ty(&self) -> cranelift::prelude::Type {
        match self {
            Self::Int | Self::Unit => cranelift::prelude::types::I64,
            Self::Float => cranelift::prelude::types::F64,
            _ => unreachable!("unresolved type")
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int => write!(f, "int"),
            Self::Float => write!(f, "float"),
            Self::Unit => write!(f, "()"),
            Self::Unknown => write!(f, "<unknown>"),
        }
    }
}