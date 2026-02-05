use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int, Float, Unit,
    
    /// for a variable's type that is not defined or inferrable at declaration
    Unknown,
}

impl Type {
    pub fn is_numeric(&self) -> bool {
        matches!(self, Self::Int | Self::Float)
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