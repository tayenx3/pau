use crate::semantics::ty::Type;
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Operator {
    Walrus,
    Plus, Minus, Star, Slash, Modulo,
}

impl Operator {
    pub fn prec(&self) -> (usize, usize) {
        match self {
            Self::Walrus => (11, 10),
            Self::Plus | Self::Minus => (20, 21),
            Self::Star | Self::Slash | Self::Modulo => (30, 31),
        }
    }

    pub fn is_infix(&self) -> bool {
        ![].contains(self) // any purely prefix op will go here, but we don't have them yet
    }

    pub fn is_prefix(&self) -> bool {
        [Self::Plus, Self::Minus].contains(self)
    }

    pub fn infix_output_ty(&self, lhs: &Type, rhs: &Type) -> Option<Type> {
        if lhs != rhs { return None }

        match self {
            Self::Plus | Self::Minus | Self::Star
            | Self::Slash | Self::Modulo => if lhs.is_numeric() {
                Some(lhs.clone())
            } else {
                None
            },
            _ => None,
        }
    }

    pub fn prefix_output_ty(&self, operand: &Type) -> Option<Type> {
        match self {
            Self::Plus | Self::Minus => operand.is_numeric().then(|| operand.clone()),
            _ => None,
        }
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Walrus => write!(f, ":="),
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Star => write!(f, "*"),
            Self::Slash => write!(f, "/"),
            Self::Modulo => write!(f, "%"),
        }
    }
}