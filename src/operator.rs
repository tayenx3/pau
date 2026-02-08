use crate::semantics::ty::Type;
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Operator {
    Walrus,
    Plus, Minus, Star, Slash, Modulo,
    Eq, Ne, Gt, Lt, Ge, Le,
    Bang,
}

impl Operator {
    pub fn prec(&self) -> (usize, usize) {
        match self {
            Self::Walrus => (11, 10),
            Self::Eq | Self::Ne => (20, 21),
            Self::Gt | Self::Lt | Self::Ge | Self::Le => (30, 31),
            Self::Plus | Self::Minus => (40, 41),
            Self::Star | Self::Slash | Self::Modulo => (50, 51),
            _ => (0, 0), // prefix only ops
        }
    }

    pub fn is_infix(&self) -> bool {
        ![Self::Bang].contains(self)
    }

    pub fn is_prefix(&self) -> bool {
        [Self::Plus, Self::Minus, Self::Bang].contains(self)
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
            Self::Eq | Self::Ne => Some(Type::Bool),
            Self::Gt | Self::Lt | Self::Ge
            | Self::Le => if lhs.is_numeric() {
                Some(Type::Bool)
            } else {
                None
            },
            _ => None,
        }
    }

    pub fn prefix_output_ty(&self, operand: &Type) -> Option<Type> {
        match self {
            Self::Plus => operand.is_numeric().then(|| operand.clone()),
            Self::Minus => ((operand.is_int() && operand.is_signed()) || operand.is_float()).then(|| operand.clone()),
            Self::Bang => (operand.is_int() || *operand == Type::Bool).then(|| operand.clone()),
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
            Self::Eq => write!(f, "=="),
            Self::Ne => write!(f, "!="),
            Self::Gt => write!(f, ">"),
            Self::Lt => write!(f, "<"),
            Self::Ge => write!(f, ">="),
            Self::Le => write!(f, "<="),
            Self::Bang => write!(f, "!"),
        }
    }
}