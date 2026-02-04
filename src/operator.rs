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
        ![].contains(self)
    }

    pub fn is_prefix(&self) -> bool {
        [Self::Plus, Self::Minus].contains(self)
    }
}