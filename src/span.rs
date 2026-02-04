#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span { pub start: usize, pub end: usize }

impl Span {
    pub fn splat_to_end(&self) -> Self {
        Self {
            start: self.end,
            end: self.end + 1,
        }
    }

    pub fn connect(&self, other: &Self) -> Self {
        Self {
            start: self.start,
            end: other.end,
        }
    }
}