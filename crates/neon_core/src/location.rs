#[derive(Debug, Clone, Copy)]
pub struct Pos(pub usize, pub usize);

impl Pos {
    pub fn start() -> Pos {
        Pos(0, 0)
    }

    pub fn line(&self) -> usize {
        return self.0;
    }

    pub fn column(&self) -> usize {
        return self.1;
    }
}

impl Into<Pos> for &Pos {
    fn into(self) -> Pos {
        *self
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Location {
    pub start: Pos,
    pub end: Pos,
}

impl Location {
    pub fn new<T: Into<Pos>>(start: T, end: T) -> Location {
        Location {
            start: start.into(),
            end: end.into(),
        }
    }
}
