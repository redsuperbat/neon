use crate::location::Location;

#[derive(Debug)]
pub enum DiagnosticKind {
    Error,
}

#[derive(Debug)]
pub struct Diagnostic {
    kind: DiagnosticKind,
    loc: Location,
}

impl Diagnostic {
    pub fn new(kind: DiagnosticKind, loc: Location) -> Diagnostic {
        Diagnostic { kind, loc }
    }
}

#[derive(Debug)]
pub struct DiagnosticsList {
    diagnostics: Vec<Diagnostic>,
}

impl DiagnosticsList {
    pub fn add(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic)
    }
}
