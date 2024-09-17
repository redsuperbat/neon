use crate::location::Location;

#[derive(Debug)]
pub struct ErrorDiagnostic {
    pub loc: Location,
}

#[derive(Debug)]
pub enum Diagnostic {
    Error(ErrorDiagnostic),
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
