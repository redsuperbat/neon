use crate::{location::Location, type_checker::Type};

#[derive(Debug)]
pub struct UnassignableTypeError {
    pub loc: Location,
    pub lhs: Type,
    pub rhs: Type,
}

#[derive(Debug)]
pub enum ErrorDiagnostic {
    UnassignableType(UnassignableTypeError),
}

impl ErrorDiagnostic {
    pub fn message(&self) -> String {
        match self {
            ErrorDiagnostic::UnassignableType(e) => {
                format!("Type {} is not assignable to type {}", e.lhs, e.rhs)
            }
        }
    }
    pub fn loc(&self) -> Location {
        match self {
            ErrorDiagnostic::UnassignableType(e) => e.loc,
        }
    }
}

#[derive(Debug)]
pub enum Diagnostic {
    Error(ErrorDiagnostic),
}

#[derive(Debug)]
pub struct DiagnosticsList {
    pub diagnostics: Vec<Diagnostic>,
}

impl DiagnosticsList {
    pub fn new() -> DiagnosticsList {
        DiagnosticsList {
            diagnostics: vec![],
        }
    }

    pub fn add(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic)
    }

    pub fn has_errors(&self) -> bool {
        self.diagnostics.len() > 0
    }
}
