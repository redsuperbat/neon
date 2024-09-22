use crate::{location::Location, type_checker::Type};
use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct UnassignableTypeError {
    pub loc: Location,
    pub lhs: Type,
    pub rhs: Type,
}

#[derive(Debug, Clone)]
pub struct UndefinedReferenceError {
    pub loc: Location,
    pub name: String,
}

#[derive(Debug, Clone)]
pub enum ErrorDiagnostic {
    UnassignableType(UnassignableTypeError),
    UndefinedReference(UndefinedReferenceError),
}

impl ToString for ErrorDiagnostic {
    fn to_string(&self) -> String {
        match self {
            ErrorDiagnostic::UnassignableType(e) => {
                format!("Type {} is not assignable to type {}", e.rhs, e.lhs)
            }
            ErrorDiagnostic::UndefinedReference(e) => format!("Undefined reference '{}'", e.name),
        }
    }
}

impl ErrorDiagnostic {
    pub fn loc(&self) -> Location {
        match self {
            ErrorDiagnostic::UnassignableType(e) => e.loc,
            ErrorDiagnostic::UndefinedReference(e) => e.loc,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Diagnostic {
    Error(ErrorDiagnostic),
}

impl Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Diagnostic::Error(error_diagnostic) => {
                write!(f, "ERROR: {}", error_diagnostic.to_string())
            }
        }
    }
}

#[derive(Debug, Clone)]
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
        !self.diagnostics.is_empty()
    }

    pub fn clear(&mut self) {
        self.diagnostics.clear()
    }

    pub fn merge(&self, list: &mut DiagnosticsList) -> DiagnosticsList {
        let mut diagnostics = self.diagnostics.clone();
        diagnostics.append(&mut list.diagnostics);
        DiagnosticsList { diagnostics }
    }
}
