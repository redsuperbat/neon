use crate::{location::Location, type_checker::Type};
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub struct UnassignableTypeError {
    pub loc: Location,
    pub lhs: Type,
    pub rhs: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UndefinedReferenceError {
    pub loc: Location,
    pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IncompatibleTypesError {
    pub loc: Location,
    pub consequent: Type,
    pub alternate: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionNotInvokableError {
    pub loc: Location,
    pub callee_type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InsufficientOverlapmentError {
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PropertyDoesNotExistError {
    pub loc: Location,
    pub access_type: Type,
    pub key: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InvalidIndexAccessError {
    pub loc: Location,
    pub index_type: Type,
    pub indexee_type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ErrorDiagnostic {
    UnassignableType(UnassignableTypeError),
    UndefinedReference(UndefinedReferenceError),
    IncompatibleTypes(IncompatibleTypesError),
    ExpressionNotInvokable(ExpressionNotInvokableError),
    InsufficientOverlapment(InsufficientOverlapmentError),
    PropertyDoesNotExist(PropertyDoesNotExistError),
    InvalidIndexAccess(InvalidIndexAccessError),
}

impl ToString for ErrorDiagnostic {
    fn to_string(&self) -> String {
        match self {
            ErrorDiagnostic::UnassignableType(e) => {
                format!("Type `{}` is not assignable to type `{}`.", e.rhs, e.lhs)
            }
            ErrorDiagnostic::UndefinedReference(e) => format!("Undefined reference `{}`.", e.name),
            ErrorDiagnostic::IncompatibleTypes(e) => format!(
                "If expression has incompatible arms, expected `{}` found `{}`.",
                e.consequent, e.alternate
            ),
            ErrorDiagnostic::ExpressionNotInvokable(e) => format!("Expression with type `{}` not invokable.", e.callee_type),
            ErrorDiagnostic::InsufficientOverlapment(_e) => "Binary operation on types seems to be a mistake since none of them overlap sufficiently with each other.".to_string(),
            ErrorDiagnostic::PropertyDoesNotExist(e) => format!("Property `{}` does not exist on type `{}`.", e.key, e.access_type),
            ErrorDiagnostic::InvalidIndexAccess(e) =>format!("Expression of type `{}` can't be used to index type `{}`.", e.index_type, e.indexee_type)
        }
    }
}

impl ErrorDiagnostic {
    pub fn loc(&self) -> Location {
        match self {
            ErrorDiagnostic::UnassignableType(e) => e.loc,
            ErrorDiagnostic::UndefinedReference(e) => e.loc,
            ErrorDiagnostic::IncompatibleTypes(e) => e.loc,
            ErrorDiagnostic::ExpressionNotInvokable(e) => e.loc,
            ErrorDiagnostic::InsufficientOverlapment(e) => e.loc,
            ErrorDiagnostic::PropertyDoesNotExist(e) => e.loc,
            ErrorDiagnostic::InvalidIndexAccess(e) => e.loc,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
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
        if self.diagnostics.iter().any(|d| *d == diagnostic) {
            return;
        }
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
