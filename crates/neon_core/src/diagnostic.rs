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
    pub reference_type: String,
    pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IncompatibleTypesError {
    pub loc: Location,
    pub consequent: Type,
    pub alternate: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionNotInvocableError {
    pub loc: Location,
    pub callee_type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InsufficientOverlapError {
    pub loc: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MissingPropertyError {
    pub loc: Location,
    pub access_type: Type,
    pub key: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SuperfluousPropertyError {
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
pub struct InsufficientArgumentsError {
    pub loc: Location,
    pub expected: usize,
    pub got: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UndefinedTypeError {
    pub loc: Location,
    pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NotIterableError {
    pub loc: Location,
    pub non_iterable: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InvalidSyntaxError {
    pub loc: Location,
    pub message: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DuplicateDefinitionError {
    pub loc: Location,
    pub name: String,
    pub typeof_duplicate: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ErrorDiagnostic {
    UnassignableType(UnassignableTypeError),
    NotIterable(NotIterableError),
    UndefinedReference(UndefinedReferenceError),
    IncompatibleTypes(IncompatibleTypesError),
    ExpressionNotInvocable(ExpressionNotInvocableError),
    InsufficientArguments(InsufficientArgumentsError),
    UndefinedType(UndefinedTypeError),
    InsufficientOverlap(InsufficientOverlapError),
    MissingProperty(MissingPropertyError),
    DuplicateDefinition(DuplicateDefinitionError),
    SuperfluousProperty(SuperfluousPropertyError),
    InvalidIndexAccess(InvalidIndexAccessError),
    InvalidSyntax(InvalidSyntaxError),
}

impl ToString for ErrorDiagnostic {
    fn to_string(&self) -> String {
        match self {
            ErrorDiagnostic::UnassignableType(e) => {
                format!("Type `{}` is not assignable to type `{}`.", e.rhs, e.lhs)
            }
            ErrorDiagnostic::UndefinedReference(e) => format!("Undefined `{}` reference `{}`.", e.reference_type, e.name),
            ErrorDiagnostic::IncompatibleTypes(e) => format!(
                "If expression has incompatible arms, expected `{}` found `{}`.",
                e.consequent, e.alternate
            ),
            ErrorDiagnostic::ExpressionNotInvocable(e) => format!("Expression with type `{}` is not invocable.", e.callee_type),
            ErrorDiagnostic::InsufficientOverlap(_e) => "Binary operation on types seems to be a mistake since none of them overlap sufficiently with each other.".to_string(),
            ErrorDiagnostic::MissingProperty(e) => format!("Property `{}` does not exist on type `{}`.", e.key, e.access_type),
            ErrorDiagnostic::SuperfluousProperty(e) =>  format!("Type `{}` does not define property `{}`.", e.access_type, e.key),
            ErrorDiagnostic::InvalidIndexAccess(e) => format!("Expression of type `{}` can't be used to index type `{}`.", e.index_type, e.indexee_type),
            ErrorDiagnostic::InsufficientArguments(e) => format!("Expected `{}` arguments got `{}`.", e.expected, e.got),
            ErrorDiagnostic::UndefinedType(e) => format!("Type `{}` is not defined in current scope.", e.name),
            ErrorDiagnostic::NotIterable(e) => format!("Type `{}` is not iterable.", e.non_iterable),
            ErrorDiagnostic::InvalidSyntax(e) => e.message.clone(),
            ErrorDiagnostic::DuplicateDefinition(e) => format!("`{}` `{}` defined multiple times in current scope.", e.typeof_duplicate, e.name),
        }
    }
}

pub trait ToDiagnostic {
    fn to_diagnostic(&self) -> Diagnostic;
}

impl ErrorDiagnostic {
    pub fn loc(&self) -> Location {
        match self {
            ErrorDiagnostic::UnassignableType(e) => e.loc,
            ErrorDiagnostic::UndefinedReference(e) => e.loc,
            ErrorDiagnostic::IncompatibleTypes(e) => e.loc,
            ErrorDiagnostic::ExpressionNotInvocable(e) => e.loc,
            ErrorDiagnostic::InsufficientOverlap(e) => e.loc,
            ErrorDiagnostic::MissingProperty(e) => e.loc,
            ErrorDiagnostic::InvalidIndexAccess(e) => e.loc,
            ErrorDiagnostic::InsufficientArguments(e) => e.loc,
            ErrorDiagnostic::UndefinedType(e) => e.loc,
            ErrorDiagnostic::NotIterable(e) => e.loc,
            ErrorDiagnostic::InvalidSyntax(e) => e.loc,
            ErrorDiagnostic::SuperfluousProperty(e) => e.loc,
            ErrorDiagnostic::DuplicateDefinition(e) => e.loc,
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

impl ToString for DiagnosticsList {
    fn to_string(&self) -> String {
        self.diagnostics
            .iter()
            .map(|d| d.to_string())
            .collect::<Vec<_>>()
            .join("\n")
    }
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
        self.diagnostics.push(diagnostic);
    }

    pub fn add_error(&mut self, error: ErrorDiagnostic) {
        self.add(Diagnostic::Error(error));
    }

    pub fn has_errors(&self) -> bool {
        !self.diagnostics.is_empty()
    }

    pub fn clear(&mut self) {
        self.diagnostics.clear()
    }

    pub fn consume(&mut self, list: &DiagnosticsList) {
        for d in &list.diagnostics {
            self.add(d.clone());
        }
    }

    pub fn merge(&self, list: &mut DiagnosticsList) -> DiagnosticsList {
        let mut diagnostics = self.diagnostics.clone();
        diagnostics.append(&mut list.diagnostics);
        DiagnosticsList { diagnostics }
    }
}
