use crate::{location::Location, type_checker::Type};

#[derive(Debug, Clone, PartialEq)]
pub enum DiagnosticKind {
    UnassignableType {
        lhs: Type,
        rhs: Type,
    },
    UndefinedReference {
        reference_type: String,
        name: String,
    },
    IncompatibleTypes {
        consequent: Type,
        alternate: Type,
    },
    ExpressionNotInvocable {
        callee_type: Type,
    },
    InsufficientOverlap,
    MissingProperty {
        missing_from: Type,
        property: String,
    },
    InvalidFnReturnType {
        expected: Type,
        found: Type,
    },
    MissingPropertyAccess {
        accessed_on: Type,
        property: String,
    },
    SuperfluousProperty {
        access_type: Type,
        key: String,
    },
    InvalidIndexAccess {
        indexee_type: Type,
    },
    InsufficientArguments {
        expected: usize,
        got: usize,
    },
    UndefinedType {
        name: String,
    },
    NotIterable {
        non_iterable: Type,
    },
    InvalidSyntax {
        message: String,
    },
    DuplicateDefinition {
        name: String,
        typeof_duplicate: String,
    },
    MismatchedTypes {
        found: Type,
        expected: Type,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum DiagnosticLevel {
    Error,
    Warning,
    Info,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Diagnostic {
    pub loc: Location,
    pub kind: DiagnosticKind,
    pub level: DiagnosticLevel,
}

impl ToString for Diagnostic {
    fn to_string(&self) -> String {
        let level_prefix = match self.level {
            DiagnosticLevel::Error => "ERROR",
            DiagnosticLevel::Warning => "WARNING",
            DiagnosticLevel::Info => "INFO",
        };

        let message = match &self.kind {
            DiagnosticKind::UnassignableType { lhs, rhs } => {
                format!("Type '{}' is not assignable to type '{}'.", rhs, lhs)
            }
            DiagnosticKind::UndefinedReference { reference_type, name } => {
                format!("Undefined {} reference '{}'.", reference_type, name)
            }
            DiagnosticKind::IncompatibleTypes { consequent, alternate } => {
                format!("If expression has incompatible arms, expected type '{}' found '{}'.", consequent, alternate)
            }
            DiagnosticKind::ExpressionNotInvocable { callee_type } => {
                format!("Expected function found '{}'", callee_type)
            }
            DiagnosticKind::InsufficientOverlap => {
                "Binary operation on types seems to be a mistake since none of them overlap sufficiently with each other.".to_string()
            }
            DiagnosticKind::MissingProperty { missing_from, property } => {
                format!("Property '{}' is missing, but required in type '{}'", property, missing_from)
            }
            DiagnosticKind::SuperfluousProperty { access_type, key } => {
                format!("Struct may only specify known properties, and '{}' does not exist in type '{}'", key, access_type)
            }
            DiagnosticKind::InvalidIndexAccess { indexee_type } => {
                format!("Cannot index into a value of type '{}'", indexee_type)
            }
            DiagnosticKind::InsufficientArguments { expected, got } => {
                format!("Expected '{}' arguments got '{}'.", expected, got)
            }
            DiagnosticKind::UndefinedType { name } => {
                format!("Type '{}' is not defined in current scope.", name)
            }
            DiagnosticKind::NotIterable { non_iterable } => {
                format!("Type '{}' is not iterable.", non_iterable)
            }
            DiagnosticKind::InvalidSyntax { message } => message.clone(),
            DiagnosticKind::DuplicateDefinition { name, typeof_duplicate } => {
                format!("'{}' '{}' defined multiple times in current scope.", typeof_duplicate, name)
            }
            DiagnosticKind::MismatchedTypes { found, expected } => {
                format!("Mismatched types, expected '{}', found '{}'", expected, found)
            }
            DiagnosticKind::MissingPropertyAccess { accessed_on, property } => {
                format!("Property '{}' does not exist in type '{}'.", property, accessed_on)
            }
            DiagnosticKind::InvalidFnReturnType { expected, found } => {
                format!("Invalid return type, expected '{}', found '{}'", expected, found)
            }
        };

        format!("{}: {}", level_prefix, message)
    }
}

pub trait ToDiagnostic {
    fn to_diagnostic(&self) -> Diagnostic;
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
        let are_equal = self
            .diagnostics
            .iter()
            .any(|d| d.loc == diagnostic.loc && d.to_string() == diagnostic.to_string());

        if are_equal {
            return;
        }
        self.diagnostics.push(diagnostic);
    }

    pub fn add_error(&mut self, diagnostic: DiagnosticKind, loc: Location) {
        self.add(Diagnostic {
            level: DiagnosticLevel::Error,
            loc,
            kind: diagnostic,
        })
    }

    pub fn has_errors(&self) -> bool {
        self.diagnostics
            .iter()
            .any(|d| d.level == DiagnosticLevel::Error)
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
        let mut diagnostics = DiagnosticsList {
            diagnostics: self.diagnostics.clone(),
        };
        for d in &list.diagnostics {
            diagnostics.add(d.clone());
        }
        return diagnostics;
    }
}
