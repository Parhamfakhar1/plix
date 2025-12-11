pub mod scope;
pub mod type_checker;
pub mod types;
pub mod use_def;

pub use self::scope::{Scope, ScopeKind, Type, VariableInfo, FunctionInfo, ParameterInfo, ScopeError};
pub use self::type_checker::{TypeChecker, TypeCheckError, TypeCheckResult};
pub use self::types::{TypeEnvironment, TypeEnvironmentError};
pub use self::use_def::{UseDefAnalysis, DefinitionKind};
