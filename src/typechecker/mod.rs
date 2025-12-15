pub mod scope;
pub mod type_checker;
pub mod types;
pub mod use_def;
pub mod smart_mutability;

#[allow(unused_imports)]
pub use self::scope::{Scope, ScopeKind, Type, VariableInfo, FunctionInfo, ParameterInfo, ScopeError};
#[allow(unused_imports)]
pub use self::type_checker::{TypeChecker, TypeCheckError, TypeCheckResult};
#[allow(unused_imports)]
pub use self::types::{TypeEnvironment, TypeEnvironmentError};
#[allow(unused_imports)]
pub use self::use_def::{UseDefAnalysis, DefinitionKind};
#[allow(unused_imports)]
pub use self::smart_mutability::{SmartMutabilityChecker, Mutability, VariableState, RegionGraph, Region};
