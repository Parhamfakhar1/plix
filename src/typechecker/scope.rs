#![allow(dead_code)]

use std::collections::HashMap;
use std::rc::Rc;
use crate::utils::position::Span;
use crate::parser::ast::{Expression, Type as AstType};

#[derive(Debug, Clone, PartialEq)]
pub enum ScopeKind {
    Global,
    Function,
    Block,
    Loop,
    If,
    Match,
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub kind: ScopeKind,
    pub parent: Option<Rc<Scope>>,
    pub variables: HashMap<String, VariableInfo>,
    pub functions: HashMap<String, FunctionInfo>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableInfo {
    pub name: String,
    pub type_: Type,
    pub mutable: bool,
    pub defined_in: ScopeKind,
    pub span: Span,
    pub used: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionInfo {
    pub name: String,
    pub parameters: Vec<ParameterInfo>,
    pub return_type: Type,
    pub defined_in: ScopeKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParameterInfo {
    pub name: String,
    pub type_: Type,
    pub default_value: Option<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Number,
    String,
    Boolean,
    Void,
    Null,
    Undefined,
    Array(Box<Type>),
    Object(HashMap<String, Type>),
    Function(Vec<Type>, Box<Type>),
    Union(Vec<Type>),
    Any,
    Custom(String),
}

impl Scope {
    pub fn new(kind: ScopeKind, parent: Option<Rc<Scope>>, span: Span) -> Self {
        Self {
            kind,
            parent,
            variables: HashMap::new(),
            functions: HashMap::new(),
            span,
        }
    }

    pub fn define_variable(&mut self, name: String, type_: Type, mutable: bool, span: Span) -> Result<(), ScopeError> {
        if self.variables.contains_key(&name) {
            return Err(ScopeError::VariableAlreadyDefined { name: name.clone(), span });
        }

        self.variables.insert(name.clone(), VariableInfo {
            name,
            type_,
            mutable,
            defined_in: self.kind.clone(),
            span,
            used: false,
        });

        Ok(())
    }

    pub fn define_function(&mut self, name: String, parameters: Vec<ParameterInfo>, return_type: Type, span: Span) -> Result<(), ScopeError> {
        if self.functions.contains_key(&name) {
            return Err(ScopeError::FunctionAlreadyDefined { name: name.clone(), span });
        }

        self.functions.insert(name.clone(), FunctionInfo {
            name,
            parameters,
            return_type,
            defined_in: self.kind.clone(),
            span,
        });

        Ok(())
    }

    pub fn lookup_variable(&self, name: &str) -> Option<VariableInfo> {
        if let Some(var_info) = self.variables.get(name) {
            Some(var_info.clone())
        } else if let Some(parent) = &self.parent {
            parent.lookup_variable(name)
        } else {
            None
        }
    }

    #[allow(dead_code)]
    pub fn lookup_function(&self, name: &str) -> Option<FunctionInfo> {
        if let Some(func_info) = self.functions.get(name) {
            Some(func_info.clone())
        } else if let Some(parent) = &self.parent {
            parent.lookup_function(name)
        } else {
            None
        }
    }

    #[allow(dead_code)]
    pub fn mark_variable_used(&mut self, name: &str) -> Result<(), ScopeError> {
        if let Some(var_info) = self.variables.get_mut(name) {
            var_info.used = true;
            Ok(())
        } else if let Some(_parent) = &self.parent {
            // We can't mutate through Rc directly, so we need to return an error
            // indicating the variable wasn't found in this scope
            Err(ScopeError::VariableNotFound { name: name.to_string() })
        } else {
            Err(ScopeError::VariableNotFound { name: name.to_string() })
        }
    }

    #[allow(dead_code)]
    pub fn get_all_variables(&self) -> Vec<VariableInfo> {
        let mut variables = self.variables.values().cloned().collect::<Vec<_>>();
        
        if let Some(parent) = &self.parent {
            variables.extend(parent.get_all_variables());
        }
        
        variables
    }

    #[allow(dead_code)]
    pub fn get_unused_variables(&self) -> Vec<VariableInfo> {
        self.get_all_variables()
            .into_iter()
            .filter(|var| !var.used && !matches!(var.defined_in, ScopeKind::Global))
            .collect()
    }

    pub fn enter_scope(&self, kind: ScopeKind, span: Span) -> Rc<Scope> {
        Rc::new(Scope::new(kind, Some(Rc::new(self.clone())), span))
    }

    #[allow(dead_code)]
    pub fn is_global(&self) -> bool {
        matches!(self.kind, ScopeKind::Global)
    }
}

#[derive(Debug, thiserror::Error, Clone)]
pub enum ScopeError {
    #[error("Variable '{name}' already defined at {span}")]
    VariableAlreadyDefined { name: String, span: Span },
    
    #[error("Function '{name}' already defined at {span}")]
    FunctionAlreadyDefined { name: String, span: Span },
    
    #[error("Variable '{name}' not found")]
    VariableNotFound { name: String },
    
    #[error("Function '{name}' not found")]
    FunctionNotFound { name: String },
    
    #[error("Type mismatch: expected {expected}, found {found}")]
    TypeMismatch { expected: String, found: String, span: Span },
    
    #[error("Cannot assign to immutable variable '{name}' at {span}")]
    ImmutableAssignment { name: String, span: Span },
    
    #[error("Variable '{name}' used before definition")]
    UseBeforeDefinition { name: String, span: Span },
}

impl Type {
    pub fn is_compatible_with(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Any, _) | (_, Type::Any) => true,
            (Type::Void, Type::Void) => true,
            (Type::Null, Type::Null) => true,
            (Type::Undefined, Type::Undefined) => true,
            (Type::Number, Type::Number) => true,
            (Type::String, Type::String) => true,
            (Type::Boolean, Type::Boolean) => true,
            (Type::Array(inner1), Type::Array(inner2)) => inner1.is_compatible_with(inner2),
            (Type::Object(fields1), Type::Object(fields2)) => {
                for (name, type1) in fields1 {
                    if let Some(type2) = fields2.get(name) {
                        if !type1.is_compatible_with(type2) {
                            return false;
                        }
                    } else {
                        return false;
                    }
                }
                true
            },
            (Type::Function(params1, return1), Type::Function(params2, return2)) => {
                if params1.len() != params2.len() {
                    return false;
                }
                
                for (param1, param2) in params1.iter().zip(params2.iter()) {
                    if !param1.is_compatible_with(param2) {
                        return false;
                    }
                }
                
                return1.is_compatible_with(return2)
            },
            (Type::Union(types1), Type::Union(types2)) => {
                for type1 in types1 {
                    if !types2.iter().any(|type2| type1.is_compatible_with(type2)) {
                        return false;
                    }
                }
                true
            },
            (Type::Union(types), other) => {
                types.iter().any(|t| t.is_compatible_with(other))
            },
            (other, Type::Union(types)) => {
                types.iter().any(|t| other.is_compatible_with(t))
            },
            _ => false,
        }
    }

    pub fn is_primitive(&self) -> bool {
        matches!(self, Type::Number | Type::String | Type::Boolean | Type::Void | Type::Null | Type::Undefined)
    }

    pub fn is_numeric(&self) -> bool {
        matches!(self, Type::Number)
    }

    pub fn is_boolean(&self) -> bool {
        matches!(self, Type::Boolean)
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Type::String)
    }

    pub fn is_void(&self) -> bool {
        matches!(self, Type::Void)
    }

    pub fn to_string(&self) -> String {
        match self {
            Type::Number => "number".to_string(),
            Type::String => "string".to_string(),
            Type::Boolean => "boolean".to_string(),
            Type::Void => "void".to_string(),
            Type::Null => "null".to_string(),
            Type::Undefined => "undefined".to_string(),
            Type::Array(inner) => format!("{}[]", inner.to_string()),
            Type::Object(fields) => {
                let fields_str: Vec<String> = fields
                    .iter()
                    .map(|(name, ty)| format!("{}: {}", name, ty.to_string()))
                    .collect();
                format!("{{ {} }}", fields_str.join(", "))
            },
            Type::Function(params, return_type) => {
                let params_str: Vec<String> = params
                    .iter()
                    .map(|p| p.to_string())
                    .collect();
                format!("fn({}) -> {}", params_str.join(", "), return_type.to_string())
            },
            Type::Union(types) => {
                let types_str: Vec<String> = types
                    .iter()
                    .map(|t| t.to_string())
                    .collect();
                format!("({})", types_str.join(" | "))
            },
            Type::Any => "any".to_string(),
            Type::Custom(name) => name.clone(),
        }
    }
}

impl From<AstType> for Type {
    fn from(ast_type: AstType) -> Self {
        match ast_type {
            AstType::Number => Type::Number,
            AstType::String => Type::String,
            AstType::Boolean => Type::Boolean,
            AstType::Void => Type::Void,
            AstType::Null => Type::Null,
            AstType::Undefined => Type::Undefined,
            AstType::Array(inner) => Type::Array(Box::new((*inner).into())),
            AstType::Object(fields) => {
                let mut field_map = HashMap::new();
                for (name, ty) in fields {
                    field_map.insert(name, ty.into());
                }
                Type::Object(field_map)
            },
            AstType::Tuple(types) => {
                let type_vec: Vec<Type> = types.into_iter().map(|t| t.into()).collect();
                Type::Union(type_vec)
            },
            AstType::Optional(inner) => {
                let inner_type = (*inner).into();
                Type::Union(vec![inner_type, Type::Null])
            },
            AstType::Result(ok, err) => {
                let ok_type = (*ok).into();
                let err_type = (*err).into();
                Type::Union(vec![ok_type, err_type])
            },
            AstType::Custom(name) => Type::Custom(name),
            AstType::Enum { name, generics } => Type::Custom(name), // For now, just use Custom type
        }
    }
}
