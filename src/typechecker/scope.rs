use std::collections::HashMap;
use crate::utils::position::Span;
use crate::parser::ast::Type as AstType;

#[derive(Debug, Clone, PartialEq)]
pub enum ScopeKind {
    Global,
    Function,
    If,
    Loop,
    Block,
    Match,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableInfo {
    pub name: String,
    pub type_: Type,
    pub mutable: bool,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParameterInfo {
    pub name: String,
    pub type_: Type,
    pub default_value: Option<crate::parser::ast::Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionInfo {
    pub name: String,
    pub parameters: Vec<ParameterInfo>,
    pub return_type: Type,
    pub span: Span,
}

#[derive(Debug, thiserror::Error, Clone)]
pub enum ScopeError {
    #[error("Variable '{name}' already defined in scope")]
    VariableAlreadyDefined { name: String, span: Span },
    
    #[error("Function '{name}' already defined in scope")]
    FunctionAlreadyDefined { name: String, span: Span },
}

#[derive(Clone)]  // ✅ اضافه شد
pub struct Scope {
    pub kind: ScopeKind,
    pub parent: Option<std::rc::Rc<Scope>>,
    pub variables: HashMap<String, VariableInfo>,
    pub functions: HashMap<String, FunctionInfo>,
    pub types: HashMap<String, Type>,
    pub span: Span,
}

impl Scope {
    pub fn new(kind: ScopeKind, parent: Option<std::rc::Rc<Scope>>, span: Span) -> Self {
        Self {
            kind,
            parent,
            variables: HashMap::new(),
            functions: HashMap::new(),
            types: HashMap::new(),
            span,
        }
    }

    pub fn enter_scope(&self, kind: ScopeKind, span: Span) -> std::rc::Rc<Scope> {
        std::rc::Rc::new(Scope::new(kind, Some(self.clone_rc()), span))
    }

    fn clone_rc(&self) -> std::rc::Rc<Scope> {
        std::rc::Rc::new(Scope {
            kind: self.kind.clone(),
            parent: self.parent.clone(),
            variables: self.variables.clone(),
            functions: self.functions.clone(),
            types: self.types.clone(),
            span: self.span,
        })
    }

    pub fn define_variable(&mut self, name: String, type_: Type, mutable: bool, span: Span) -> Result<(), ScopeError> {
        if self.variables.contains_key(&name) {
            return Err(ScopeError::VariableAlreadyDefined { name, span });
        }

        // ✅ اصلاح: clone name قبل از move
        self.variables.insert(name.clone(), VariableInfo { name, type_, mutable, span });
        Ok(())
    }

    pub fn define_function(&mut self, name: String, parameters: Vec<ParameterInfo>, return_type: Type, span: Span) -> Result<(), ScopeError> {
        if self.functions.contains_key(&name) {
            return Err(ScopeError::FunctionAlreadyDefined { name, span });
        }

        // ✅ اصلاح: clone name قبل از move
        self.functions.insert(name.clone(), FunctionInfo { name, parameters, return_type, span });
        Ok(())
    }

    pub fn lookup_variable(&self, name: &str) -> Option<&VariableInfo> {
        if let Some(var) = self.variables.get(name) {
            Some(var)
        } else if let Some(parent) = &self.parent {
            parent.lookup_variable(name)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Number,
    String,
    Boolean,
    Void,
    Null,
    Undefined,
    Any,
    Unit,
    Array(Box<Type>),
    Object(HashMap<String, Type>),
    Tuple(Vec<Type>),
    Function(Vec<Type>, Box<Type>),
    Custom(String),
    Enum {
        name: String,
        generics: Vec<String>,
        variants: HashMap<String, Type>,
    },
    Result(Box<Type>, Box<Type>),
    Optional(Box<Type>),
    Union(Vec<Type>),  // ✅ اضافه شد
}

impl Type {
    pub fn is_numeric(&self) -> bool {
        matches!(self, Type::Number)
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Type::String)
    }

    pub fn is_boolean(&self) -> bool {
        matches!(self, Type::Boolean)
    }

    pub fn is_compatible_with(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Any, _) | (_, Type::Any) => true,
            (Type::Number, Type::Number) => true,
            (Type::String, Type::String) => true,
            (Type::Boolean, Type::Boolean) => true,
            (Type::Void, Type::Void) => true,
            (Type::Null, Type::Null) => true,
            (Type::Undefined, Type::Undefined) => true,
            (Type::Array(a), Type::Array(b)) => a.is_compatible_with(b),
            (Type::Object(a), Type::Object(b)) => {
                // For now, just check that they have the same keys
                a.keys().eq(b.keys()) && a.values().zip(b.values()).all(|(a, b)| a.is_compatible_with(b))
            },
            (Type::Tuple(a), Type::Tuple(b)) => {
                a.len() == b.len() && a.iter().zip(b.iter()).all(|(a, b)| a.is_compatible_with(b))
            },
            (Type::Function(params1, ret1), Type::Function(params2, ret2)) => {
                params1.len() == params2.len() &&
                params1.iter().zip(params2.iter()).all(|(a, b)| a.is_compatible_with(b)) &&
                ret1.is_compatible_with(ret2)
            },
            (Type::Custom(a), Type::Custom(b)) => a == b,
            (Type::Enum { name: a, .. }, Type::Enum { name: b, .. }) => a == b,
            (Type::Result(a1, b1), Type::Result(a2, b2)) => {
                a1.is_compatible_with(a2) && b1.is_compatible_with(b2)
            },
            (Type::Optional(a), Type::Optional(b)) => a.is_compatible_with(b),
            (Type::Union(types1), Type::Union(types2)) => {
                // A union is compatible if all its types are compatible with the other union's types
                types1.iter().all(|t1| types2.iter().any(|t2| t1.is_compatible_with(t2))) &&
                types2.iter().all(|t2| types1.iter().any(|t1| t2.is_compatible_with(t1)))
            },
            (Type::Union(types), other) | (other, Type::Union(types)) => {
                // A union is compatible with a type if any of its types is compatible with that type
                types.iter().any(|t| t.is_compatible_with(other))
            },
            _ => false,
        }
    }

    pub fn is_result(&self) -> bool {
        matches!(self, Type::Result(_, _))
    }

    #[allow(dead_code)]
    pub fn is_optional(&self) -> bool {
        matches!(self, Type::Optional(_))
    }

    #[allow(dead_code)]
    pub fn is_enum(&self) -> bool {
        matches!(self, Type::Enum { .. })
    }

    #[allow(dead_code)]
    pub fn is_unit(&self) -> bool {
        matches!(self, Type::Unit)
    }

    #[allow(dead_code)]
    pub fn is_tuple(&self) -> bool {
        matches!(self, Type::Tuple(_))
    }

    #[allow(dead_code)]
    pub fn is_union(&self) -> bool {
        matches!(self, Type::Union(_))
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
                let mut map = HashMap::new();
                for (name, field_type) in fields {
                    map.insert(name, field_type.into());
                }
                Type::Object(map)
            },
            AstType::Tuple(types) => Type::Tuple(types.into_iter().map(|t| t.into()).collect()),
            AstType::Optional(inner) => Type::Optional(Box::new((*inner).into())),
            AstType::Result(ok, err) => Type::Result(Box::new((*ok).into()), Box::new((*err).into())),
            AstType::Custom(name) => Type::Custom(name),
            AstType::Enum { name, generics } => Type::Enum {
                name,
                generics,
                variants: HashMap::new(),
            },
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Number => write!(f, "number"),
            Type::String => write!(f, "string"),
            Type::Boolean => write!(f, "boolean"),
            Type::Void => write!(f, "void"),
            Type::Null => write!(f, "null"),
            Type::Undefined => write!(f, "undefined"),
            Type::Any => write!(f, "any"),
            Type::Unit => write!(f, "unit"),
            Type::Array(inner) => write!(f, "{}[]", inner),
            Type::Object(fields) => {
                write!(f, "{{")?;
                let mut first = true;
                for (name, ty) in fields {
                    if !first {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", name, ty)?;
                    first = false;
                }
                write!(f, "}}")
            },
            Type::Tuple(types) => {
                write!(f, "(")?;
                let mut first = true;
                for ty in types {
                    if !first {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", ty)?;
                    first = false;
                }
                write!(f, ")")
            },
            Type::Function(params, ret) => {
                write!(f, "fn(")?;
                let mut first = true;
                for param in params {
                    if !first {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", param)?;
                    first = false;
                }
                write!(f, ") -> {}", ret)
            },
            Type::Custom(name) => write!(f, "{}", name),
            Type::Enum { name, .. } => write!(f, "{}", name),
            Type::Result(ok, err) => write!(f, "Result<{}, {}>", ok, err),
            Type::Optional(inner) => write!(f, "{}?", inner),
            Type::Union(types) => {
                write!(f, "(")?;
                let mut first = true;
                for ty in types {
                    if !first {
                        write!(f, " | ")?;
                    }
                    write!(f, "{}", ty)?;
                    first = false;
                }
                write!(f, ")")
            },
        }
    }
}
