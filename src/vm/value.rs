#![allow(dead_code)]

use std::fmt;
use crate::parser::ast::Type;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Null,
    Undefined,
    Array(Vec<Value>),
    Object(std::collections::HashMap<String, Value>),
    Function {
        name: String,
        parameters: Vec<String>,
        body: Box<crate::parser::ast::Statement>,
    },
    Enum {
        name: String,
        tag: u32,
        fields: Vec<Value>,
    },
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Null => write!(f, "null"),
            Value::Undefined => write!(f, "undefined"),
            Value::Array(arr) => {
                let values: Vec<String> = arr.iter().map(|v| v.to_string()).collect();
                write!(f, "[{}]", values.join(", "))
            },
            Value::Object(obj) => {
                let mut pairs = Vec::new();
                for (name, value) in obj {
                    pairs.push(format!("{}: {}", name, value));
                }
                write!(f, "{{ {} }}", pairs.join(", "))
            },
            Value::Function { name, .. } => write!(f, "function {}", name),
            Value::Enum { name, tag, fields } => {
                if fields.is_empty() {
                    write!(f, "{}", name)
                } else {
                    let values: Vec<String> = fields.iter().map(|v| v.to_string()).collect();
                    write!(f, "{}({})", name, values.join(", "))
                }
            },
        }
    }
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Boolean(b) => *b,
            Value::Number(n) => *n != 0.0,
            Value::String(s) => !s.is_empty(),
            Value::Null | Value::Undefined => false,
            Value::Array(arr) => !arr.is_empty(),
            Value::Object(_) => true,
            Value::Function { .. } => true,
            Value::Enum { .. } => true,
        }
    }

    pub fn equals(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Null, Value::Null) => true,
            (Value::Undefined, Value::Undefined) => true,
            (Value::Array(a), Value::Array(b)) => a == b,
            (Value::Object(a), Value::Object(b)) => a == b,
            (Value::Enum { name: name1, tag: tag1, fields: fields1 }, 
             Value::Enum { name: name2, tag: tag2, fields: fields2 }) => {
                name1 == name2 && tag1 == tag2 && fields1 == fields2
            },
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct VMError {
    pub message: String,
    pub span: crate::utils::position::Span,
}

impl fmt::Display for VMError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "VM error at {}: {}", self.span, self.message)
    }
}

impl std::error::Error for VMError {}
