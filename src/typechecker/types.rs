#![allow(dead_code)]

use std::collections::HashMap;
use super::scope::Type;
use crate::parser::ast::{BinaryOp, UnaryOp};

#[derive(Debug, Clone)]
pub struct TypeEnvironment {
    pub types: HashMap<String, Type>,
    pub builtin_types: HashMap<String, Type>,
    pub type_aliases: HashMap<String, Type>,
}

impl TypeEnvironment {
    pub fn new() -> Self {
        let mut builtin_types = HashMap::new();
        
        builtin_types.insert("number".to_string(), Type::Number);
        builtin_types.insert("string".to_string(), Type::String);
        builtin_types.insert("boolean".to_string(), Type::Boolean);
        builtin_types.insert("void".to_string(), Type::Void);
        builtin_types.insert("null".to_string(), Type::Null);
        builtin_types.insert("undefined".to_string(), Type::Undefined);
        builtin_types.insert("any".to_string(), Type::Any);
        
        builtin_types.insert("Array".to_string(), 
            Type::Function(vec![Type::Any], Box::new(Type::Any)));
        
        builtin_types.insert("Object".to_string(), 
            Type::Function(vec![], Box::new(Type::Any)));
        
        Self {
            types: HashMap::new(),
            builtin_types,
            type_aliases: HashMap::new(),
        }
    }

    pub fn lookup_type(&self, name: &str) -> Option<Type> {
        if let Some(alias) = self.type_aliases.get(name) {
            return Some(alias.clone());
        }
        
        if let Some(builtin) = self.builtin_types.get(name) {
            return Some(builtin.clone());
        }
        
        self.types.get(name).cloned()
    }

    pub fn define_type(&mut self, name: String, type_def: Type) -> Result<(), TypeEnvironmentError> {
        if self.types.contains_key(&name) || self.builtin_types.contains_key(&name) {
            return Err(TypeEnvironmentError::TypeAlreadyDefined { name });
        }
        
        self.types.insert(name, type_def);
        Ok(())
    }

    pub fn define_alias(&mut self, name: String, alias_type: Type) -> Result<(), TypeEnvironmentError> {
        if self.type_aliases.contains_key(&name) || self.types.contains_key(&name) {
            return Err(TypeEnvironmentError::TypeAlreadyDefined { name });
        }
        
        self.type_aliases.insert(name, alias_type);
        Ok(())
    }

    pub fn infer_expression_type(&self, expr: &super::super::parser::ast::Expression) -> Result<Type, TypeEnvironmentError> {
        match expr {
            super::super::parser::ast::Expression::Identifier(name, _) => {
                self.lookup_type(name)
                    .ok_or_else(|| TypeEnvironmentError::UnknownType { name: name.clone() })
            },
            
super::super::parser::ast::Expression::VariantCall { enum_name: _, variant_name: _, arguments: _arguments, .. } => {
                // For now, we'll return a placeholder type
                // This should be resolved during type checking phase
                Ok(Type::Any)
            },
            
            super::super::parser::ast::Expression::Literal(literal, _) => self.infer_literal_type(literal),
            
            super::super::parser::ast::Expression::Binary { left, op, right, .. } => {
                let left_type = self.infer_expression_type(left)?;
                let right_type = self.infer_expression_type(right)?;
                
                self.infer_binary_op_type(op, &left_type, &right_type)
            },
            
            super::super::parser::ast::Expression::Unary { op, expr, .. } => {
                let expr_type = self.infer_expression_type(expr)?;
                self.infer_unary_op_type(op, &expr_type)
            },
            
            super::super::parser::ast::Expression::Call { function, arguments, .. } => {
                let func_type = self.infer_expression_type(function)?;
                
                if let Type::Function(params, return_type) = func_type {
                    if params.len() != arguments.len() {
                        return Err(TypeEnvironmentError::ArgumentCountMismatch {
                            expected: params.len(),
                            found: arguments.len(),
                        });
                    }
                    
                    for (param_type, arg) in params.iter().zip(arguments.iter()) {
                        let arg_type = self.infer_expression_type(arg)?;
                        if !arg_type.is_compatible_with(param_type) {
                            return Err(TypeEnvironmentError::ArgumentTypeMismatch {
                                expected: param_type.to_string(),
                                found: arg_type.to_string(),
                            });
                        }
                    }
                    
                    Ok(*return_type)
                } else {
                    Err(TypeEnvironmentError::NotAFunction {
                        type_: func_type.to_string(),
                    })
                }
            },
            
            super::super::parser::ast::Expression::Index { expr, index, .. } => {
                let array_type = self.infer_expression_type(expr)?;
                let index_type = self.infer_expression_type(index)?;
                
                if !index_type.is_numeric() {
                    return Err(TypeEnvironmentError::InvalidIndexType {
                        type_: index_type.to_string(),
                    });
                }
                
                if let Type::Array(element_type) = array_type {
                    Ok(*element_type)
                } else {
                    Err(TypeEnvironmentError::NotAnArray {
                        type_: array_type.to_string(),
                    })
                }
            },
            
            super::super::parser::ast::Expression::Member { expr, member, .. } => {
                let obj_type = self.infer_expression_type(expr)?;
                
                if let Type::Object(fields) = &obj_type {
                    fields.get(member)
                        .cloned()
                        .ok_or_else(|| TypeEnvironmentError::UnknownField {
                            field: member.clone(),
                            type_: obj_type.to_string(),
                        })
                } else {
                    Err(TypeEnvironmentError::NotAnObject {
                        type_: obj_type.to_string(),
                    })
                }
            },
            
            super::super::parser::ast::Expression::Assignment { target: _target, value, .. } => {
                let value_type = self.infer_expression_type(value)?;
                Ok(value_type)
            },
            
            super::super::parser::ast::Expression::Lambda { parameters, return_type, .. } => {
                let param_types: Vec<Type> = parameters
                    .iter()
                    .map(|p| {
                        p.type_annotation
                            .clone()
                            .map(|ast_type| Type::from(ast_type))
                            .unwrap_or_else(|| Type::Any)
                    })
                    .collect();
                
                let return_type = return_type
                    .as_ref()
                    .map(|rt| Type::from(rt.clone()))
                    .unwrap_or_else(|| Type::Void);
                
                Ok(Type::Function(param_types, Box::new(return_type)))
            },
            
            super::super::parser::ast::Expression::If { condition, then_branch, else_branch, .. } => {
                let cond_type = self.infer_expression_type(condition)?;
                if !cond_type.is_boolean() && !cond_type.is_compatible_with(&Type::Any) {
                    return Err(TypeEnvironmentError::InvalidConditionType {
                        type_: cond_type.to_string(),
                    });
                }
                
                let then_type = self.infer_expression_type(then_branch)?;
                
                let else_type = if let Some(else_branch) = else_branch {
                    self.infer_expression_type(else_branch)?
                } else {
                    Type::Void
                };
                
                Ok(Type::Union(vec![then_type, else_type]))
            },
            
            super::super::parser::ast::Expression::Match { expr, arms, .. } => {
                let _expr_type = self.infer_expression_type(expr)?;
                let mut arm_types = Vec::new();
                
                for arm in arms {
                    let arm_type = self.infer_expression_type(&arm.body)?;
                    arm_types.push(arm_type);
                }
                
                Ok(Type::Union(arm_types))
            },
            
            super::super::parser::ast::Expression::Try { expr, .. } => {
                let expr_type = self.infer_expression_type(expr)?;
                
                // For now, we'll just return the expression type
                // The actual Result type checking should be done in the type checker
                Ok(expr_type)
            },
        }
    }

    fn infer_literal_type(&self, literal: &super::super::parser::ast::Literal) -> Result<Type, TypeEnvironmentError> {
        match literal {
            super::super::parser::ast::Literal::Integer(_) => Ok(Type::Number),
            super::super::parser::ast::Literal::Float(_) => Ok(Type::Number),
            super::super::parser::ast::Literal::String(_) => Ok(Type::String),
            super::super::parser::ast::Literal::Boolean(_) => Ok(Type::Boolean),
            super::super::parser::ast::Literal::Null => Ok(Type::Null),
            super::super::parser::ast::Literal::Undefined => Ok(Type::Undefined),
            super::super::parser::ast::Literal::Array(elements) => {
                if elements.is_empty() {
                    Ok(Type::Array(Box::new(Type::Any)))
                } else {
                    let first_type = self.infer_expression_type(&elements[0])?;
                    
                    for element in elements.iter().skip(1) {
                        let element_type = self.infer_expression_type(element)?;
                        if !element_type.is_compatible_with(&first_type) {
                            return Ok(Type::Array(Box::new(Type::Any)));
                        }
                    }
                    
                    Ok(Type::Array(Box::new(first_type)))
                }
            },
            super::super::parser::ast::Literal::Object(fields) => {
                let mut field_types = HashMap::new();
                
                for (name, value) in fields {
                    let value_type = self.infer_expression_type(value)?;
                    field_types.insert(name.clone(), value_type);
                }
                
                Ok(Type::Object(field_types))
            },
        }
    }

    fn infer_binary_op_type(
        &self, 
        op: &BinaryOp, 
        left_type: &Type, 
        right_type: &Type
    ) -> Result<Type, TypeEnvironmentError> {
        match op {
            BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide | BinaryOp::Modulo => {
                if left_type.is_numeric() && right_type.is_numeric() {
                    Ok(Type::Number)
                } else if left_type.is_string() && right_type.is_string() {
                    Ok(Type::String)
                } else {
                    Err(TypeEnvironmentError::InvalidBinaryOperands {
                        op: op.to_string(),
                        left: left_type.to_string(),
                        right: right_type.to_string(),
                    })
                }
            },
            
            BinaryOp::Equal | BinaryOp::NotEqual => {
                if left_type.is_compatible_with(right_type) {
                    Ok(Type::Boolean)
                } else {
                    Err(TypeEnvironmentError::IncompatibleTypesForComparison {
                        left: left_type.to_string(),
                        right: right_type.to_string(),
                    })
                }
            },
            
            BinaryOp::Less | BinaryOp::LessEqual | BinaryOp::Greater | BinaryOp::GreaterEqual => {
                if left_type.is_numeric() && right_type.is_numeric() {
                    Ok(Type::Boolean)
                } else if left_type.is_string() && right_type.is_string() {
                    Ok(Type::Boolean)
                } else {
                    Err(TypeEnvironmentError::InvalidComparisonOperands {
                        left: left_type.to_string(),
                        right: right_type.to_string(),
                    })
                }
            },
            
            BinaryOp::And | BinaryOp::Or => {
                if left_type.is_boolean() && right_type.is_boolean() {
                    Ok(Type::Boolean)
                } else {
                    Err(TypeEnvironmentError::InvalidLogicalOperands {
                        op: op.to_string(),
                        left: left_type.to_string(),
                        right: right_type.to_string(),
                    })
                }
            },
            
            BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::BitXor | BinaryOp::ShiftLeft | BinaryOp::ShiftRight => {
                if left_type.is_numeric() && right_type.is_numeric() {
                    Ok(Type::Number)
                } else {
                    Err(TypeEnvironmentError::InvalidBitwiseOperands {
                        op: op.to_string(),
                        left: left_type.to_string(),
                        right: right_type.to_string(),
                    })
                }
            },
            
            BinaryOp::Range => {
                if left_type.is_numeric() && right_type.is_numeric() {
                    Ok(Type::Array(Box::new(Type::Number)))
                } else {
                    Err(TypeEnvironmentError::InvalidRangeOperands {
                        left: left_type.to_string(),
                        right: right_type.to_string(),
                    })
                }
            },
        }
    }

    fn infer_unary_op_type(
        &self, 
        op: &UnaryOp, 
        expr_type: &Type
    ) -> Result<Type, TypeEnvironmentError> {
        match op {
            UnaryOp::Plus | UnaryOp::Minus => {
                if expr_type.is_numeric() {
                    Ok(Type::Number)
                } else {
                    Err(TypeEnvironmentError::InvalidUnaryOperand {
                        op: op.to_string(),
                        type_: expr_type.to_string(),
                    })
                }
            },
            
            UnaryOp::Not => {
                if expr_type.is_boolean() {
                    Ok(Type::Boolean)
                } else {
                    Err(TypeEnvironmentError::InvalidUnaryOperand {
                        op: op.to_string(),
                        type_: expr_type.to_string(),
                    })
                }
            },
            
            UnaryOp::BitNot => {
                if expr_type.is_numeric() {
                    Ok(Type::Number)
                } else {
                    Err(TypeEnvironmentError::InvalidUnaryOperand {
                        op: op.to_string(),
                        type_: expr_type.to_string(),
                    })
                }
            },
        }
    }
}

#[derive(Debug, thiserror::Error, Clone)]
pub enum TypeEnvironmentError {
    #[error("Type '{name}' is already defined")]
    TypeAlreadyDefined { name: String },
    
    #[error("Unknown type '{name}'")]
    UnknownType { name: String },
    
    #[error("Expected {expected} arguments, found {found}")]
    ArgumentCountMismatch { expected: usize, found: usize },
    
    #[error("Expected argument of type '{expected}', found '{found}'")]
    ArgumentTypeMismatch { expected: String, found: String },
    
    #[error("Cannot call expression of type '{type_}'")]
    NotAFunction { type_: String },
    
    #[error("Cannot index expression of type '{type_}'")]
    NotAnArray { type_: String },
    
    #[error("Invalid index type '{type_}'")]
    InvalidIndexType { type_: String },
    
    #[error("Cannot access field '{field}' on type '{type_}'")]
    UnknownField { field: String, type_: String },
    
    #[error("Cannot access field on type '{type_}'")]
    NotAnObject { type_: String },
    
    #[error("Cannot assign to immutable variable")]
    ImmutableAssignment,
    
    #[error("Condition must be boolean, found '{type_}'")]
    InvalidConditionType { type_: String },
    
    #[error("Invalid operands for binary operator '{op}': {left} and {right}")]
    InvalidBinaryOperands { op: String, left: String, right: String },
    
    #[error("Cannot compare types '{left}' and '{right}'")]
    IncompatibleTypesForComparison { left: String, right: String },
    
    #[error("Invalid operands for logical operator '{op}': {left} and {right}")]
    InvalidLogicalOperands { op: String, left: String, right: String },
    
    #[error("Invalid operands for bitwise operator '{op}': {left} and {right}")]
    InvalidBitwiseOperands { op: String, left: String, right: String },
    
    #[error("Invalid operands for range operator: {left} and {right}")]
    InvalidRangeOperands { left: String, right: String },
    
    #[error("Invalid operand for unary operator '{op}': type '{type_}'")]
    InvalidUnaryOperand { op: String, type_: String },
    
    #[error("Invalid comparison operands: {left} and {right}")]
    InvalidComparisonOperands { left: String, right: String },
    
    #[error("Cannot use '?' outside a function that returns Result<..., string>")]
    InvalidTryOperator { type_: String },
}
