#![allow(dead_code)]

use std::rc::Rc;
use crate::utils::position::Span;
use crate::parser::ast::{Statement, Expression, Program};
use super::scope::{Scope, ScopeKind, Type, ScopeError};
use super::types::{TypeEnvironment, TypeEnvironmentError};
use super::use_def::{UseDefAnalysis, DefinitionKind};
use super::smart_mutability::SmartMutabilityChecker;

pub type TypeCheckResult<T> = Result<T, TypeCheckError>;

#[derive(Debug, thiserror::Error, Clone)]
pub enum TypeCheckError {
    #[error("Type check error: {message}")]
    TypeCheck { message: String, span: Span },
    
    #[error("Scope error: {0}")]
    Scope(#[from] ScopeError),
    
    #[error("Type environment error: {0}")]
    TypeEnvironment(#[from] TypeEnvironmentError),
}

impl TypeCheckError {
    pub fn new(message: String, span: Span) -> Self {
        Self::TypeCheck { message, span }
    }
}

pub struct TypeChecker {
    global_scope: Rc<Scope>,
    current_scope: Rc<Scope>,
    type_env: TypeEnvironment,
    use_def_analysis: UseDefAnalysis,
    errors: Vec<TypeCheckError>,
}

impl TypeChecker {
    pub fn new() -> Self {
        let global_scope = Rc::new(Scope::new(ScopeKind::Global, None, Span::default()));
        Self {
            global_scope: global_scope.clone(),
            current_scope: global_scope,
            type_env: TypeEnvironment::new(),
            use_def_analysis: UseDefAnalysis::new(),
            errors: Vec::new(),
        }
    }

    pub fn check_program(&mut self, program: &Program) -> TypeCheckResult<()> {
        for stmt in &program.statements {
            self.collect_top_level_definitions(stmt)?;
        }

        for stmt in &program.statements {
            self.check_statement(stmt)?;
        }

        self.check_unused_definitions();

        self.check_circular_dependencies();

        // Check Smart Mutability™ rules
        let mut smart_checker = SmartMutabilityChecker::new();
        if let Err(mutability_errors) = smart_checker.check_program(program) {
            for error in mutability_errors {
                self.errors.push(TypeCheckError::new(error, Span::default()));
            }
        }

        if !self.errors.is_empty() {
            Err(self.errors.remove(0))
        } else {
            Ok(())
        }
    }

    pub fn collect_top_level_definitions(&mut self, stmt: &Statement) -> TypeCheckResult<()> {
        match stmt {
            Statement::Variable { mutable, name, type_annotation, value, .. } => {
                let type_ = if let Some(annotation) = type_annotation {
                    Type::from(annotation.clone())
                } else {
                    let value_type = self.infer_expression_type(value)?;
                    value_type
                };

                let current_scope = Rc::make_mut(&mut self.current_scope);
                current_scope.define_variable(name.clone(), type_, *mutable, stmt.span())?;

                self.use_def_analysis.define_variable(name.clone(), stmt.span());
            },

            Statement::Constant { name, type_annotation, value, .. } => {
                let type_ = if let Some(annotation) = type_annotation {
                    Type::from(annotation.clone())
                } else {
                    let value_type = self.infer_expression_type(value)?;
                    value_type
                };

                let current_scope = Rc::make_mut(&mut self.current_scope);
                current_scope.define_variable(name.clone(), type_, false, stmt.span())?;

                self.use_def_analysis.define_constant(name.clone(), stmt.span());
            },

            Statement::Function { name, parameters, return_type, body: _, .. } => {
                let param_types: Vec<Type> = parameters
                    .iter()
                    .map(|p| {
                        p.type_annotation
                            .clone()
                            .unwrap_or_else(|| crate::parser::ast::Type::Undefined.into())
                            .into()
                    })
                    .collect();

                let return_type_clone = return_type
                    .as_ref()
                    .map(|rt| rt.clone().into())
                    .unwrap_or_else(|| Type::Void);

                let _func_type = Type::Function(param_types.clone(), Box::new(return_type_clone.clone()));

                let current_scope = Rc::make_mut(&mut self.current_scope);
                current_scope.define_function(
                    name.clone(),
                    param_types.iter()
                        .zip(parameters.iter())
                        .map(|(ty, param)| super::scope::ParameterInfo {
                            name: param.name.clone(),
                            type_: ty.clone(),
                            default_value: param.default_value.clone(),
                        })
                        .collect(),
                    return_type_clone,
                    stmt.span()
                )?;

                self.use_def_analysis.define_function(name.clone(), stmt.span());
            },

            Statement::Class { name, base, fields, methods, .. } => {
                let mut field_types = std::collections::HashMap::new();
                
                for field in fields {
                    let field_type = field.type_annotation
                        .clone()
                        .unwrap_or_else(|| crate::parser::ast::Type::Undefined.into())
                        .into();
                    field_types.insert(field.name.clone(), field_type);
                }

                let class_type = Type::Object(field_types);
                self.type_env.define_type(name.clone(), class_type)?;

                self.use_def_analysis.define_variable(name.clone(), stmt.span());

                if let Some(base_name) = base {
                    if let Some(_base_def) = self.use_def_analysis.lookup_definition(base_name) {
                        self.use_def_analysis.add_dependency(name, base_name);
                    }
                }

                for method in methods {
                    if let Statement::Function { name: method_name, .. } = method {
                        self.use_def_analysis.add_dependency(name, method_name);
                    }
                }
            },

            Statement::Import { .. } => {
            },

            Statement::Block(statements, _) => {
                let block_scope = self.current_scope.enter_scope(ScopeKind::Block, stmt.span());
                let previous_scope = std::mem::replace(&mut self.current_scope, block_scope);

                for stmt in statements {
                    self.collect_top_level_definitions(stmt)?;
                }

                self.current_scope = previous_scope;
            },

            _ => {
            }
        }

        Ok(())
    }

    pub fn check_statement(&mut self, stmt: &Statement) -> TypeCheckResult<()> {
        match stmt {
            Statement::Variable { mutable, name, type_annotation, value, span } => {
                let expected_type = if let Some(annotation) = type_annotation {
                    Type::from(annotation.clone())
                } else {
                    let value_type = self.infer_expression_type(value)?;
                    let current_scope = Rc::make_mut(&mut self.current_scope);
                    current_scope.define_variable(name.clone(), value_type.clone(), *mutable, *span)?;
                    value_type
                };

                let actual_type = self.infer_expression_type(value)?;
                if !actual_type.is_compatible_with(&expected_type) {
                    self.errors.push(TypeCheckError::new(
                        format!("Expected type '{}', found type '{}'", expected_type.to_string(), actual_type.to_string()),
                        *span
                    ));
                    return Err(self.errors.last().unwrap().clone());
                }

                self.use_def_analysis.mark_usage(name, *span);

                Ok(())
            },

            Statement::Constant { name, type_annotation, value, span } => {
                let expected_type = if let Some(annotation) = type_annotation {
                    Type::from(annotation.clone())
                } else {
                    let value_type = self.infer_expression_type(value)?;
                    let current_scope = Rc::make_mut(&mut self.current_scope);
                    current_scope.define_variable(name.clone(), value_type.clone(), false, *span)?;
                    value_type
                };

                let actual_type = self.infer_expression_type(value)?;
                if !actual_type.is_compatible_with(&expected_type) {
                    self.errors.push(TypeCheckError::new(
                        format!("Expected type '{}', found type '{}'", expected_type.to_string(), actual_type.to_string()),
                        *span
                    ));
                    return Err(self.errors.last().unwrap().clone());
                }

                self.use_def_analysis.mark_usage(name, *span);

                Ok(())
            },

            Statement::Function { name, parameters, return_type, body, span, .. } => {
                let function_scope = self.current_scope.enter_scope(ScopeKind::Function, *span);
                let previous_scope = std::mem::replace(&mut self.current_scope, function_scope);

                for param in parameters {
                    let param_type = param.type_annotation
                        .clone()
                        .map(|t| Type::from(t))
                        .unwrap_or_else(|| crate::parser::ast::Type::Undefined.into());

                    let current_scope = Rc::make_mut(&mut self.current_scope);
                    current_scope.define_variable(param.name.clone(), param_type.clone(), false, *span)?;

                    self.use_def_analysis.define_parameter(param.name.clone(), *span);
                }

                for stmt in body {
                    self.check_statement(stmt)?;
                }

                let actual_return_type = self.infer_statement_return_type(body)?;
                let expected_return_type = return_type
                    .as_ref()
                    .map(|rt| rt.clone().into())
                    .unwrap_or_else(|| Type::Void);

                if !actual_return_type.is_compatible_with(&expected_return_type) {
                    self.errors.push(TypeCheckError::new(
                        format!("Function returns '{}', but declared return type is '{}'", 
                               actual_return_type.to_string(), expected_return_type.to_string()),
                        *span
                    ));
                    return Err(self.errors.last().unwrap().clone());
                }

                self.current_scope = previous_scope;

                self.use_def_analysis.mark_usage(name, *span);

                Ok(())
            },

            Statement::If { condition, then_branch, elif_branches, else_branch, span } => {
                let cond_type = self.infer_expression_type(condition)?;
                if !cond_type.is_boolean() && !cond_type.is_compatible_with(&Type::Any) {
                    self.errors.push(TypeCheckError::new(
                        format!("Condition must be boolean, found type '{}'", cond_type.to_string()),
                        condition.span()
                    ));
                    return Err(self.errors.last().unwrap().clone());
                }

                let if_scope = self.current_scope.enter_scope(ScopeKind::If, *span);
                let previous_scope = std::mem::replace(&mut self.current_scope, if_scope);

                for stmt in then_branch {
                    self.check_statement(stmt)?;
                }

                self.current_scope = previous_scope;

                for (elif_cond, elif_body) in elif_branches {
                    let elif_cond_type = self.infer_expression_type(elif_cond)?;
                    if !elif_cond_type.is_boolean() && !elif_cond_type.is_compatible_with(&Type::Any) {
                        self.errors.push(TypeCheckError::new(
                            format!("Elif condition must be boolean, found type '{}'", elif_cond_type.to_string()),
                            elif_cond.span()
                        ));
                        return Err(self.errors.last().unwrap().clone());
                    }

                    let elif_scope = self.current_scope.enter_scope(ScopeKind::If, *span);
                    let elif_previous_scope = std::mem::replace(&mut self.current_scope, elif_scope);

                    for stmt in elif_body {
                        self.check_statement(stmt)?;
                    }

                    self.current_scope = elif_previous_scope;
                }

                if let Some(else_body) = else_branch {
                    let else_scope = self.current_scope.enter_scope(ScopeKind::If, *span);
                    let else_previous_scope = std::mem::replace(&mut self.current_scope, else_scope);

                    for stmt in else_body {
                        self.check_statement(stmt)?;
                    }

                    self.current_scope = else_previous_scope;
                }

                Ok(())
            },

            Statement::While { condition, body, span } => {
                let cond_type = self.infer_expression_type(condition)?;
                if !cond_type.is_boolean() && !cond_type.is_compatible_with(&Type::Any) {
                    self.errors.push(TypeCheckError::new(
                        format!("While condition must be boolean, found type '{}'", cond_type.to_string()),
                        condition.span()
                    ));
                    return Err(self.errors.last().unwrap().clone());
                }

                let loop_scope = self.current_scope.enter_scope(ScopeKind::Loop, *span);
                let previous_scope = std::mem::replace(&mut self.current_scope, loop_scope);

                for stmt in body {
                    self.check_statement(stmt)?;
                }

                self.current_scope = previous_scope;

                Ok(())
            },

            Statement::For { variable, iterable, body, span } => {
                let iterable_type = self.infer_expression_type(iterable)?;
                if !iterable_type.is_compatible_with(&Type::Array(Box::new(Type::Any))) {
                    self.errors.push(TypeCheckError::new(
                        format!("For loop iterable must be array, found type '{}'", iterable_type.to_string()),
                        iterable.span()
                    ));
                    return Err(self.errors.last().unwrap().clone());
                }

                let current_scope = Rc::make_mut(&mut self.current_scope);
                current_scope.define_variable(variable.clone(), Type::Any, true, *span)?;

                let loop_scope = self.current_scope.enter_scope(ScopeKind::Loop, *span);
                let previous_scope = std::mem::replace(&mut self.current_scope, loop_scope);

                for stmt in body {
                    self.check_statement(stmt)?;
                }

                self.current_scope = previous_scope;

                self.use_def_analysis.mark_usage(variable, *span);

                Ok(())
            },

            Statement::Match { expr, arms, span } => {
                let _expr_type = self.infer_expression_type(expr)?;

                let match_scope = self.current_scope.enter_scope(ScopeKind::Match, *span);
                let previous_scope = std::mem::replace(&mut self.current_scope, match_scope);

                for arm in arms {
                    // Patterns are not expressions, so we can't infer their type directly
                    // For now, we'll skip pattern type checking
                    if let Some(guard) = &arm.guard {
                        let guard_type = self.infer_expression_type(guard)?;
                        if !guard_type.is_boolean() {
                            self.errors.push(TypeCheckError::new(
                                format!("Match guard must be boolean, found type '{}'", guard_type.to_string()),
                                guard.span()
                            ));
                            return Err(self.errors.last().unwrap().clone());
                        }
                    }

                    self.check_expression(&arm.body)?;
                }

                self.current_scope = previous_scope;

                Ok(())
            },

            Statement::Block(statements, span) => {
                let block_scope = self.current_scope.enter_scope(ScopeKind::Block, *span);
                let previous_scope = std::mem::replace(&mut self.current_scope, block_scope);

                for stmt in statements {
                    self.check_statement(stmt)?;
                }

                self.current_scope = previous_scope;

                Ok(())
            },

            Statement::Expression(expr, _span) => {
                self.check_expression(expr)
            },

            Statement::Return(value, _span) => {
                if let Some(expr) = value {
                    self.check_expression(expr)?;
                }
                Ok(())
            },

            Statement::Class { name, base, fields, methods, span } => {
                let mut field_types = std::collections::HashMap::new();
                
                for field in fields {
                    let field_type = field.type_annotation
                        .clone()
                        .unwrap_or_else(|| crate::parser::ast::Type::Undefined.into())
                        .into();
                    field_types.insert(field.name.clone(), field_type);
                }

                let class_type = Type::Object(field_types);
                self.type_env.define_type(name.clone(), class_type)?;
                
                if let Some(base_name) = base {
                    if let Some(_base_type) = self.type_env.lookup_type(base_name) {
                        self.use_def_analysis.add_dependency(name, base_name);
                    } else {
                        self.errors.push(TypeCheckError::new(
                            format!("Base class '{}' not found", base_name),
                            *span
                        ));
                        return Err(self.errors.last().unwrap().clone());
                    }
                }

                for method in methods {
                    self.check_statement(method)?;
                }

                Ok(())
            },

            Statement::Import { module, alias, items, span } => {
                let import_name = alias.as_ref().unwrap_or(module);
                self.use_def_analysis.mark_usage(import_name, *span);

                if let Some(items) = items {
                    for item in items {
                        let imported_name = item.name.clone();
                        let tracking_name = item.alias.clone().unwrap_or_else(|| imported_name.clone());
                        
                        self.use_def_analysis.mark_usage(&tracking_name, *span);
                    }
                }

                Ok(())
            },
        }
    }

    pub fn check_expression(&mut self, expr: &Expression) -> TypeCheckResult<()> {
        match expr {
            Expression::Identifier(name, span) => {
                if self.current_scope.lookup_variable(name).is_none() {
                    self.errors.push(TypeCheckError::new(
                        format!("Undefined variable '{}'", name),
                        *span
                    ));
                    return Err(self.errors.last().unwrap().clone());
                }

                self.use_def_analysis.mark_usage(name, *span);

                Ok(())
            },

            Expression::Literal(_, _) => {
                Ok(())
            },

            Expression::Binary { left, op, right, .. } => {
                self.check_expression(left)?;
                self.check_expression(right)?;

                let left_type = self.infer_expression_type(left)?;
                let right_type = self.infer_expression_type(right)?;

                match op {
                    crate::parser::ast::BinaryOp::Add | 
                    crate::parser::ast::BinaryOp::Subtract | 
                    crate::parser::ast::BinaryOp::Multiply | 
                    crate::parser::ast::BinaryOp::Divide |
                    crate::parser::ast::BinaryOp::Modulo => {
                        if !left_type.is_numeric() || !right_type.is_numeric() {
                            self.errors.push(TypeCheckError::new(
                                format!("Arithmetic operators require numeric operands, got '{}' and '{}'", 
                                       left_type.to_string(), right_type.to_string()),
                                expr.span()
                            ));
                            return Err(self.errors.last().unwrap().clone());
                        }
                    },

                    crate::parser::ast::BinaryOp::Equal | 
                    crate::parser::ast::BinaryOp::NotEqual => {
                        if !left_type.is_compatible_with(&right_type) {
                            self.errors.push(TypeCheckError::new(
                                format!("Cannot compare incompatible types '{}' and '{}'", 
                                       left_type.to_string(), right_type.to_string()),
                                expr.span()
                            ));
                            return Err(self.errors.last().unwrap().clone());
                        }
                    },

                    crate::parser::ast::BinaryOp::Less | 
                    crate::parser::ast::BinaryOp::LessEqual | 
                    crate::parser::ast::BinaryOp::Greater | 
                    crate::parser::ast::BinaryOp::GreaterEqual => {
                        if (!left_type.is_numeric() || !right_type.is_numeric()) &&
                           (!left_type.is_string() || !right_type.is_string()) {
                            self.errors.push(TypeCheckError::new(
                                format!("Comparison operators require comparable types, got '{}' and '{}'", 
                                       left_type.to_string(), right_type.to_string()),
                                expr.span()
                            ));
                            return Err(self.errors.last().unwrap().clone());
                        }
                    },

                    crate::parser::ast::BinaryOp::And | 
                    crate::parser::ast::BinaryOp::Or => {
                        if !left_type.is_boolean() || !right_type.is_boolean() {
                            self.errors.push(TypeCheckError::new(
                                format!("Logical operators require boolean operands, got '{}' and '{}'", 
                                       left_type.to_string(), right_type.to_string()),
                                expr.span()
                            ));
                            return Err(self.errors.last().unwrap().clone());
                        }
                    },

                    _ => {
                    }
                }

                Ok(())
            },

            Expression::Unary { op, expr, .. } => {
                self.check_expression(expr)?;

                let expr_type = self.infer_expression_type(expr)?;
                match op {
                    crate::parser::ast::UnaryOp::Plus | 
                    crate::parser::ast::UnaryOp::Minus => {
                        if !expr_type.is_numeric() {
                            self.errors.push(TypeCheckError::new(
                                format!("Unary operators require numeric operand, got type '{}'", expr_type.to_string()),
                                expr.span()
                            ));
                            return Err(self.errors.last().unwrap().clone());
                        }
                    },

                    crate::parser::ast::UnaryOp::Not => {
                        if !expr_type.is_boolean() {
                            self.errors.push(TypeCheckError::new(
                                format!("Logical not requires boolean operand, got type '{}'", expr_type.to_string()),
                                expr.span()
                            ));
                            return Err(self.errors.last().unwrap().clone());
                        }
                    },

                    _ => {
                    }
                }

                Ok(())
            },

            Expression::Call { function, arguments, .. } => {
                self.check_expression(function)?;

                let func_type = self.infer_expression_type(function)?;
                if let Type::Function(params, _return_type) = func_type {
                    if params.len() != arguments.len() {
                        self.errors.push(TypeCheckError::new(
                            format!("Expected {} arguments, got {}", params.len(), arguments.len()),
                            expr.span()
                        ));
                        return Err(self.errors.last().unwrap().clone());
                    }

                    for (param_type, arg) in params.iter().zip(arguments.iter()) {
                        self.check_expression(arg)?;
                        let arg_type = self.infer_expression_type(arg)?;
                        if !arg_type.is_compatible_with(param_type) {
                            self.errors.push(TypeCheckError::new(
                                format!("Expected argument of type '{}', got '{}'", 
                                       param_type.to_string(), arg_type.to_string()),
                                arg.span()
                            ));
                            return Err(self.errors.last().unwrap().clone());
                        }
                    }
                } else {
                    self.errors.push(TypeCheckError::new(
                        format!("Cannot call value of type '{}'", func_type.to_string()),
                        expr.span()
                    ));
                    return Err(self.errors.last().unwrap().clone());
                }

                Ok(())
            },

            Expression::Index { expr, index, .. } => {
                self.check_expression(expr)?;
                self.check_expression(index)?;

                let array_type = self.infer_expression_type(expr)?;
                if let Type::Array(_) = array_type {
                    let index_type = self.infer_expression_type(index)?;
                    if !index_type.is_numeric() {
                        self.errors.push(TypeCheckError::new(
                            format!("Array index must be numeric, got type '{}'", index_type.to_string()),
                            index.span()
                        ));
                        return Err(self.errors.last().unwrap().clone());
                    }
                } else {
                    self.errors.push(TypeCheckError::new(
                        format!("Cannot index value of type '{}'", array_type.to_string()),
                        expr.span()
                    ));
                    return Err(self.errors.last().unwrap().clone());
                }

                Ok(())
            },

            Expression::Member { expr, member, .. } => {
                self.check_expression(expr)?;

                let obj_type = self.infer_expression_type(expr)?;
                if let Type::Object(fields) = obj_type {
                    if !fields.contains_key(member) {
                        self.errors.push(TypeCheckError::new(
                            format!("Object has no field '{}'", member),
                            expr.span()
                        ));
                        return Err(self.errors.last().unwrap().clone());
                    }
                } else {
                    self.errors.push(TypeCheckError::new(
                        format!("Cannot access property '{}' on value of type '{}'", member, obj_type.to_string()),
                        expr.span()
                    ));
                    return Err(self.errors.last().unwrap().clone());
                }

                Ok(())
            },

            Expression::Assignment { target, value, op, .. } => {
                self.check_expression(target)?;
                self.check_expression(value)?;

                let target_type = self.infer_expression_type(target)?;
                let value_type = self.infer_expression_type(value)?;

                if !value_type.is_compatible_with(&target_type) {
                    self.errors.push(TypeCheckError::new(
                        format!("Cannot assign value of type '{}' to target of type '{}'", 
                               value_type.to_string(), target_type.to_string()),
                        expr.span()
                    ));
                    return Err(self.errors.last().unwrap().clone());
                }

                if let Some(_) = op {
                    match op.as_ref().unwrap() {
                        crate::parser::ast::BinaryOp::Add |
                        crate::parser::ast::BinaryOp::Subtract |
                        crate::parser::ast::BinaryOp::Multiply |
                        crate::parser::ast::BinaryOp::Divide |
                        crate::parser::ast::BinaryOp::Modulo => {
                            if !target_type.is_numeric() {
                                self.errors.push(TypeCheckError::new(
                                    format!("Compound assignment requires numeric type, got '{}'", target_type.to_string()),
                                    expr.span()
                                ));
                                return Err(self.errors.last().unwrap().clone());
                            }
                        },
                        _ => {
                        }
                    }
                }

                Ok(())
            },

            Expression::Lambda { parameters, return_type: _return_type, body, .. } => {
                let lambda_scope = self.current_scope.enter_scope(ScopeKind::Function, expr.span());
                let previous_scope = std::mem::replace(&mut self.current_scope, lambda_scope);

                for param in parameters {
                    let param_type = param.type_annotation
                        .clone()
                        .unwrap_or_else(|| crate::parser::ast::Type::Undefined.into())
                        .into();

                    let current_scope = Rc::make_mut(&mut self.current_scope);
                    current_scope.define_variable(param.name.clone(), param_type, false, expr.span())?;
                }

                self.check_statement(body)?;

                self.current_scope = previous_scope;

                Ok(())
            },

            Expression::If { condition, then_branch, else_branch, .. } => {
                self.check_expression(condition)?;

                let cond_type = self.infer_expression_type(condition)?;
                if !cond_type.is_boolean() {
                    self.errors.push(TypeCheckError::new(
                        format!("If condition must be boolean, got type '{}'", cond_type.to_string()),
                        condition.span()
                    ));
                    return Err(self.errors.last().unwrap().clone());
                }

                self.check_expression(then_branch)?;

                if let Some(else_branch) = else_branch {
                    self.check_expression(else_branch)?;
                }

                Ok(())
            },

            Expression::Match { expr, arms, .. } => {
                self.check_expression(expr)?;

                for arm in arms {
                    // Patterns are not expressions, so we can't check them directly
                    // For now, we'll skip pattern checking
                    
                    if let Some(guard) = &arm.guard {
                        self.check_expression(guard)?;
                    }
                    
                    self.check_expression(&arm.body)?;
                }

                Ok(())
            },

            Expression::Try { expr, .. } => {
                self.check_expression(expr)?;
                Ok(())
            },
        }
    }

    pub fn infer_expression_type(&mut self, expr: &Expression) -> TypeCheckResult<super::scope::Type> {
        self.type_env.infer_expression_type(expr)
            .map_err(|e| TypeCheckError::TypeEnvironment(e))
    }

    pub fn infer_statement_return_type(&mut self, statements: &[Statement]) -> TypeCheckResult<Type> {
        let mut return_type = Type::Void;

        for stmt in statements {
            if let Statement::Return(value, _) = stmt {
                if let Some(expr) = value {
                    return_type = self.infer_expression_type(expr)?;
                } else {
                    return_type = Type::Void;
                }
                break;
            } else if let Statement::Block(inner_statements, _) = stmt {
                return_type = self.infer_statement_return_type(inner_statements)?;
                if !matches!(return_type, Type::Void) {
                    break;
                }
            }
        }

        Ok(return_type)
    }

    pub fn check_unused_definitions(&mut self) {
        let unused_defs = self.use_def_analysis.get_unused_definitions();
        
        for def in unused_defs {
            self.errors.push(TypeCheckError::new(
                format!("Unused {}: '{}'", match def.kind {
                    DefinitionKind::Variable => "variable",
                    DefinitionKind::Function => "function",
                    DefinitionKind::Parameter => "parameter",
                    DefinitionKind::Constant => "constant",
                    DefinitionKind::Class => "class",
                    DefinitionKind::Module => "module",
                }, def.name),
                def.span
            ));
        }
    }

    pub fn check_circular_dependencies(&mut self) {
        let circular_deps = self.use_def_analysis.get_circular_dependencies();
        
        for cycle in circular_deps {
            if !cycle.is_empty() {
                self.errors.push(TypeCheckError::new(
                    format!("Circular dependency detected: {}", cycle.join(" -> ")),
                    Span::default()
                ));
            }
        }
    }

    pub fn get_errors(&self) -> &[TypeCheckError] {
        &self.errors
    }

    pub fn get_use_def_analysis(&self) -> &UseDefAnalysis {
        &self.use_def_analysis
    }

    pub fn get_type_environment(&self) -> &TypeEnvironment {
        &self.type_env
    }

    pub fn get_global_scope(&self) -> &Rc<Scope> {
        &self.global_scope
    }

    pub fn get_current_scope(&self) -> &Rc<Scope> {
        &self.current_scope
    }
}
