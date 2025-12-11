use std::rc::Rc;
use crate::utils::error::{CompilerError, CompilerResult};
use crate::utils::position::Span;
use crate::parser::ast::{Statement, Expression, Program};
use super::scope::{Scope, ScopeKind, Type, ScopeError};
use super::types::{TypeEnvironment, TypeEnvironmentError};
use super::use_def::{UseDefAnalysis, DefinitionKind};

pub type TypeCheckResult<T> = Result<T, TypeCheckError>;

#[derive(Debug, thiserror::Error)]
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
        // First pass: collect all top-level definitions
        for stmt in &program.statements {
            self.collect_top_level_definitions(stmt)?;
        }

        // Second pass: analyze each statement with full type information
        for stmt in &program.statements {
            self.check_statement(stmt)?;
        }

        // Check for unused definitions
        self.check_unused_definitions();

        // Check for circular dependencies
        self.check_circular_dependencies();

        if !self.errors.is_empty() {
            Err(self.errors.remove(0))
        } else {
            Ok(())
        }
    }

    pub fn collect_top_level_definitions(&mut self, stmt: &Statement) -> TypeCheckResult<()> {
        match stmt {
            Statement::Variable { mutable, name, type_annotation, value, .. } => {
                // Define variable in scope
                let type_ = if let Some(annotation) = type_annotation {
                    Type::from(annotation.clone())
                } else {
                    // Try to infer type from value
                    let value_type = self.infer_expression_type(value)?;
                    value_type
                };

                let current_scope = Rc::make_mut(&mut self.current_scope);
                current_scope.define_variable(name.clone(), type_, *mutable, stmt.span())?;

                // Record in use-def analysis
                self.use_def_analysis.define_variable(name.clone(), stmt.span());
            },

            Statement::Constant { name, type_annotation, value, .. } => {
                // Define constant in scope
                let type_ = if let Some(annotation) = type_annotation {
                    Type::from(annotation.clone())
                } else {
                    // Try to infer type from value
                    let value_type = self.infer_expression_type(value)?;
                    value_type
                };

                let current_scope = Rc::make_mut(&mut self.current_scope);
                current_scope.define_variable(name.clone(), type_, false, stmt.span())?;

                // Record in use-def analysis
                self.use_def_analysis.define_constant(name.clone(), stmt.span());
            },

            Statement::Function { name, parameters, return_type, body, .. } => {
                // Define function in scope
                let param_types: Vec<Type> = parameters
                    .iter()
                    .map(|p| {
                        p.type_annotation
                            .clone()
                            .unwrap_or_else(|| Type::Any)
                            .into()
                    })
                    .collect();

                let return_type = return_type
                    .as_ref()
                    .map(|rt| rt.clone().into())
                    .unwrap_or_else(|| Type::Void);

                let func_type = Type::Function(param_types.clone(), Box::new(return_type));

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
                    return_type,
                    stmt.span()
                )?;

                // Record in use-def analysis
                self.use_def_analysis.define_function(name.clone(), stmt.span());
            },

            Statement::Class { name, base, fields, methods, .. } => {
                // Define class as a type
                let mut field_types = std::collections::HashMap::new();
                
                for field in fields {
                    let field_type = field.type_annotation
                        .clone()
                        .unwrap_or_else(|| Type::Any)
                        .into();
                    field_types.insert(field.name.clone(), field_type);
                }

                let class_type = Type::Object(field_types);
                self.type_env.define_type(name.clone(), class_type)?;

                // Record in use-def analysis
                self.use_def_analysis.define_variable(name.clone(), stmt.span());

                // Handle base class dependency
                if let Some(base_name) = base {
                    if let Some(base_def) = self.use_def_analysis.lookup_definition(base_name) {
                        // Add dependency from derived class to base class
                        self.use_def_analysis.add_dependency(name, base_name);
                    }
                }

                // Define class methods
                for method in methods {
                    if let Statement::Function { name: method_name, .. } = method {
                        // Add dependency from class to method
                        self.use_def_analysis.add_dependency(name, method_name);
                    }
                }
            },

            Statement::Import { .. } => {
                // Imports are handled during use-def analysis
            },

            Statement::Block(statements) => {
                let block_scope = self.current_scope.enter_scope(ScopeKind::Block, stmt.span());
                let previous_scope = std::mem::replace(&mut self.current_scope, block_scope);

                for stmt in statements {
                    self.collect_top_level_definitions(stmt)?;
                }

                self.current_scope = previous_scope;
            },

            _ => {
                // Other statements don't define top-level symbols
            }
        }

        Ok(())
    }

    pub fn check_statement(&mut self, stmt: &Statement) -> TypeCheckResult<()> {
        match stmt {
            Statement::Variable { mutable, name, type_annotation, value, span } => {
                // Get variable type (either from annotation or inference)
                let expected_type = if let Some(annotation) = type_annotation {
                    Type::from(annotation.clone())
                } else {
                    // Infer type from value
                    let value_type = self.infer_expression_type(value)?;
                    let current_scope = Rc::make_mut(&mut self.current_scope);
                    current_scope.define_variable(name.clone(), value_type.clone(), *mutable, *span)?;
                    value_type
                };

                // Check value type matches expected type
                let actual_type = self.infer_expression_type(value)?;
                if !actual_type.is_compatible_with(&expected_type) {
                    self.errors.push(TypeCheckError::new(
                        format!("Expected type '{}', found type '{}'", expected_type.to_string(), actual_type.to_string()),
                        *span
                    ));
                    return Err(self.errors.last().unwrap().clone());
                }

                // Mark variable as used in use-def analysis
                self.use_def_analysis.mark_usage(name, *span);

                Ok(())
            },

            Statement::Constant { name, type_annotation, value, span } => {
                // Get constant type (either from annotation or inference)
                let expected_type = if let Some(annotation) = type_annotation {
                    Type::from(annotation.clone())
                } else {
                    // Infer type from value
                    let value_type = self.infer_expression_type(value)?;
                    let current_scope = Rc::make_mut(&mut self.current_scope);
                    current_scope.define_variable(name.clone(), value_type.clone(), false, *span)?;
                    value_type
                };

                // Check value type matches expected type
                let actual_type = self.infer_expression_type(value)?;
                if !actual_type.is_compatible_with(&expected_type) {
                    self.errors.push(TypeCheckError::new(
                        format!("Expected type '{}', found type '{}'", expected_type.to_string(), actual_type.to_string()),
                        *span
                    ));
                    return Err(self.errors.last().unwrap().clone());
                }

                // Mark constant as used in use-def analysis
                self.use_def_analysis.mark_usage(name, *span);

                Ok(())
            },

            Statement::Function { name, parameters, return_type, body, span, .. } => {
                // Enter function scope
                let function_scope = self.current_scope.enter_scope(ScopeKind::Function, *span);
                let previous_scope = std::mem::replace(&mut self.current_scope, function_scope);

                // Define parameters
                for param in parameters {
                    let param_type = param.type_annotation
                        .clone()
                        .unwrap_or_else(|| Type::Any)
                        .into();

                    let current_scope = Rc::make_mut(&mut self.current_scope);
                    current_scope.define_variable(param.name.clone(), param_type.clone(), false, param.span.unwrap_or(*span))?;

                    // Record parameter in use-def analysis
                    self.use_def_analysis.define_parameter(param.name.clone(), param.span.unwrap_or(*span));
                }

                // Check function body
                for stmt in body {
                    self.check_statement(stmt)?;
                }

                // Verify return type
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

                // Restore previous scope
                self.current_scope = previous_scope;

                // Mark function as used in use-def analysis
                self.use_def_analysis.mark_usage(name, *span);

                Ok(())
            },

            Statement::If { condition, then_branch, elif_branches, else_branch, span } => {
                // Check condition type
                let cond_type = self.infer_expression_type(condition)?;
                if !cond_type.is_boolean() && !cond_type.is_compatible_with(&Type::Any) {
                    self.errors.push(TypeCheckError::new(
                        format!("Condition must be boolean, found type '{}'", cond_type.to_string()),
                        condition.span()
                    ));
                    return Err(self.errors.last().unwrap().clone());
                }

                // Check then branch
                let if_scope = self.current_scope.enter_scope(ScopeKind::If, *span);
                let previous_scope = std::mem::replace(&mut self.current_scope, if_scope);

                for stmt in then_branch {
                    self.check_statement(stmt)?;
                }

                self.current_scope = previous_scope;

                // Check elif branches
                for (elif_cond, elif_body) in elif_branches {
                    // Check elif condition type
                    let elif_cond_type = self.infer_expression_type(elif_cond)?;
                    if !elif_cond_type.is_boolean() && !elif_cond_type.is_compatible_with(&Type::Any) {
                        self.errors.push(TypeCheckError::new(
                            format!("Elif condition must be boolean, found type '{}'", elif_cond_type.to_string()),
                            elif_cond.span()
                        ));
                        return Err(self.errors.last().unwrap().clone());
                    }

                    // Check elif body
                    let elif_scope = self.current_scope.enter_scope(ScopeKind::If, *span);
                    let elif_previous_scope = std::mem::replace(&mut self.current_scope, elif_scope);

                    for stmt in elif_body {
                        self.check_statement(stmt)?;
                    }

                    self.current_scope = elif_previous_scope;
                }

                // Check else branch if exists
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
                // Check condition type
                let cond_type = self.infer_expression_type(condition)?;
                if !cond_type.is_boolean() && !cond_type.is_compatible_with(&Type::Any) {
                    self.errors.push(TypeCheckError::new(
                        format!("While condition must be boolean, found type '{}'", cond_type.to_string()),
                        condition.span()
                    ));
                    return Err(self.errors.last().unwrap().clone());
                }

                // Check loop body
                let loop_scope = self.current_scope.enter_scope(ScopeKind::Loop, *span);
                let previous_scope = std::mem::replace(&mut self.current_scope, loop_scope);

                for stmt in body {
                    self.check_statement(stmt)?;
                }

                self.current_scope = previous_scope;

                Ok(())
            },

            Statement::For { variable, iterable, body, span } => {
                // Check iterable type
                let iterable_type = self.infer_expression_type(iterable)?;
                if !iterable_type.is_compatible_with(&Type::Array(Box::new(Type::Any))) {
                    self.errors.push(TypeCheckError::new(
                        format!("For loop iterable must be array, found type '{}'", iterable_type.to_string()),
                        iterable.span()
                    ));
                    return Err(self.errors.last().unwrap().clone());
                }

                // Define loop variable
                let current_scope = Rc::make_mut(&mut self.current_scope);
                current_scope.define_variable(variable.clone(), Type::Any, true, *span)?;

                // Check loop body
                let loop_scope = self.current_scope.enter_scope(ScopeKind::Loop, *span);
                let previous_scope = std::mem::replace(&mut self.current_scope, loop_scope);

                for stmt in body {
                    self.check_statement(stmt)?;
                }

                self.current_scope = previous_scope;

                // Mark loop variable as used
                self.use_def_analysis.mark_usage(variable, *span);

                Ok(())
            },

            Statement::Match { expr, arms, span } => {
                // Check expression type
                let expr_type = self.infer_expression_type(expr)?;

                // Check each arm
                let match_scope = self.current_scope.enter_scope(ScopeKind::Match, *span);
                let previous_scope = std::mem::replace(&mut self.current_scope, match_scope);

                for arm in arms {
                    // Check pattern type
                    let pattern_type = self.infer_expression_type(&arm.pattern)?;
                    if !pattern_type.is_compatible_with(&expr_type) {
                        self.errors.push(TypeCheckError::new(
                            format!("Pattern type '{}' does not match expression type '{}'", 
                                   pattern_type.to_string(), expr_type.to_string()),
                            arm.pattern.span()
                        ));
                        return Err(self.errors.last().unwrap().clone());
                    }

                    // Check guard if exists
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

                    // Check arm body
                    self.check_expression(&arm.body)?;
                }

                self.current_scope = previous_scope;

                Ok(())
            },

            Statement::Block(statements) => {
                let block_scope = self.current_scope.enter_scope(ScopeKind::Block, stmt.span());
                let previous_scope = std::mem::replace(&mut self.current_scope, block_scope);

                for stmt in statements {
                    self.check_statement(stmt)?;
                }

                self.current_scope = previous_scope;

                Ok(())
            },

            Statement::Expression(expr) => {
                self.check_expression(expr)
            },

            Statement::Return(value) => {
                if let Some(expr) = value {
                    self.check_expression(expr)?;
                }
                Ok(())
            },

            Statement::Class { name, base, fields, methods, span } => {
                // Define class type
                let mut field_types = std::collections::HashMap::new();
                
                for field in fields {
                    let field_type = field.type_annotation
                        .clone()
                        .unwrap_or_else(|| Type::Any)
                        .into();
                    field_types.insert(field.name.clone(), field_type);
                }

                let class_type = Type::Object(field_types);
                self.type_env.define_type(name.clone(), class_type)?;

                // Check base class if exists
                if let Some(base_name) = base {
                    if let Some(base_type) = self.type_env.lookup_type(base_name) {
                        // In a real implementation, we'd check inheritance compatibility
                        // For now, just mark the dependency
                        self.use_def_analysis.add_dependency(name, base_name);
                    } else {
                        self.errors.push(TypeCheckError::new(
                            format!("Base class '{}' not found", base_name),
                            *span
                        ));
                        return Err(self.errors.last().unwrap().clone());
                    }
                }

                // Check methods
                for method in methods {
                    self.check_statement(method)?;
                }

                Ok(())
            },

            Statement::Import { module, alias, items, span } => {
                // In a real implementation, we'd resolve the import
                // For now, just mark it as used in use-def analysis
                let import_name = alias.as_ref().unwrap_or(module);
                self.use_def_analysis.mark_usage(import_name, *span);

                // Handle specific item imports
                if let Some(items) = items {
                    for item in items {
                        let imported_name = item.name.clone();
                        let tracking_name = item.as_ref().map_or_else(
                            || imported_name.clone(),
                            |(_, alias)| alias.clone().unwrap_or_else(|| imported_name.clone())
                        );
                        
                        self.use_def_analysis.mark_usage(&tracking_name, *span);
                    }
                }

                Ok(())
            },
        }
    }

    pub fn check_expression(&mut self, expr: &Expression) -> TypeCheckResult<()> {
        match expr {
            Expression::Identifier(name) => {
                // Check if variable is defined
                if self.current_scope.lookup_variable(name).is_none() {
                    self.errors.push(TypeCheckError::new(
                        format!("Undefined variable '{}'", name),
                        expr.span()
                    ));
                    return Err(self.errors.last().unwrap().clone());
                }

                // Mark as used in use-def analysis
                self.use_def_analysis.mark_usage(name, expr.span());

                Ok(())
            },

            Expression::Literal(_) => {
                // Literals are always valid
                Ok(())
            },

            Expression::Binary { left, op, right } => {
                self.check_expression(left)?;
                self.check_expression(right)?;

                // Check operator compatibility
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
                        // Other operators are handled in type inference
                    }
                }

                Ok(())
            },

            Expression::Unary { op, expr } => {
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
                        // Other operators are handled in type inference
                    }
                }

                Ok(())
            },

            Expression::Call { function, arguments } => {
                self.check_expression(function)?;

                // Check function type
                let func_type = self.infer_expression_type(function)?;
                if let Type::Function(params, return_type) = func_type {
                    // Check argument count
                    if params.len() != arguments.len() {
                        self.errors.push(TypeCheckError::new(
                            format!("Expected {} arguments, got {}", params.len(), arguments.len()),
                            expr.span()
                        ));
                        return Err(self.errors.last().unwrap().clone());
                    }

                    // Check argument types
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

            Expression::Index { expr, index } => {
                self.check_expression(expr)?;
                self.check_expression(index)?;

                // Check array type
                let array_type = self.infer_expression_type(expr)?;
                if let Type::Array(_) = array_type {
                    // Check index type
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

            Expression::Member { expr, member } => {
                self.check_expression(expr)?;

                // Check object type
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

            Expression::Assignment { target, value, op } => {
                self.check_expression(target)?;
                self.check_expression(value)?;

                // Check target type
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

                // Check if assignment operator is valid
                if let Some(_) = op {
                    // Compound assignment - check if operation is valid
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
                            // Other operators may be valid for other types
                        }
                    }
                }

                Ok(())
            },

            Expression::Lambda { parameters, return_type, body } => {
                // Enter lambda scope
                let lambda_scope = self.current_scope.enter_scope(ScopeKind::Function, expr.span());
                let previous_scope = std::mem::replace(&mut self.current_scope, lambda_scope);

                // Define parameters
                for param in parameters {
                    let param_type = param.type_annotation
                        .clone()
                        .unwrap_or_else(|| Type::Any)
                        .into();

                    let current_scope = Rc::make_mut(&mut self.current_scope);
                    current_scope.define_variable(param.name.clone(), param_type, false, param.span.unwrap_or_else(|| expr.span()))?;
                }

                // Check lambda body
                self.check_statement(body)?;

                // Restore previous scope
                self.current_scope = previous_scope;

                Ok(())
            },

            Expression::If { condition, then_branch, else_branch } => {
                self.check_expression(condition)?;

                // Check condition type
                let cond_type = self.infer_expression_type(condition)?;
                if !cond_type.is_boolean() {
                    self.errors.push(TypeCheckError::new(
                        format!("If condition must be boolean, got type '{}'", cond_type.to_string()),
                        condition.span()
                    ));
                    return Err(self.errors.last().unwrap().clone());
                }

                // Check then branch
                self.check_expression(then_branch)?;

                // Check else branch if exists
                if let Some(else_branch) = else_branch {
                    self.check_expression(else_branch)?;
                }

                Ok(())
            },

            Expression::Match { expr, arms } => {
                self.check_expression(expr)?;

                // Check each arm
                for arm in arms {
                    self.check_expression(&arm.pattern)?;
                    
                    if let Some(guard) = &arm.guard {
                        self.check_expression(guard)?;
                    }
                    
                    self.check_expression(&arm.body)?;
                }

                Ok(())
            },
        }
    }

    pub fn infer_expression_type(&mut self, expr: &Expression) -> TypeCheckResult<Type> {
        self.type_env.infer_expression_type(expr)
            .map_err(|e| TypeCheckError::TypeEnvironment(e))
    }

    pub fn infer_statement_return_type(&mut self, statements: &[Statement]) -> TypeCheckResult<Type> {
        let mut return_type = Type::Void;

        for stmt in statements {
            if let Statement::Return(value) = stmt {
                if let Some(expr) = value {
                    return_type = self.infer_expression_type(expr)?;
                } else {
                    return_type = Type::Void;
                }
                break;
            } else if let Statement::Block(inner_statements) = stmt {
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
                    Span::default() // In a real implementation, would track actual spans
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
