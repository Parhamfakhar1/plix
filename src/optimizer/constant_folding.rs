use crate::parser::ast::{Program, Statement, Expression, BinaryOp, UnaryOp, Literal};
use crate::utils::error::{CompilerResult, CompilerError};
use crate::utils::position::Span;
use super::Optimizer;

pub struct ConstantFoldingOptimizer {
    constant_values: std::collections::HashMap<String, Literal>,
}

impl ConstantFoldingOptimizer {
    pub fn new() -> Self {
        Self {
            constant_values: std::collections::HashMap::new(),
        }
    }

    fn evaluate_binary_op(&self, op: &BinaryOp, left: &Literal, right: &Literal) -> Option<Literal> {
        use Literal::*;
        
        match (op, left, right) {
            (BinaryOp::Add, Integer(a), Integer(b)) => Some(Integer(a + b)),
            (BinaryOp::Add, Integer(a), Float(b)) => Some.Float(*a as f64 + b),
            (BinaryOp::Add, Float(a), Integer(b)) => Some.Float(a + *b as f64),
            (BinaryOp::Add, Float(a), Float(b)) => Some.Float(a + b),
            
            (BinaryOp::Subtract, Integer(a), Integer(b)) => Some.Integer(a - b),
            (BinaryOp::Subtract, Integer(a), Float(b)) => Some.Float(*a as f64 - b),
            (BinaryOp::Subtract, Float(a), Integer(b)) => Some.Float(a - *b as f64),
            (BinaryOp::Subtract, Float(a), Float(b)) => Some.Float(a - b),
            
            (BinaryOp::Multiply, Integer(a), Integer(b)) => Some.Integer(a * b),
            (BinaryOp::Multiply, Integer(a), Float(b)) => Some.Float(*a as f64 * b),
            (BinaryOp::Multiply, Float(a), Integer(b)) => Some.Float(a * *b as f64),
            (BinaryOp::Multiply, Float(a), Float(b)) => Some.Float(a * b),
            
            (BinaryOp::Divide, Integer(a), Integer(b)) => {
                if *b != 0 {
                    Some.Float(*a as f64 / *b as f64)
                } else {
                    None
                }
            },
            (BinaryOp::Divide, Integer(a), Float(b)) => {
                if *b != 0.0 {
                    Some.Float(*a as f64 / b)
                } else {
                    None
                }
            },
            (BinaryOp::Divide, Float(a), Integer(b)) => {
                if *b != 0 {
                    Some.Float(a / *b as f64)
                } else {
                    None
                }
            },
            (BinaryOp::Divide, Float(a), Float(b)) => {
                if *b != 0.0 {
                    Some.Float(a / b)
                } else {
                    None
                }
            },
            
            (BinaryOp::Modulo, Integer(a), Integer(b)) => {
                if *b != 0 {
                    Some.Integer(a % b)
                } else {
                    None
                }
            },
            
            (BinaryOp::BitAnd, Integer(a), Integer(b)) => Some.Integer(a & b),
            (BinaryOp::BitOr, Integer(a), Integer(b)) => Some.Integer(a | b),
            (BinaryOp::BitXor, Integer(a), Integer(b)) => Some.Integer(a ^ b),
            (BinaryOp::ShiftLeft, Integer(a), Integer(b)) => Some.Integer(a << b),
            (BinaryOp::ShiftRight, Integer(a), Integer(b)) => Some.Integer(a >> b),
            
            (BinaryOp::And, Boolean(a), Boolean(b)) => Some.Boolean(*a && *b),
            (BinaryOp::Or, Boolean(a), Boolean(b)) => Some.Boolean(*a || *b),
            
            (BinaryOp::Equal, Integer(a), Integer(b)) => Some.Boolean(a == b),
            (BinaryOp::Equal, Float(a), Float(b)) => Some.Boolean(a == b),
            (BinaryOp::Equal, String(a), String(b)) => Some.Boolean(a == b),
            (BinaryOp::Equal, Boolean(a), Boolean(b)) => Some.Boolean(*a == *b),
            (BinaryOp::Equal, Null, Null) => Some.Boolean(true),
            (BinaryOp::Equal, Undefined, Undefined) => Some.Boolean(true),
            
            (BinaryOp::NotEqual, Integer(a), Integer(b)) => Some.Boolean(a != b),
            (BinaryOp::NotEqual, Float(a), Float(b)) => Some.Boolean(a != b),
            (BinaryOp::NotEqual, String(a), String(b)) => Some.Boolean(a != b),
            (BinaryOp::NotEqual, Boolean(a), Boolean(b)) => Some.Boolean(*a != *b),
            (BinaryOp::NotEqual, Null, Null) => Some.Boolean(false),
            (BinaryOp::NotEqual, Undefined, Undefined) => Some.Boolean(false),
            
            (BinaryOp::Less, Integer(a), Integer(b)) => Some.Boolean(*a < *b),
            (BinaryOp::Less, Float(a), Float(b)) => Some.Boolean(*a < *b),
            (BinaryOp::Less, String(a), String(b)) => Some.Boolean(*a < *b),
            
            (BinaryOp::LessEqual, Integer(a), Integer(b)) => Some.Boolean(*a <= *b),
            (BinaryOp::LessEqual, Float(a), Float(b)) => Some.Boolean(*a <= *b),
            (BinaryOp::LessEqual, String(a), String(b)) => Some.Boolean(*a <= *b),
            
            (BinaryOp::Greater, Integer(a), Integer(b)) => Some.Boolean(*a > *b),
            (BinaryOp::Greater, Float(a), Float(b)) => Some.Boolean(*a > *b),
            (BinaryOp::Greater, String(a), String(b)) => Some.Boolean(*a > *b),
            
            (BinaryOp::GreaterEqual, Integer(a), Integer(b)) => Some.Boolean(*a >= *b),
            (BinaryOp::GreaterEqual, Float(a), Float(b)) => Some.Boolean(*a >= *b),
            (BinaryOp::GreaterEqual, String(a), String(b)) => Some.Boolean(*a >= *b),
            
            (BinaryOp::Range, Integer(a), Integer(b)) => {
                let start = *a;
                let end = *b;
                let mut range = Vec::new();
                for i in start..end {
                    range.push(Expression::Literal.Integer(i));
                }
                Some(Literal::Array(range))
            },
            
            _ => None,
        }
    }

    fn evaluate_unary_op(&self, op: &UnaryOp, expr: &Literal) -> Option<Literal> {
        use Literal::*;
        
        match (op, expr) {
            (UnaryOp::Plus, Integer(a)) => Some.Integer(*a),
            (UnaryOp::Plus, Float(a)) => Some.Float(*a),
            (UnaryOp::Minus, Integer(a)) => Some.Integer(-*a),
            (UnaryOp::Minus, Float(a)) => Some.Float(-*a),
            (UnaryOp::Not, Boolean(b)) => Some.Boolean(!*b),
            (UnaryOp::BitNot, Integer(a)) => Some.Integer(!*a),
            _ => None,
        }
    }

    fn fold_constants_in_expression(&mut self, expr: &mut Expression) -> CompilerResult<()> {
        match expr {
            Expression::Binary { left, op, right } => {
                self.fold_constants_in_expression(left)?;
                self.fold_constants_in_expression(right)?;
                
                if let (Some(left_val), Some(right_val)) = 
                    (self.get_literal_value(left), self.get_literal_value(right)) {
                    
                    if let Some(result) = self.evaluate_binary_op(op, left_val, right_val) {
                        *expr = Expression::Literal(result);
                        return Ok(());
                    }
                }
            },
            
            Expression::Unary { op, expr } => {
                self.fold_constants_in_expression(expr)?;
                
                if let Some(val) = self.get_literal_value(expr) {
                    if let Some(result) = self.evaluate_unary_op(op, val) {
                        *expr = Expression::Literal(result);
                        return Ok(());
                    }
                }
            },
            
            Expression::Call { function, arguments } => {
                self.fold_constants_in_expression(function)?;
                for arg in arguments {
                    self.fold_constants_in_expression(arg)?;
                }
            },
            
            Expression::Index { expr, index } => {
                self.fold_constants_in_expression(expr)?;
                self.fold_constants_in_expression(index)?;
            },
            
            Expression::Member { expr, member } => {
                self.fold_constants_in_expression(expr)?;
            },
            
            Expression::Assignment { target, value, op } => {
                self.fold_constants_in_expression(target)?;
                self.fold_constants_in_expression(value)?;
                
                if let (Some(assignment_op), Some(target_val), Some(value_val)) = 
                    (op.as_ref(), self.get_literal_value(target), self.get_literal_value(value)) {
                    
                    if let Some(result) = self.evaluate_binary_op(assignment_op, target_val, value_val) {
                        *value = Expression::Literal(result);
                    }
                }
            },
            
            Expression::Lambda { parameters, return_type, body } => {
                self.fold_constants_in_statement(body)?;
            },
            
            Expression::If { condition, then_branch, else_branch } => {
                self.fold_constants_in_expression(condition)?;
                self.fold_constants_in_expression(then_branch)?;
                
                if let Some(else_branch) = else_branch {
                    self.fold_constants_in_expression(else_branch)?;
                }
            },
            
            Expression::Match { expr, arms } => {
                self.fold_constants_in_expression(expr)?;
                
                for arm in arms {
                    self.fold_constants_in_expression(&arm.pattern)?;
                    if let Some(guard) = &mut arm.guard {
                        self.fold_constants_in_expression(guard)?;
                    }
                    self.fold_constants_in_expression(&mut arm.body)?;
                }
            },
            
            Expression::Identifier(_) | Expression::Literal(_) => {
            },
        }
        
        Ok(())
    }

    fn fold_constants_in_statement(&mut self, stmt: &mut Statement) -> CompilerResult<()> {
        match stmt {
            Statement::Expression(expr) => {
                self.fold_constants_in_expression(expr)?;
            },
            
            Statement::Variable { mutable: _, name, type_annotation, value } => {
                self.fold_constants_in_expression(value)?;
            },
            
            Statement::Constant { name, type_annotation, value } => {
                self.fold_constants_in_expression(value)?;
            },
            
            Statement::Return(value) => {
                if let Some(expr) = value {
                    self.fold_constants_in_expression(expr)?;
                }
            },
            
            Statement::If { condition, then_branch, elif_branches, else_branch } => {
                self.fold_constants_in_expression(condition)?;
                
                for stmt in then_branch {
                    self.fold_constants_in_statement(stmt)?;
                }
                
                for (elif_cond, elif_body) in elif_branches {
                    self.fold_constants_in_expression(elif_cond)?;
                    for stmt in elif_body {
                        self.fold_constants_in_statement(stmt)?;
                    }
                }
                
                if let Some(else_body) = else_branch {
                    for stmt in else_body {
                        self.fold_constants_in_statement(stmt)?;
                    }
                }
            },
            
            Statement::While { condition, body } => {
                self.fold_constants_in_expression(condition)?;
                
                for stmt in body {
                    self.fold_constants_in_statement(stmt)?;
                }
            },
            
            Statement::For { variable, iterable, body } => {
                self.fold_constants_in_expression(iterable)?;
                
                for stmt in body {
                    self.fold_constants_in_statement(stmt)?;
                }
            },
            
            Statement::Match { expr, arms } => {
                self.fold_constants_in_expression(expr)?;
                
                for arm in arms {
                    self.fold_constants_in_expression(&arm.pattern)?;
                    if let Some(guard) = &mut arm.guard {
                        self.fold_constants_in_expression(guard)?;
                    }
                    self.fold_constants_in_statement(&mut Statement::Expression(arm.body.clone()))?;
                }
            },
            
            Statement::Block(statements) => {
                for stmt in statements {
                    self.fold_constants_in_statement(stmt)?;
                }
            },
            
            Statement::Function { name, parameters, return_type, body, async_flag } => {
            },
            
            Statement::Class { name, base, fields, methods } => {
            },
            
            Statement::Import { module, alias, items } => {
            },
        }
        
        Ok(())
    }

    fn get_literal_value(&self, expr: &Expression) -> Option<&Literal> {
        match expr {
            Expression::Literal(literal) => Some(literal),
            _ => None,
        }
    }

    fn is_constant_expression(&self, expr: &Expression) -> bool {
        matches!(expr, Expression::Literal(_))
    }
}

impl Optimizer for ConstantFoldingOptimizer {
    fn optimize_program(&mut self, program: &mut Program) -> CompilerResult<()> {
        for stmt in &mut program.statements {
            self.fold_constants_in_statement(stmt)?;
        }
        Ok(())
    }

    fn optimize_statement(&mut self, stmt: &mut Statement) -> CompilerResult<()> {
        self.fold_constants_in_statement(stmt)
    }

    fn optimize_expression(&mut self, expr: &mut Expression) -> CompilerResult<()> {
        self.fold_constants_in_expression(expr)
    }
}
