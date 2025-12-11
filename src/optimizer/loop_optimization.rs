use crate::parser::ast::{Program, Statement, Expression, BinaryOp, Literal};
use crate::utils::error::{CompilerResult, CompilerError};
use crate::utils::position::Span;
use super::Optimizer;

pub struct LoopOptimizer {
    // Track loop invariants and other optimization opportunities
    loop_invariants: std::collections::HashMap<String, Expression>,
}

impl LoopOptimizer {
    pub fn new() -> Self {
        Self {
            loop_invariants: std::collections::HashMap::new(),
        }
    }

    fn is_loop_invariant(&self, expr: &Expression, loop_depth: usize) -> bool {
        match expr {
            Expression::Identifier(_) => {
                // Check if this variable is modified in the loop
                // This is a simplified check - in a real implementation,
                // we'd track variable modifications per loop
                true
            },
            Expression::Binary { left, op: _, right } => {
                self.is_loop_invariant(left, loop_depth) && self.is_loop_invariant(right, loop_depth)
            },
            Expression::Unary { op: _, expr } => {
                self.is_loop_invariant(expr, loop_depth)
            },
            Expression::Call { function, arguments } => {
                self.is_loop_invariant(function, loop_depth) && 
                arguments.iter().all(|arg| self.is_loop_invariant(arg, loop_depth))
            },
            Expression::Index { expr, index } => {
                self.is_loop_invariant(expr, loop_depth) && self.is_loop_invariant(index, loop_depth)
            },
            Expression::Member { expr, .. } => {
                self.is_loop_invariant(expr, loop_depth)
            },
            Expression::Assignment { target, value, op: _ } => {
                // Check if the target is being assigned to in the loop
                // This is a simplified check
                !self.is_loop_invariant(target, loop_depth) && 
                self.is_loop_invariant(value, loop_depth)
            },
            Expression::Lambda { parameters, return_type: _, body } => {
                // Lambdas can't be loop invariants in this simplified version
                false
            },
            Expression::If { condition, then_branch, else_branch } => {
                self.is_loop_invariant(condition, loop_depth) && 
                self.is_loop_invariant(then_branch, loop_depth) && 
                else_branch.as_ref().map_or(true, |branch| self.is_loop_invariant(branch, loop_depth))
            },
            Expression::Match { expr, arms } => {
                self.is_loop_invariant(expr, loop_depth) && 
                arms.iter().all(|arm| {
                    self.is_loop_invariant(&arm.pattern, loop_depth) && 
                    arm.guard.as_ref().map_or(true, |guard| self.is_loop_invariant(guard, loop_depth)) && 
                    self.is_loop_invariant(&arm.body, loop_depth)
                })
            },
            Expression::Literal(_) => true,
        }
    }

    fn optimize_loop_condition(&mut self, condition: &mut Expression, loop_depth: usize) {
        // Try to simplify loop conditions
        if let Expression::Binary { left, op, right } = condition {
            match op {
                BinaryOp::NotEqual | BinaryOp::Less | BinaryOp::LessEqual | 
                BinaryOp::Greater | BinaryOp::GreaterEqual => {
                    // Check if we can detect infinite loops or loops that never run
                    if let (Expression::Literal(Literal::Integer(left_val)), 
                           Expression::Literal(Literal::Integer(right_val))) = 
                           (left.as_ref(), right.as_ref()) {
                        
                        match op {
                            BinaryOp::NotEqual => {
                                // If left != right is always true, it's an infinite loop
                                if left_val != right_val {
                                    // Could mark as infinite loop
                                }
                            },
                            BinaryOp::Less => {
                                // If left < right is always false, loop never runs
                                if left_val >= right_val {
                                    // Could mark as dead code
                                }
                            },
                            _ => {}
                        }
                    }
                },
                _ => {}
            }
        }
    }

    fn optimize_loop_body(&mut self, statements: &mut Vec<Statement>, loop_depth: usize) {
        // Remove dead code from loops
        statements.retain_mut(|stmt| {
            match stmt {
                Statement::Expression(expr) => {
                    // Keep expression statements
                    true
                },
                Statement::Variable { mutable: _, name, type_annotation, value } => {
                    // If the variable is never used, remove it
                    // This is a simplified check - in a real implementation,
                    // we'd track variable usage
                    true
                },
                Statement::Constant { name, type_annotation, value } => {
                    // Constants are always kept
                    true
                },
                Statement::Return(value) => {
                    // Return statements exit the loop
                    true
                },
                Statement::If { condition, then_branch, elif_branches, else_branch } => {
                    // Optimize each branch
                    self.optimize_loop_body(then_branch, loop_depth);
                    
                    for (_, elif_body) in elif_branches {
                        self.optimize_loop_body(elif_body, loop_depth);
                    }
                    
                    if let Some(else_body) = else_branch {
                        self.optimize_loop_body(else_body, loop_depth);
                    }
                    
                    // Keep if statements that might execute
                    true
                },
                Statement::While { condition, body } => {
                    // Nested loop
                    self.optimize_loop_condition(condition, loop_depth + 1);
                    self.optimize_loop_body(body, loop_depth + 1);
                    !body.is_empty()
                },
                Statement::For { variable, iterable, body } => {
                    // Nested loop
                    self.optimize_loop_body(body, loop_depth + 1);
                    !body.is_empty()
                },
                Statement::Match { expr, arms } => {
                    // Optimize each arm
                    for arm in arms {
                        self.optimize_loop_body(&mut vec![Statement::Expression(arm.body.clone())], loop_depth);
                    }
                    !arms.is_empty()
                },
                Statement::Block(statements) => {
                    self.optimize_loop_body(statements, loop_depth);
                    !statements.is_empty()
                },
                Statement::Function { name, parameters, return_type, body, async_flag } => {
                    // Don't optimize function definitions inside loops
                    // This could change semantics
                    true
                },
                Statement::Class { name, base, fields, methods } => {
                    // Don't optimize class definitions inside loops
                    // This could change semantics
                    true
                },
                Statement::Import { module, alias, items } => {
                    // Imports are always kept
                    true
                },
            }
        });
    }

    // Detect and eliminate loop invariants
    fn eliminate_loop_invariants(&mut self, statements: &mut Vec<Statement>, loop_depth: usize) {
        let mut invariant_expressions = Vec::new();
        
        // First pass: find invariant expressions
        for stmt in statements {
            if let Statement::Expression(expr) = stmt {
                if self.is_loop_invariant(expr, loop_depth) {
                    invariant_expressions.push(expr.clone());
                }
            }
        }
        
        // Second pass: move invariants out of the loop if possible
        // This is a simplified version - in a real implementation,
        // we'd need to be more careful about scope and side effects
        statements.retain_mut(|stmt| {
            match stmt {
                Statement::Expression(expr) => {
                    // Keep only non-invariant expressions in the loop
                    !self.is_loop_invariant(expr, loop_depth)
                },
                _ => true,
            }
        });
    }

    // Detect and eliminate empty loops
    fn eliminate_empty_loops(&mut self, statements: &mut Vec<Statement>) {
        statements.retain_mut(|stmt| {
            match stmt {
                Statement::While { condition, body } => {
                    // Remove empty while loops
                    if body.is_empty() {
                        false
                    } else {
                        // Check if condition is always false
                        if let Expression::Binary { left, op, right } = condition.as_ref() {
                            if let (Expression::Literal(Literal::Boolean(left_val)), 
                                   Expression::Literal(Literal::Boolean(right_val))) = 
                                   (left.as_ref(), right.as_ref()) {
                                if let BinaryOp::And = op {
                                    if !(*left_val && *right_val) {
                                        return false; // Condition is always false
                                    }
                                }
                            }
                        }
                        true
                    }
                },
                Statement::For { variable, iterable, body } => {
                    // Remove empty for loops
                    !body.is_empty()
                },
                _ => true,
            }
        });
    }
}

impl Optimizer for LoopOptimizer {
    fn optimize_program(&mut self, program: &mut Program) -> CompilerResult<()> {
        for stmt in &mut program.statements {
            self.optimize_statement(stmt)?;
        }
        
        // Eliminate empty loops at the program level
        self.eliminate_empty_loops(&mut program.statements);
        
        Ok(())
    }

    fn optimize_statement(&mut self, stmt: &mut Statement) -> CompilerResult<()> {
        match stmt {
            Statement::While { condition, body } => {
                // Optimize loop condition
                self.optimize_loop_condition(condition, 0);
                
                // Optimize loop body
                self.optimize_loop_body(body, 1);
                
                // Eliminate loop invariants
                self.eliminate_loop_invariants(body, 1);
                
                // Eliminate empty loops
                self.eliminate_empty_loops(body);
            },
            
            Statement::For { variable, iterable, body } => {
                // Optimize loop body
                self.optimize_loop_body(body, 1);
                
                // Eliminate loop invariants
                self.eliminate_loop_invariants(body, 1);
                
                // Eliminate empty loops
                self.eliminate_empty_loops(body);
            },
            
            Statement::If { condition, then_branch, elif_branches, else_branch } => {
                // Optimize condition
                if let Expression::Binary { left, op, right } = condition {
                    // Try to simplify boolean conditions
                    if let (Expression::Literal(Literal::Boolean(left_val)), 
                           Expression::Literal(Literal::Boolean(right_val))) = 
                           (left.as_ref(), right.as_ref()) {
                        
                        match op {
                            BinaryOp::And => {
                                if !(*left_val && *right_val) {
                                    // Short-circuit - if left is false, whole expression is false
                                    *condition = Expression::Literal(Literal::Boolean(false));
                                }
                            },
                            BinaryOp::Or => {
                                if *left_val || *right_val {
                                    // Short-circuit - if left is true, whole expression is true
                                    *condition = Expression::Literal(Literal::Boolean(true));
                                }
                            },
                            _ => {}
                        }
                    }
                }
                
                // Optimize branches
                self.optimize_loop_body(then_branch, 0);
                
                for (_, elif_body) in elif_branches {
                    self.optimize_loop_body(elif_body, 0);
                }
                
                if let Some(else_body) = else_branch {
                    self.optimize_loop_body(else_body, 0);
                }
            },
            
            Statement::Block(statements) => {
                // Optimize statements in block
                for stmt in statements {
                    self.optimize_statement(stmt)?;
                }
                
                // Eliminate empty loops in the block
                self.eliminate_empty_loops(statements);
            },
            
            _ => {}
        }
        
        Ok(())
    }

    fn optimize_expression(&mut self, expr: &mut Expression) -> CompilerResult<()> {
        // Loop optimization primarily works at the statement level
        // This method is kept for completeness
        Ok(())
    }
}
