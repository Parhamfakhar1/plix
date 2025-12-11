use crate::parser::ast::{Program, Statement, Expression, BinaryOp, Literal};
use crate::utils::error::{CompilerResult, CompilerError};
use crate::utils::position::Span;
use super::Optimizer;

pub struct LoopOptimizer {
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
                !self.is_loop_invariant(target, loop_depth) && 
                self.is_loop_invariant(value, loop_depth)
            },
            Expression::Lambda { parameters, return_type: _, body } => {
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
        if let Expression::Binary { left, op, right } = condition {
            match op {
                BinaryOp::NotEqual | BinaryOp::Less | BinaryOp::LessEqual | 
                BinaryOp::Greater | BinaryOp::GreaterEqual => {
                    if let (Expression::Literal(Literal::Integer(left_val)), 
                           Expression::Literal(Literal::Integer(right_val))) = 
                           (left.as_ref(), right.as_ref()) {
                        
                        match op {
                            BinaryOp::NotEqual => {
                                if left_val != right_val {
                                }
                            },
                            BinaryOp::Less => {
                                if left_val >= right_val {
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
        statements.retain_mut(|stmt| {
            match stmt {
                Statement::Expression(expr) => {
                    true
                },
                Statement::Variable { mutable: _, name, type_annotation, value } => {
                    true
                },
                Statement::Constant { name, type_annotation, value } => {
                    true
                },
                Statement::Return(value) => {
                    true
                },
                Statement::If { condition, then_branch, elif_branches, else_branch } => {
                    self.optimize_loop_body(then_branch, loop_depth);
                    
                    for (_, elif_body) in elif_branches {
                        self.optimize_loop_body(elif_body, loop_depth);
                    }
                    
                    if let Some(else_body) = else_branch {
                        self.optimize_loop_body(else_body, loop_depth);
                    }
                    
                    true
                },
                Statement::While { condition, body } => {
                    self.optimize_loop_condition(condition, loop_depth + 1);
                    self.optimize_loop_body(body, loop_depth + 1);
                    !body.is_empty()
                },
                Statement::For { variable, iterable, body } => {
                    self.optimize_loop_body(body, loop_depth + 1);
                    !body.is_empty()
                },
                Statement::Match { expr, arms } => {
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
                    true
                },
                Statement::Class { name, base, fields, methods } => {
                    true
                },
                Statement::Import { module, alias, items } => {
                    true
                },
            }
        });
    }

    fn eliminate_loop_invariants(&mut self, statements: &mut Vec<Statement>, loop_depth: usize) {
        let mut invariant_expressions = Vec::new();
        
        for stmt in statements {
            if let Statement::Expression(expr) = stmt {
                if self.is_loop_invariant(expr, loop_depth) {
                    invariant_expressions.push(expr.clone());
                }
            }
        }
        
        statements.retain_mut(|stmt| {
            match stmt {
                Statement::Expression(expr) => {
                    !self.is_loop_invariant(expr, loop_depth)
                },
                _ => true,
            }
        });
    }

    fn eliminate_empty_loops(&mut self, statements: &mut Vec<Statement>) {
        statements.retain_mut(|stmt| {
            match stmt {
                Statement::While { condition, body } => {
                    if body.is_empty() {
                        false
                    } else {
                        if let Expression::Binary { left, op, right } = condition.as_ref() {
                            if let (Expression::Literal(Literal::Boolean(left_val)), 
                                   Expression::Literal(Literal::Boolean(right_val))) = 
                                   (left.as_ref(), right.as_ref()) {
                                if let BinaryOp::And = op {
                                    if !(*left_val && *right_val) {
                                    }
                                }
                            }
                        }
                        true
                    }
                },
                Statement::For { variable, iterable, body } => {
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
        
        self.eliminate_empty_loops(&mut program.statements);
        
        Ok(())
    }

    fn optimize_statement(&mut self, stmt: &mut Statement) -> CompilerResult<()> {
        match stmt {
            Statement::While { condition, body } => {
                self.optimize_loop_condition(condition, 0);
                
                self.optimize_loop_body(body, 1);
                
                self.eliminate_loop_invariants(body, 1);
                
                self.eliminate_empty_loops(body);
            },
            
            Statement::For { variable, iterable, body } => {
                self.optimize_loop_body(body, 1);
                
                self.eliminate_loop_invariants(body, 1);
                
                self.eliminate_empty_loops(body);
            },
            
            Statement::If { condition, then_branch, elif_branches, else_branch } => {
                if let Expression::Binary { left, op, right } = condition {
                    if let (Expression::Literal(Literal::Boolean(left_val)), 
                           Expression::Literal(Literal::Boolean(right_val))) = 
                           (left.as_ref(), right.as_ref()) {
                        
                        match op {
                            BinaryOp::And => {
                                if !(*left_val && *right_val) {
                                    *condition = Expression::Literal(Literal::Boolean(false));
                                }
                            },
                            BinaryOp::Or => {
                                if *left_val || *right_val {
                                    *condition = Expression::Literal(Literal::Boolean(true));
                                }
                            },
                            _ => {}
                        }
                    }
                }
                
                self.optimize_loop_body(then_branch, 0);
                
                for (_, elif_body) in elif_branches {
                    self.optimize_loop_body(elif_body, 0);
                }
                
                if let Some(else_body) = else_branch {
                    self.optimize_loop_body(else_body, 0);
                }
            },
            
            Statement::Block(statements) => {
                for stmt in statements {
                    self.optimize_statement(stmt)?;
                }
                
                self.eliminate_empty_loops(statements);
            },
            
            _ => {}
        }
        
        Ok(())
    }

    fn optimize_expression(&mut self, expr: &mut Expression) -> CompilerResult<()> {
        Ok(())
    }
}
