use std::collections::HashSet;
use crate::parser::ast::{Program, Statement, Expression};
use crate::utils::error::{CompilerResult, CompilerError};
use crate::utils::position::Span;
use super::Optimizer;

pub struct TreeShakingOptimizer {
    used_symbols: HashSet<String>,
    symbol_stack: Vec<String>,
}

impl TreeShakingOptimizer {
    pub fn new() -> Self {
        Self {
            used_symbols: HashSet::new(),
            symbol_stack: Vec::new(),
        }
    }

    pub fn mark_symbol_used(&mut self, symbol: &str) {
        self.used_symbols.insert(symbol.to_string());
    }

    fn is_symbol_used(&self, symbol: &str) -> bool {
        self.used_symbols.contains(symbol)
    }

    fn collect_used_symbols(&mut self, expr: &Expression) {
        match expr {
            Expression::Identifier(name) => {
                self.mark_symbol_used(name);
            },
            Expression::Binary { left, op: _, right } => {
                self.collect_used_symbols(left);
                self.collect_used_symbols(right);
            },
            Expression::Unary { op: _, expr } => {
                self.collect_used_symbols(expr);
            },
            Expression::Call { function, arguments } => {
                self.collect_used_symbols(function);
                for arg in arguments {
                    self.collect_used_symbols(arg);
                }
            },
            Expression::Index { expr, index } => {
                self.collect_used_symbols(expr);
                self.collect_used_symbols(index);
            },
            Expression::Member { expr, member } => {
                self.collect_used_symbols(expr);
                self.mark_symbol_used(member);
            },
            Expression::Assignment { target, value, op: _ } => {
                self.collect_used_symbols(target);
                self.collect_used_symbols(value);
            },
            Expression::Lambda { parameters, return_type: _, body } => {
                for param in parameters {
                    self.symbol_stack.push(param.name.clone());
                }
                self.collect_used_symbols_from_statement(body);
                for param in parameters {
                    self.symbol_stack.pop();
                }
            },
            Expression::If { condition, then_branch, else_branch } => {
                self.collect_used_symbols(condition);
                self.collect_used_symbols(then_branch);
                if let Some(else_branch) = else_branch {
                    self.collect_used_symbols(else_branch);
                }
            },
            Expression::Match { expr, arms } => {
                self.collect_used_symbols(expr);
                for arm in arms {
                    self.collect_used_symbols(&arm.pattern);
                    if let Some(guard) = &arm.guard {
                        self.collect_used_symbols(guard);
                    }
                    self.collect_used_symbols(&arm.body);
                }
            },
            Expression::Literal(_) => {},
        }
    }

    fn collect_used_symbols_from_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Expression(expr) => {
                self.collect_used_symbols(expr);
            },
            Statement::Variable { mutable: _, name, value, .. } => {
                self.collect_used_symbols(value);
                self.mark_symbol_used(name);
            },
            Statement::Constant { name, value, .. } => {
                self.collect_used_symbols(value);
                self.mark_symbol_used(name);
            },
            Statement::Return(value) => {
                if let Some(expr) = value {
                    self.collect_used_symbols(expr);
                }
            },
            Statement::If { condition, then_branch, elif_branches, else_branch } => {
                self.collect_used_symbols(condition);
                for stmt in then_branch {
                    self.collect_used_symbols_from_statement(stmt);
                }
                for (elif_cond, elif_body) in elif_branches {
                    self.collect_used_symbols(elif_cond);
                    for stmt in elif_body {
                        self.collect_used_symbols_from_statement(stmt);
                    }
                }
                if let Some(else_body) = else_branch {
                    for stmt in else_body {
                        self.collect_used_symbols_from_statement(stmt);
                    }
                }
            },
            Statement::While { condition, body } => {
                self.collect_used_symbols(condition);
                for stmt in body {
                    self.collect_used_symbols_from_statement(stmt);
                }
            },
            Statement::For { variable, iterable, body } => {
                self.collect_used_symbols(iterable);
                self.mark_symbol_used(variable);
                for stmt in body {
                    self.collect_used_symbols_from_statement(stmt);
                }
            },
            Statement::Match { expr, arms } => {
                self.collect_used_symbols(expr);
                for arm in arms {
                    self.collect_used_symbols(&arm.pattern);
                    if let Some(guard) = &arm.guard {
                        self.collect_used_symbols(guard);
                    }
                    self.collect_used_symbols_from_statement(&Statement::Expression(arm.body.clone()));
                }
            },
            Statement::Block(statements) => {
                for stmt in statements {
                    self.collect_used_symbols_from_statement(stmt);
                }
            },
            Statement::Function { name, parameters, body, .. } => {
                self.mark_symbol_used(name);
                for param in parameters {
                    self.symbol_stack.push(param.name.clone());
                }
                for stmt in body {
                    self.collect_used_symbols_from_statement(stmt);
                }
                for param in parameters {
                    self.symbol_stack.pop();
                }
            },
            Statement::Class { name, base, fields, methods } => {
                self.mark_symbol_used(name);
                if let Some(base_name) = base {
                    self.mark_symbol_used(base_name);
                }
                for field in fields {
                    self.mark_symbol_used(&field.name);
                }
                for method in methods {
                    self.collect_used_symbols_from_statement(method);
                }
            },
            Statement::Import { module, alias, items } => {
                self.mark_symbol_used(module);
                if let Some(alias_name) = alias {
                    self.mark_symbol_used(alias_name);
                }
                if let Some(items) = items {
                    for item in items {
                        self.mark_symbol_used(&item.name);
                        if let Some(alias_name) = &item.alias {
                            self.mark_symbol_used(alias_name);
                        }
                    }
                }
            },
        }
    }

    fn remove_unused_statements(&mut self, statements: &mut Vec<Statement>) {
        statements.retain_mut(|stmt| {
            match stmt {
                Statement::Expression(expr) => {
                    self.collect_used_symbols(expr);
                    true
                },
                Statement::Variable { mutable, name, type_annotation, value } => {
                    if self.is_symbol_used(name) {
                        self.collect_used_symbols(value);
                        true
                    } else {
                    }
                },
                Statement::Constant { name, type_annotation, value } => {
                    if self.is_symbol_used(name) {
                        self.collect_used_symbols(value);
                        true
                    } else {
                    }
                },
                Statement::Function { name, parameters, return_type, body, async_flag } => {
                    if self.is_symbol_used(name) {
                        for param in parameters {
                            self.symbol_stack.push(param.name.clone());
                        }
                        for stmt in body {
                            self.remove_unused_statements(&mut vec![stmt.clone()]);
                        }
                        for param in parameters {
                            self.symbol_stack.pop();
                        }
                        true
                    } else {
                    }
                },
                Statement::Class { name, base, fields, methods } => {
                    if self.is_symbol_used(name) {
                        for field in fields {
                            if !self.is_symbol_used(&field.name) {
                            }
                        }
                        for method in methods {
                            self.remove_unused_statements(&mut vec![method.clone()]);
                        }
                        true
                    } else {
                    }
                },
                Statement::If { condition, then_branch, elif_branches, else_branch } => {
                    then_branch.retain(|stmt| {
                        match stmt {
                            Statement::Expression(_) => true,
                        }
                    });

                    for (_, elif_body) in elif_branches {
                        elif_body.retain(|stmt| {
                            match stmt {
                                Statement::Expression(_) => true,
                                _ => false
                            }
                        });
                    }

                    if let Some(else_branch) = else_branch {
                        else_branch.retain(|stmt| {
                            match stmt {
                                Statement::Expression(_) => true,
                                _ => false
                            }
                        });
                    }

                    !then_branch.is_empty() || 
                    elif_branches.iter().any(|(_, body)| !body.is_empty()) || 
                    else_branch.as_ref().map_or(false, |body| !body.is_empty())
                },
                Statement::While { condition, body } => {
                    body.retain(|stmt| {
                        match stmt {
                            Statement::Expression(_) => true,
                            _ => false
                        }
                    });
                    !body.is_empty()
                },
                Statement::For { variable, iterable, body } => {
                    body.retain(|stmt| {
                        match stmt {
                            Statement::Expression(_) => true,
                            _ => false
                        }
                    });
                    !body.is_empty()
                },
                Statement::Block(statements) => {
                    self.remove_unused_statements(statements);
                    !statements.is_empty()
                },
            }
        });
    }
}

impl Optimizer for TreeShakingOptimizer {
    fn optimize_program(&mut self, program: &mut Program) -> CompilerResult<()> {
        self.collect_used_symbols_from_statement(&Statement::Block(program.statements.clone()));
        
        self.remove_unused_statements(&mut program.statements);
        
        Ok(())
    }

    fn optimize_statement(&mut self, stmt: &mut Statement) -> CompilerResult<()> {
        match stmt {
            Statement::Block(statements) => {
                self.remove_unused_statements(statements);
            },
            Statement::If { then_branch, elif_branches, else_branch, .. } => {
                self.remove_unused_statements(then_branch);
                
                for (_, elif_body) in elif_branches {
                    self.remove_unused_statements(elif_body);
                }
                
                if let Some(else_body) = else_branch {
                    self.remove_unused_statements(else_body);
                }
            },
            Statement::While { body, .. } => {
                self.remove_unused_statements(body);
            },
            Statement::For { body, .. } => {
                self.remove_unused_statements(body);
            },
            _ => {}
        }
        
        Ok(())
    }

    fn optimize_expression(&mut self, expr: &mut Expression) -> CompilerResult<()> {
        Ok(())
    }
}
