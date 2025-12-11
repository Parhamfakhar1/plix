use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use crate::utils::position::Span;
use crate::parser::ast::{Statement, Expression};

#[derive(Debug, Clone, PartialEq)]
pub enum DefinitionKind {
    Variable,
    Function,
    Parameter,
    Constant,
    Class,
    Module,
}

#[derive(Debug, Clone)]
pub struct Definition {
    pub name: String,
    pub kind: DefinitionKind,
    pub span: Span,
}

impl Definition {
    pub fn new(name: String, kind: DefinitionKind, span: Span, scope_id: usize) -> Self {
        Self {
            name,
            kind,
            span,
            defined_in_scope: scope_id,
            used_in_scopes: HashSet::new(),
            dependencies: HashSet::new(),
        }
    }

    pub fn mark_used(&mut self, scope_id: usize) {
        self.used_in_scopes.insert(scope_id);
    }

    pub fn add_dependency(&mut self, name: &str) {
        self.dependencies.insert(name.to_string());
    }

    pub fn is_used(&self) -> bool {
        !self.used_in_scopes.is_empty()
    }

    pub fn is_unused(&self) -> bool {
        self.used_in_scopes.is_empty()
    }
}

#[derive(Debug, Clone)]
pub struct UseDefAnalysis {
    pub definitions: HashMap<String, Definition>,
    pub scope_stack: Vec<usize>,
    pub current_scope_id: usize,
    pub scope_counter: usize,
}

impl UseDefAnalysis {
    pub fn new() -> Self {
        Self {
            definitions: HashMap::new(),
            current_scope_id: 0,
            scope_counter: 1,
            call_graph: HashMap::new(),
            ast_dependents: HashMap::new(),
        }
    }

    pub fn enter_scope(&mut self) -> usize {
        let new_scope_id = self.scope_counter;
        self.scope_counter += 1;
        self.current_scope_id = new_scope_id;
        self.scope_stack.push(new_scope_id);
        new_scope_id
    }

    pub fn exit_scope(&mut self) {
        if !self.scope_stack.is_empty() {
            self.scope_stack.pop();
            self.current_scope_id = *self.scope_stack.last().unwrap_or(&0);
        }
    }

    pub fn define_variable(&mut self, name: String, span: Span) -> Definition {
        let def = Definition::new(
            name.clone(),
            DefinitionKind::Variable,
            span,
            self.current_scope_id,
        );
        
        self.definitions.insert(name.clone(), def.clone());
        self.ast_dependents.entry(name).or_insert_with(Vec::new);
        
        def
    }

    pub fn define_function(&mut self, name: String, span: Span) -> Definition {
        let def = Definition::new(
            name.clone(),
            DefinitionKind::Function,
            span,
            self.current_scope_id,
        );
        
        self.definitions.insert(name.clone(), def.clone());
        self.call_graph.insert(name.clone(), HashSet::new());
        self.ast_dependents.entry(name).or_insert_with(Vec::new);
        
        def
    }

    pub fn define_parameter(&mut self, name: String, span: Span) -> Definition {
        let def = Definition::new(
            name.clone(),
            DefinitionKind::Parameter,
            span,
            self.current_scope_id,
        );
        
        self.definitions.insert(name.clone(), def.clone());
        self.ast_dependents.entry(name).or_insert_with(Vec::new);
        
        def
    }

    pub fn define_constant(&mut self, name: String, span: Span) -> Definition {
        let def = Definition::new(
            name.clone(),
            DefinitionKind::Constant,
            span,
            self.current_scope_id,
        );
        
        self.definitions.insert(name.clone(), def.clone());
        self.ast_dependents.entry(name).or_insert_with(Vec::new);
        
        def
    }

    pub fn lookup_definition(&self, name: &str) -> Option<&Definition> {
        self.definitions.get(name)
    }

    pub fn mark_usage(&mut self, name: &str, span: Span) {
        if let Some(mut def) = self.definitions.get_mut(name) {
            def.mark_used(self.current_scope_id);
            
            if let Some(dependents) = self.ast_dependents.get_mut(name) {
                let dependent_key = format!("usage_{}_{}", span.start.line, span.start.column);
                if !dependents.contains(&dependent_key) {
                    dependents.push(dependent_key);
                }
            }
        }
    }

    pub fn add_dependency(&mut self, from: &str, to: &str) {
        if let Some(from_def) = self.definitions.get_mut(from) {
            from_def.add_dependency(to);
        }
        
        if let Some(from_def) = self.definitions.get(from) {
            if from_def.kind == DefinitionKind::Function {
                self.call_graph.entry(from.to_string()).or_insert_with(HashSet::new).insert(to.to_string());
            }
        }
    }

    pub fn analyze_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Variable { mutable: _, name, value, .. } => {
                let def = self.define_variable(name.clone(), stmt.span());
                
                self.analyze_expression(value);
                
                self.collect_expression_dependencies(value, &def.name);
            },
            
            Statement::Constant { name, value, .. } => {
                let def = self.define_constant(name.clone(), stmt.span());
                
                self.analyze_expression(value);
                
                self.collect_expression_dependencies(value, &def.name);
            },
            
            Statement::Function { name, parameters, body, .. } => {
                let def = self.define_function(name.clone(), stmt.span());
                
                let function_scope = self.enter_scope();
                
                for param in parameters {
                    let param_def = self.define_parameter(param.name.clone(), param.span.unwrap_or_else(|| stmt.span()));
                    
                    if let Some(default_value) = &param.default_value {
                        self.analyze_expression(default_value);
                        self.collect_expression_dependencies(default_value, &param.name);
                    }
                }
                
                for stmt in body {
                    self.analyze_statement(stmt);
                }
                
                self.exit_scope();
                
                self.collect_statement_dependencies(body, &def.name);
            },
            
            Statement::If { condition, then_branch, elif_branches, else_branch } => {
                self.analyze_expression(condition);
                
                let if_scope = self.enter_scope();
                for stmt in then_branch {
                    self.analyze_statement(stmt);
                }
                self.exit_scope();
                
                for (elif_cond, elif_body) in elif_branches {
                    self.analyze_expression(elif_cond);
                    
                    let elif_scope = self.enter_scope();
                    for stmt in elif_body {
                        self.analyze_statement(stmt);
                    }
                    self.exit_scope();
                }
                
                if let Some(else_body) = else_branch {
                    let else_scope = self.enter_scope();
                    for stmt in else_body {
                        self.analyze_statement(stmt);
                    }
                    self.exit_scope();
                }
            },
            
            Statement::While { condition, body } => {
                self.analyze_expression(condition);
                
                let loop_scope = self.enter_scope();
                for stmt in body {
                    self.analyze_statement(stmt);
                }
                self.exit_scope();
            },
            
            Statement::For { variable, iterable, body } => {
                self.analyze_expression(iterable);
                
                let def = self.define_variable(variable.clone(), stmt.span());
                
                let loop_scope = self.enter_scope();
                
                self.mark_usage(variable, stmt.span());
                
                for stmt in body {
                    self.analyze_statement(stmt);
                }
                
                self.exit_scope();
                
                self.add_dependency(variable, &def.name);
            },
            
            Statement::Match { expr, arms } => {
                self.analyze_expression(expr);
                
                let match_scope = self.enter_scope();
                
                for arm in arms {
                    self.analyze_expression(&arm.pattern);
                    if let Some(guard) = &arm.guard {
                        self.analyze_expression(guard);
                    }
                    self.analyze_expression(&arm.body);
                }
                
                self.exit_scope();
            },
            
            Statement::Block(statements) => {
                let block_scope = self.enter_scope();
                for stmt in statements {
                    self.analyze_statement(stmt);
                }
                self.exit_scope();
            },
            
            Statement::Expression(expr) => {
                self.analyze_expression(expr);
            },
            
            Statement::Return(value) => {
                if let Some(expr) = value {
                    self.analyze_expression(expr);
                }
            },
            
            Statement::Class { name, base, fields, methods } => {
                let def = self.define_variable(name.clone(), stmt.span());
                
                if let Some(base_name) = base {
                    self.mark_usage(base_name, stmt.span());
                    self.add_dependency(name, base_name);
                }
                
                for field in fields {
                    let field_def = self.define_variable(field.name.clone(), stmt.span());
                    
                    self.add_dependency(name, &field.name);
                }
                
                for method in methods {
                    self.analyze_statement(method);
                    
                    if let Statement::Function { name: method_name, .. } = method {
                        self.add_dependency(name, method_name);
                    }
                }
            },
            
            Statement::Import { module, alias, items } => {
                self.mark_usage(module, stmt.span());
                
                if let Some(items) = items {
                    for item in items {
                        let imported_name = item.name.clone();
                        
                        let tracking_name = item.as_ref().map_or_else(
                            || imported_name.clone(),
                            |(_, alias)| alias.clone().unwrap_or_else(|| imported_name.clone())
                        );
                        
                        let _ = self.define_variable(tracking_name.clone(), stmt.span());
                        
                        self.add_dependency(&tracking_name, module);
                        
                        if tracking_name != imported_name {
                            self.add_dependency(&tracking_name, &imported_name);
                        }
                    }
                } else if let Some(alias) = alias {
                    let _ = self.define_variable(alias.clone(), stmt.span());
                    self.add_dependency(alias, module);
                } else {
                    let _ = self.define_variable(module.clone(), stmt.span());
                }
            },
        }
    }

    pub fn analyze_expression(&mut self, expr: &Expression) {
        match expr {
            Expression::Identifier(name) => {
                self.mark_usage(name, expr.span());
            },
            
            Expression::Literal(_) => {
            },
            
            Expression::Binary { left, op: _, right } => {
                self.analyze_expression(left);
                self.analyze_expression(right);
            },
            
            Expression::Unary { op: _, expr } => {
                self.analyze_expression(expr);
            },
            
            Expression::Call { function, arguments } => {
                self.analyze_expression(function);
                
                for arg in arguments {
                    self.analyze_expression(arg);
                }
                
                if let Expression::Identifier(func_name) = &**function {
                    for arg in arguments {
                        self.collect_expression_dependencies(arg, func_name);
                    }
                }
            },
            
            Expression::Index { expr, index } => {
                self.analyze_expression(expr);
                self.analyze_expression(index);
            },
            
            Expression::Member { expr, member } => {
                self.analyze_expression(expr);
                
                if let Expression::Identifier(obj_name) = &**expr {
                    self.add_dependency(obj_name, member);
                }
            },
            
            Expression::Assignment { target, value, op: _ } => {
                self.analyze_expression(target);
                self.analyze_expression(value);
                
                if let Expression::Identifier(target_name) = &**target {
                    self.collect_expression_dependencies(value, target_name);
                }
            },
            
            Expression::Lambda { parameters, return_type: _, body } => {
                let lambda_scope = self.enter_scope();
                
                for param in parameters {
                    let _ = self.define_parameter(param.name.clone(), expr.span());
                }
                
                self.analyze_statement(body);
                
                self.exit_scope();
            },
            
            Expression::If { condition, then_branch, else_branch } => {
                self.analyze_expression(condition);
                self.analyze_expression(then_branch);
                
                if let Some(else_branch) = else_branch {
                    self.analyze_expression(else_branch);
                }
            },
            
            Expression::Match { expr, arms } => {
                self.analyze_expression(expr);
                
                for arm in arms {
                    self.analyze_expression(&arm.pattern);
                    if let Some(guard) = &arm.guard {
                        self.analyze_expression(guard);
                    }
                    self.analyze_expression(&arm.body);
                }
            },
        }
    }

    fn collect_expression_dependencies(&mut self, expr: &Expression, dependent_name: &str) {
        match expr {
            Expression::Identifier(name) => {
                self.add_dependency(dependent_name, name);
            },
            
            Expression::Binary { left, op: _, right } => {
                self.collect_expression_dependencies(left, dependent_name);
                self.collect_expression_dependencies(right, dependent_name);
            },
            
            Expression::Unary { op: _, expr } => {
                self.collect_expression_dependencies(expr, dependent_name);
            },
            
            Expression::Call { function, arguments } => {
                self.collect_expression_dependencies(function, dependent_name);
                
                for arg in arguments {
                    self.collect_expression_dependencies(arg, dependent_name);
                }
            },
            
            Expression::Index { expr, index } => {
                self.collect_expression_dependencies(expr, dependent_name);
                self.collect_expression_dependencies(index, dependent_name);
            },
            
            Expression::Member { expr, member } => {
                self.collect_expression_dependencies(expr, dependent_name);
                self.add_dependency(dependent_name, member);
            },
            
            Expression::Assignment { target, value, op: _ } => {
                self.collect_expression_dependencies(target, dependent_name);
                self.collect_expression_dependencies(value, dependent_name);
            },
            
            Expression::Lambda { parameters, return_type: _, body } => {
                for param in parameters {
                    self.add_dependency(dependent_name, &param.name);
                }
                self.collect_statement_dependencies(&vec![body.clone()], dependent_name);
            },
            
            Expression::If { condition, then_branch, else_branch } => {
                self.collect_expression_dependencies(condition, dependent_name);
                self.collect_expression_dependencies(then_branch, dependent_name);
                
                if let Some(else_branch) = else_branch {
                    self.collect_expression_dependencies(else_branch, dependent_name);
                }
            },
            
            Expression::Match { expr, arms } => {
                self.collect_expression_dependencies(expr, dependent_name);
                
                for arm in arms {
                    self.collect_expression_dependencies(&arm.pattern, dependent_name);
                    if let Some(guard) = &arm.guard {
                        self.collect_expression_dependencies(guard, dependent_name);
                    }
                    self.collect_expression_dependencies(&arm.body, dependent_name);
                }
            },
            
            Expression::Literal(_) => {
            },
        }
    }

    fn collect_statement_dependencies(&mut self, statements: &[Statement], dependent_name: &str) {
        for stmt in statements {
            match stmt {
                Statement::Variable { name, value, .. } => {
                    self.add_dependency(dependent_name, name);
                    self.collect_expression_dependencies(value, dependent_name);
                },
                
                Statement::Constant { name, value, .. } => {
                    self.add_dependency(dependent_name, name);
                    self.collect_expression_dependencies(value, dependent_name);
                },
                
                Statement::Function { name, parameters, body, .. } => {
                    self.add_dependency(dependent_name, name);
                    
                    for param in parameters {
                        self.add_dependency(dependent_name, &param.name);
                    }
                    
                    self.collect_statement_dependencies(body, dependent_name);
                },
                
                Statement::If { condition, then_branch, elif_branches, else_branch } => {
                    self.collect_expression_dependencies(condition, dependent_name);
                    
                    for stmt in then_branch {
                        self.collect_statement_dependencies(&vec![stmt.clone()], dependent_name);
                    }
                    
                    for (elif_cond, elif_body) in elif_branches {
                        self.collect_expression_dependencies(elif_cond, dependent_name);
                        for stmt in elif_body {
                            self.collect_statement_dependencies(&vec![stmt.clone()], dependent_name);
                        }
                    }
                    
                    if let Some(else_body) = else_branch {
                        for stmt in else_body {
                            self.collect_statement_dependencies(&vec![stmt.clone()], dependent_name);
                        }
                    }
                },
                
                Statement::While { condition, body } => {
                    self.collect_expression_dependencies(condition, dependent_name);
                    
                    for stmt in body {
                        self.collect_statement_dependencies(&vec![stmt.clone()], dependent_name);
                    }
                },
                
                Statement::For { variable, iterable, .. } => {
                    self.add_dependency(dependent_name, variable);
                    self.collect_expression_dependencies(iterable, dependent_name);
                },
                
                Statement::Match { expr, arms } => {
                    self.collect_expression_dependencies(expr, dependent_name);
                    
                    for arm in arms {
                        self.collect_expression_dependencies(&arm.pattern, dependent_name);
                        if let Some(guard) = &arm.guard {
                            self.collect_expression_dependencies(guard, dependent_name);
                        }
                        self.collect_expression_dependencies(&arm.body, dependent_name);
                    }
                },
                
                Statement::Block(statements) => {
                    for stmt in statements {
                        self.collect_statement(&vec![stmt.clone()], dependent_name);
                    }
                },
                
                Statement::Expression(expr) => {
                    self.collect_expression_dependencies(expr, dependent_name);
                },
                
                Statement::Return(value) => {
                    if let Some(expr) = value {
                        self.collect_expression_dependencies(expr, dependent_name);
                    }
                },
                
                Statement::Class { name, base, fields, methods } => {
                    self.add_dependency(dependent_name, name);
                    
                    if let Some(base_name) = base {
                        self.add_dependency(dependent_name, base_name);
                    }
                    
                    for field in fields {
                        self.add_dependency(dependent_name, &field.name);
                    }
                    
                    for method in methods {
                        self.collect_statement(&vec![method.clone()], dependent_name);
                    }
                },
                
                Statement::Import { module, alias, items } => {
                    self.add_dependency(dependent_name, module);
                    
                    if let Some(items) = items {
                        for item in items {
                            let tracking_name = item.as_ref().map_or_else(
                                || item.name.clone(),
                                |(_, alias)| alias.clone().unwrap_or_else(|| item.name.clone())
                            );
                            self.add_dependency(dependent_name, &tracking_name);
                        }
                    } else if let Some(alias) = alias {
                        self.add_dependency(dependent_name, alias);
                    }
                },
            }
        }
    }

    pub fn get_unused_definitions(&self) -> Vec<&Definition> {
        self.definitions
            .values()
            .filter(|def| def.is_unused() && !matches!(def.kind, DefinitionKind::Parameter))
            .collect()
    }

    pub fn get_circular_dependencies(&self) -> Vec<Vec<String>> {
        let mut circular_deps = Vec::new();
        
        for (name, callers) in &self.call_graph {
            for caller in callers {
                if callers.contains(name) {
                    circular_deps.push(vec![name.clone(), caller.clone()]);
                }
            }
        }
        
        circular_deps
    }

    pub fn get_dependency_chain(&self, name: &str) -> Option<Vec<String>> {
        let mut chain = Vec::new();
        let mut current = name;
        
        let mut visited = HashSet::new();
        
        while let Some(def) = self.definitions.get(current) {
            if visited.contains(current) {
            }
            
            visited.insert(current.to_string());
            chain.push(current.to_string());
            
            if let Some(first_dep) = def.dependencies.iter().next() {
                current = first_dep;
            } else {
                break;
            }
        }
        
        if chain.len() > 1 {
            Some(chain)
        } else {
            None
        }
    }
}
