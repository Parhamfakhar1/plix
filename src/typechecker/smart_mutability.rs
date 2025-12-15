#![allow(dead_code)]

use std::collections::{HashMap, HashSet};
use crate::utils::position::Span;
use crate::parser::ast::{Expression, Statement, Parameter, Program};
use super::scope::Type;
use super::types::TypeEnvironment;
use super::use_def::UseDefAnalysis;

#[derive(Debug, Clone, PartialEq)]
pub enum Mutability {
    Immutable,
    Mutable,
    Unknown,
}

#[derive(Debug, Clone)]
pub struct VariableState {
    pub name: String,
    pub mutability: Mutability,
    pub span: Span,
    pub references: Vec<Span>,
    pub mutations: Vec<Span>,
    pub regions: HashSet<String>,
    pub inferred_type: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct Region {
    pub id: String,
    pub variables: HashSet<String>,
    pub parent: Option<String>,
    pub children: Vec<String>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct RegionGraph {
    pub regions: HashMap<String, Region>,
    pub current_region: String,
    pub region_counter: usize,
}

impl RegionGraph {
    pub fn new() -> Self {
        let root_region = Region {
            id: "root".to_string(),
            variables: HashSet::new(),
            parent: None,
            children: Vec::new(),
            span: Span::default(),
        };
        
        let mut regions = HashMap::new();
        regions.insert("root".to_string(), root_region);
        
        Self {
            regions,
            current_region: "root".to_string(),
            region_counter: 1,
        }
    }
    
    pub fn enter_region(&mut self, span: Span) -> String {
        let region_id = format!("region_{}", self.region_counter);
        self.region_counter += 1;
        
        let new_region = Region {
            id: region_id.clone(),
            variables: HashSet::new(),
            parent: Some(self.current_region.clone()),
            children: Vec::new(),
            span,
        };
        
        self.regions.insert(region_id.clone(), new_region);
        
        if let Some(parent) = self.regions.get_mut(&self.current_region) {
            parent.children.push(region_id.clone());
        }
        
        self.current_region = region_id.clone();
        region_id
    }
    
    pub fn exit_region(&mut self) {
        if let Some(region) = self.regions.get(&self.current_region) {
            if let Some(parent) = &region.parent {
                self.current_region = parent.clone();
            }
        }
    }
    
    pub fn add_variable_to_region(&mut self, region_id: &str, var_name: &str) {
        if let Some(region) = self.regions.get_mut(region_id) {
            region.variables.insert(var_name.to_string());
        }
    }
    
    pub fn get_variable_regions(&self, var_name: &str) -> Vec<String> {
        self.regions
            .iter()
            .filter(|(_, region)| region.variables.contains(var_name))
            .map(|(id, _)| id.clone())
            .collect()
    }
    
    pub fn find_common_ancestor(&self, region1: &str, region2: &str) -> Option<String> {
        let mut path1 = Vec::new();
        let mut current = region1;
        while let Some(region) = self.regions.get(current) {
            path1.push(current.to_string());
            if let Some(parent) = &region.parent {
                current = parent;
            } else {
                break;
            }
        }
        
        let mut current = region2;
        while let Some(region) = self.regions.get(current) {
            if path1.contains(&current.to_string()) {
                return Some(current.to_string());
            }
            if let Some(parent) = &region.parent {
                current = parent;
            } else {
                break;
            }
        }
        
        None
    }
}

#[derive(Debug, Clone)]
pub struct SmartMutabilityChecker {
    pub variable_states: HashMap<String, VariableState>,
    pub region_graph: RegionGraph,
    pub type_env: TypeEnvironment,
    pub use_def: UseDefAnalysis,
    pub errors: Vec<String>,
}

impl SmartMutabilityChecker {
    pub fn new() -> Self {
        Self {
            variable_states: HashMap::new(),
            region_graph: RegionGraph::new(),
            type_env: TypeEnvironment::new(),
            use_def: UseDefAnalysis::new(),
            errors: Vec::new(),
        }
    }
    
    pub fn check_program(&mut self, program: &Program) -> Result<(), Vec<String>> {
        // First pass: collect variable declarations and initial states
        self.collect_variable_declarations(program)?;
        
        // Second pass: analyze usage patterns
        self.analyze_usage_patterns(program)?;
        
        // Third pass: enforce mutability rules
        self.enforce_mutability_rules()?;
        
        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }
    
    fn collect_variable_declarations(&mut self, program: &Program) -> Result<(), Vec<String>> {
        for stmt in &program.statements {
            self.collect_declarations_in_statement(stmt)?;
        }
        Ok(())
    }
    
    fn collect_declarations_in_statement(&mut self, stmt: &Statement) -> Result<(), Vec<String>> {
        match stmt {
            Statement::Variable { mutable, name, type_annotation: _, value, span } => {
                let mutability = if *mutable {
                    Mutability::Mutable
                } else {
                    Mutability::Immutable
                };
                
                let current_region = self.region_graph.current_region.clone();
                let state = VariableState {
                    name: name.clone(),
                    mutability,
                    span: *span,
                    references: Vec::new(),
                    mutations: Vec::new(),
                    regions: std::iter::once(current_region.clone()).collect(),
                    inferred_type: None,
                };
                
                self.variable_states.insert(name.clone(), state);
                self.region_graph.add_variable_to_region(&current_region, name);
                
                self.collect_declarations_in_expression(value)?;
            },
            
            Statement::Constant { name, type_annotation: _, value, span } => {
                let current_region = self.region_graph.current_region.clone();
                let state = VariableState {
                    name: name.clone(),
                    mutability: Mutability::Immutable,
                    span: *span,
                    references: Vec::new(),
                    mutations: Vec::new(),
                    regions: std::iter::once(current_region).collect(),
                    inferred_type: None,
                };
                
                self.variable_states.insert(name.clone(), state);
                let current_region = self.region_graph.current_region.clone();
                self.region_graph.add_variable_to_region(&current_region, name);
                
                self.collect_declarations_in_expression(value)?;
            },
            
            Statement::Function { name: _, parameters, body, span, .. } => {
                let function_region = self.region_graph.enter_region(*span);
                
                for param in parameters {
                    let current_region = self.region_graph.current_region.clone();
                    let state = VariableState {
                        name: param.name.clone(),
                        mutability: Mutability::Unknown,
                        span: Span::default(),
                        references: Vec::new(),
                        mutations: Vec::new(),
                        regions: std::iter::once(current_region).collect(),
                        inferred_type: None,
                    };
                    self.variable_states.insert(param.name.clone(), state);
                    self.region_graph.add_variable_to_region(&function_region, &param.name);
                }
                
                for stmt in body {
                    self.collect_declarations_in_statement(stmt)?;
                }
                
                self.region_graph.exit_region();
            },
            
            Statement::Block(statements, span) => {
                let block_region = self.region_graph.enter_region(*span);
                
                for stmt in statements {
                    self.collect_declarations_in_statement(stmt)?;
                }
                
                self.region_graph.exit_region();
            },
            
            _ => {}
        }
        
        Ok(())
    }
    
    fn collect_declarations_in_expression(&mut self, expr: &Expression) -> Result<(), Vec<String>> {
        match expr {
            Expression::Identifier(name, span) => {
                if let Some(state) = self.variable_states.get_mut(name) {
                    state.references.push(*span);
                }
            },
            
            Expression::Binary { left, right, .. } => {
                self.collect_declarations_in_expression(left)?;
                self.collect_declarations_in_expression(right)?;
            },
            
            Expression::Unary { expr, .. } => {
                self.collect_declarations_in_expression(expr)?;
            },
            
            Expression::Call { function, arguments, .. } => {
                self.collect_declarations_in_expression(function)?;
                for arg in arguments {
                    self.collect_declarations_in_expression(arg)?;
                }
            },
            
            Expression::Assignment { target, value, .. } => {
                if let Expression::Identifier(name, span) = &**target {
                    if let Some(state) = self.variable_states.get_mut(name) {
                        state.mutations.push(*span);
                    }
                }
                
                self.collect_declarations_in_expression(value)?;
            },
            
            _ => {}
        }
        
        Ok(())
    }
    
    fn analyze_usage_patterns(&mut self, program: &Program) -> Result<(), Vec<String>> {
        for stmt in &program.statements {
            self.analyze_statement(stmt)?;
        }
        Ok(())
    }
    
    fn analyze_statement(&mut self, stmt: &Statement) -> Result<(), Vec<String>> {
        match stmt {
            Statement::Variable { name, value, .. } => {
                self.analyze_expression(value)?;
                
                if let Some(state) = self.variable_states.get_mut(name) {
                    if state.mutations.is_empty() {
                        state.mutability = Mutability::Immutable;
                    } else {
                        state.mutability = Mutability::Mutable;
                    }
                }
            },
            
            Statement::Constant { name, value, .. } => {
                self.analyze_expression(value)?;
                
                if let Some(state) = self.variable_states.get_mut(name) {
                    state.mutability = Mutability::Immutable;
                }
            },
            
            Statement::Function { body, span, .. } => {
                let function_region = self.region_graph.enter_region(*span);
                
                for stmt in body {
                    self.analyze_statement(stmt)?;
                }
                
                self.region_graph.exit_region();
            },
            
            Statement::Block(statements, span) => {
                let block_region = self.region_graph.enter_region(*span);
                
                for stmt in statements {
                    self.analyze_statement(stmt)?;
                }
                
                self.region_graph.exit_region();
            },
            
            _ => {}
        }
        
        Ok(())
    }
    
    fn analyze_expression(&mut self, expr: &Expression) -> Result<(), Vec<String>> {
        match expr {
            Expression::Identifier(name, span) => {
                if let Some(state) = self.variable_states.get_mut(name) {
                    state.references.push(*span);
                }
            },
            
            Expression::Binary { left, right, .. } => {
                self.analyze_expression(left)?;
                self.analyze_expression(right)?;
            },
            
            Expression::Unary { expr, .. } => {
                self.analyze_expression(expr)?;
            },
            
            Expression::Call { function, arguments, .. } => {
                self.analyze_expression(function)?;
                for arg in arguments {
                    self.analyze_expression(arg)?;
                }
            },
            
            Expression::Assignment { target, value, .. } => {
                if let Expression::Identifier(name, span) = &**target {
                    if let Some(state) = self.variable_states.get_mut(name) {
                        state.mutations.push(*span);
                    }
                }
                
                self.analyze_expression(value)?;
            },
            
            _ => {}
        }
        
        Ok(())
    }
    
    fn enforce_mutability_rules(&mut self) -> Result<(), Vec<String>> {
        let variable_names: Vec<String> = self.variable_states.keys().cloned().collect();
        for name in variable_names {
            if let Some(state) = self.variable_states.get(&name).cloned() {
                self.check_variable_mutability(&name, &state)?;
            }
        }
        Ok(())
    }
    
    fn check_variable_mutability(&mut self, name: &str, state: &VariableState) -> Result<(), Vec<String>> {
        match state.mutability {
            Mutability::Immutable => {
                if !state.mutations.is_empty() {
                    self.errors.push(format!(
                        "Cannot mutate immutable variable '{}' at line {}",
                        name, state.mutations[0].start.line
                    ));
                }
            },
            
            Mutability::Mutable => {
                self.check_mutable_references(name, state)?;
            },
            
            Mutability::Unknown => {
                // Variable was never used, which is fine
            }
        }
        
        Ok(())
    }
    
    fn check_mutable_references(&mut self, name: &str, state: &VariableState) -> Result<(), Vec<String>> {
        let regions: Vec<String> = state.regions.iter().cloned().collect();
        
        if regions.len() > 1 {
            self.errors.push(format!(
                "Variable '{}' has multiple references in different regions, cannot safely mutate",
                name
            ));
        }
        
        for region_id in regions {
            if let Some(region) = self.region_graph.regions.get(&region_id) {
                for other_var in &region.variables {
                    if other_var != name {
                        if let Some(other_state) = self.variable_states.get(other_var) {
                            if other_state.mutability == Mutability::Mutable {
                                self.errors.push(format!(
                                    "Cannot have multiple mutable references to '{}' and '{}' in the same region",
                                    name, other_var
                                ));
                            }
                        }
                    }
                }
            }
        }
        
        Ok(())
    }
    
    fn get_current_regions(&self) -> HashSet<String> {
        let mut regions = HashSet::new();
        regions.insert(self.region_graph.current_region.clone());
        regions
    }
    
    pub fn get_variable_state(&self, name: &str) -> Option<&VariableState> {
        self.variable_states.get(name)
    }
    
    pub fn get_region_graph(&self) -> &RegionGraph {
        &self.region_graph
    }
    
    pub fn get_errors(&self) -> &[String] {
        &self.errors
    }
}

impl Default for SmartMutabilityChecker {
    fn default() -> Self {
        Self::new()
    }
}
