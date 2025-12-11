use crate::parser::ast::{Program, Statement, Expression, BinaryOp, UnaryOp, Literal};
use crate::utils::error::{CompilerResult, CompilerError};
use crate::utils::position::Span;
use super::CodeGenerator;

#[derive(Debug, Clone, PartialEq)]
pub enum Operand {
    Register(String),
    Immediate(i64),
    ImmediateF64(f64),
    Label(String),
    Memory { base: Option<String>, offset: i64, scale: i64 },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    // Arithmetic operations
    Add { destination: Operand, source1: Operand, source2: Operand },
    Sub { destination: Operand, source1: Operand, source2: Operand },
    Mul { destination: Operand, source1: Operand, source2: Operand },
    Div { destination: Operand, source1: Operand, source2: Operand },
    Mod { destination: Operand, source1: Operand, source2: Operand },
    
    // Bitwise operations
    And { destination: Operand, source1: Operand, source2: Operand },
    Or { destination: Operand, source1: Operand, source2: Operand },
    Xor { destination: Operand, source1: Operand, source2: Operand },
    Not { destination: Operand, source: Operand },
    Shl { destination: Operand, source1: Operand, source2: Operand },
    Shr { destination: Operand, source1: Operand, source2: Operand },
    
    // Memory operations
    Load { destination: Operand, address: Operand },
    Store { address: Operand, source: Operand },
    Alloc { destination: Operand, size: Operand },
    Free { address: Operand },
    
    // Control flow
    Jump { target: Operand },
    JumpIf { condition: Operand, target: Operand },
    JumpIfNot { condition: Operand, target: Operand },
    Call { function: Operand, arguments: Vec<Operand> },
    Return { value: Option<Operand> },
    
    // Function operations
    Function { name: String, parameters: Vec<String>, body: Vec<Instruction> },
    Prologue,
    Epilogue,
    
    // Stack operations
    Push { source: Operand },
    Pop { destination: Operand },
    
    // Comparison
    Compare { source1: Operand, source2: Operand },
    SetEqual { destination: Operand },
    SetNotEqual { destination: Operand },
    SetLess { destination: Operand },
    SetLessEqual { destination: Operand },
    SetGreater { destination: Operand },
    SetGreaterEqual { destination: Operand },
    
    // Type conversions
    ConvertToInt { destination: Operand, source: Operand },
    ConvertToFloat { destination: Operand, source: Operand },
    ConvertToString { destination: Operand, source: Operand },
    ConvertToBool { destination: Operand, source: Operand },
    
    // Special
    Nop,
    Comment(String),
}

pub struct IRGenerator {
    instructions: Vec<Instruction>,
    label_counter: usize,
    current_function: Option<String>,
    symbol_table: std::collections::HashMap<String, Operand>,
}

impl IRGenerator {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            label_counter: 0,
            current_function: None,
            symbol_table: std::collections::HashMap::new(),
        }
    }

    pub fn get_instructions(&self) -> &[Instruction] {
        &self.instructions
    }

    pub fn get_mut_instructions(&mut self) -> &mut Vec<Instruction> {
        &mut self.instructions
    }

    fn generate_label(&mut self, prefix: &str) -> Operand {
        let label = format!("{}_{}", prefix, self.label_counter);
        self.label_counter += 1;
        Operand::Label(label)
    }

    fn emit(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }

    fn generate_arithmetic_op(&mut self, op: BinaryOp, destination: &Operand, left: &Operand, right: &Operand) {
        match op {
            BinaryOp::Add => self.emit(Instruction::Add {
                destination: destination.clone(),
                source1: left.clone(),
                source2: right.clone(),
            }),
            BinaryOp::Subtract => self.emit(Instruction::Sub {
                destination: destination.clone(),
                source1: left.clone(),
                source2: right.clone(),
            }),
            BinaryOp::Multiply => self.emit(Instruction::Mul {
                destination: destination.clone(),
                source1: left.clone(),
                source2: right.clone(),
            }),
            BinaryOp::Divide => self.emit(Instruction::Div {
                destination: destination.clone(),
                source1: left.clone(),
                source2: right.clone(),
            }),
            BinaryOp::Modulo => self.emit(Instruction::Mod {
                destination: destination.clone(),
                source1: left.clone(),
                source2: right.clone(),
            }),
            _ => {
                // For other operations, we'll handle them in the expression generation
                self.emit(Instruction::Nop);
            }
        }
    }

    fn generate_comparison_op(&mut self, op: BinaryOp, destination: &Operand, left: &Operand, right: &Operand) {
        // First generate the comparison
        self.emit(Instruction::Compare {
            source1: left.clone(),
            source2: right.clone(),
        });
        
        // Then set the appropriate flags
        match op {
            BinaryOp::Equal => self.emit(Instruction::SetEqual { destination: destination.clone() }),
            BinaryOp::NotEqual => self.emit(Instruction::SetNotEqual { destination: destination.clone() }),
            BinaryOp::Less => self.emit(Instruction::SetLess { destination: destination.clone() }),
            BinaryOp::LessEqual => self.emit(Instruction::SetLessEqual { destination: destination.clone() }),
            BinaryOp::Greater => self.emit(Instruction::SetGreater { destination: destination.clone() }),
            BinaryOp::GreaterEqual => self.emit(Instruction::SetGreaterEqual { destination: destination.clone() }),
            _ => {}
        }
    }

    fn generate_unary_op(&mut self, op: UnaryOp, destination: &Operand, source: &Operand) {
        match op {
            UnaryOp::Plus => {
                // Unary plus is a no-op, just copy the value
                self.emit(Instruction::Add {
                    destination: destination.clone(),
                    source1: source.clone(),
                    source2: Operand::Immediate(0),
                });
            },
            UnaryOp::Minus => {
                // Negate the value
                self.emit(Instruction::Sub {
                    destination: destination.clone(),
                    source1: Operand::Immediate(0),
                    source2: source.clone(),
                });
            },
            UnaryOp::Not => {
                // Logical not
                self.emit(Instruction::ConvertToBool {
                    destination: destination.clone(),
                    source: source.clone(),
                });
                self.emit(Instruction::Xor {
                    destination: destination.clone(),
                    source1: destination.clone(),
                    source2: Operand::Immediate(1),
                });
            },
            UnaryOp::BitNot => {
                // Bitwise not
                self.emit(Instruction::Not {
                    destination: destination.clone(),
                    source: source.clone(),
                });
            },
        }
    }

    fn generate_literal(&mut self, literal: &Literal) -> Operand {
        match literal {
            Literal::Integer(value) => Operand::Immediate(*value),
            Literal::Float(value) => Operand::ImmediateF64(*value),
            Literal::Boolean(value) => Operand::Immediate(if *value { 1 } else { 0 }),
            Literal::String(value) => {
                // In a real implementation, we'd store this in a string table
                Operand::Immediate(0) // Placeholder
            },
            Literal::Null => Operand::Immediate(0),
            Literal::Undefined => Operand::Immediate(0),
            _ => Operand::Immediate(0),
        }
    }

    fn allocate_register(&mut self, name: &str) -> Operand {
        Operand::Register(name.to_string())
    }

    fn store_symbol(&mut self, name: &str, operand: Operand) {
        self.symbol_table.insert(name.to_string(), operand);
    }

    fn load_symbol(&mut self, name: &str) -> Option<Operand> {
        self.symbol_table.get(name).cloned()
    }
}

impl CodeGenerator for IRGenerator {
    fn generate_program(&mut self, program: &Program) -> CompilerResult<()> {
        // Start with program prologue
        self.emit(Instruction::Comment("Program prologue".to_string()));
        
        for stmt in &program.statements {
            self.generate_statement(stmt)?;
        }
        
        // End with program epilogue
        self.emit(Instruction::Comment("Program epilogue".to_string()));
        self.emit(Instruction::Return { value: None });
        
        Ok(())
    }

    fn generate_statement(&mut self, stmt: &Statement) -> CompilerResult<()> {
        match stmt {
            Statement::Expression(expr) => {
                self.generate_expression(expr)?;
            },
            
            Statement::Variable { mutable: _, name, type_annotation: _, value } => {
                let temp_reg = self.allocate_register("temp");
                self.generate_expression(value)?;
                
                // Store the value in a variable (in a real implementation, this would be in memory)
                self.store_symbol(name, temp_reg);
            },
            
            Statement::Constant { name, type_annotation: _, value } => {
                let temp_reg = self.allocate_register("temp");
                self.generate_expression(value)?;
                
                // Store the constant value
                self.store_symbol(name, temp_reg);
            },
            
            Statement::Return(value) => {
                if let Some(expr) = value {
                    self.generate_expression(expr)?;
                }
                self.emit(Instruction::Return { value: None });
            },
            
            Statement::If { condition, then_branch, elif_branches, else_branch } => {
                self.generate_expression(condition)?;
                
                let temp_reg = self.allocate_register("temp");
                let else_label = self.generate_label("else");
                let end_label = self.generate_label("endif");
                
                self.emit(Instruction::JumpIfNot {
                    condition: temp_reg,
                    target: else_label.clone(),
                });
                
                // Generate then branch
                for stmt in then_branch {
                    self.generate_statement(stmt)?;
                }
                
                self.emit(Instruction::Jump { target: end_label.clone() });
                
                // Generate else branch if exists
                self.emit(Instruction::Jump { target: else_label.clone() });
                
                if let Some(else_body) = else_branch {
                    for stmt in else_body {
                        self.generate_statement(stmt)?;
                    }
                }
                
                self.emit(Instruction::Jump { target: end_label });
            },
            
            Statement::While { condition, body } => {
                let loop_start = self.generate_label("while_start");
                let loop_end = self.generate_label("while_end");
                
                self.emit(Instruction::Comment("While loop start".to_string()));
                
                // Generate condition check
                self.emit(Instruction::Jump { target: loop_start.clone() });
                
                // Generate loop body
                self.emit(Instruction::Label(loop_start.clone()));
                
                for stmt in body {
                    self.generate_statement(stmt)?;
                }
                
                // Check condition again
                self.generate_expression(condition)?;
                let temp_reg = self.allocate_register("temp");
                
                self.emit(Instruction::JumpIf {
                    condition: temp_reg,
                    target: loop_start.clone(),
                });
                
                self.emit(Instruction::Jump { target: loop_end.clone() });
                self.emit(Instruction::Label(loop_end));
                self.emit(Instruction::Comment("While loop end".to_string()));
            },
            
            Statement::For { variable, iterable, body } => {
                // This is a simplified version - in a real implementation,
                // we'd generate proper iterator code
                
                // Store the variable
                let temp_reg = self.allocate_register("temp");
                self.generate_expression(iterable)?;
                self.store_symbol(variable, temp_reg);
                
                // Generate loop body
                for stmt in body {
                    self.generate_statement(stmt)?;
                }
            },
            
            Statement::Match { expr, arms } => {
                self.generate_expression(expr)?;
                
                let temp_reg = self.allocate_register("temp");
                let end_label = self.generate_label("endmatch");
                
                for arm in arms {
                    // Generate pattern matching code (simplified)
                    self.generate_expression(&arm.pattern)?;
                    let temp_reg2 = self.allocate_register("temp");
                    
                    self.emit(Instruction::JumpIf {
                        condition: temp_reg2,
                        target: self.generate_label("match_arm"),
                    });
                }
                
                // Default case (if no patterns match)
                self.emit(Instruction::Jump { target: end_label.clone() });
                
                // Generate arm bodies
                for arm in arms {
                    self.emit(Instruction::Label(self.generate_label("match_arm")));
                    
                    if let Some(guard) = &arm.guard {
                        self.generate_expression(guard)?;
                        let guard_reg = self.allocate_register("temp");
                        
                        self.emit(Instruction::JumpIfNot {
                            condition: guard_reg,
                            target: end_label.clone(),
                        });
                    }
                    
                    self.generate_expression(&arm.body)?;
                    self.emit(Instruction::Jump { target: end_label.clone() });
                }
                
                self.emit(Instruction::Label(end_label));
            },
            
            Statement::Block(statements) => {
                for stmt in statements {
                    self.generate_statement(stmt)?;
                }
            },
            
            Statement::Function { name, parameters, return_type: _, body, async_flag: _ } => {
                let func_label = self.generate_label("function");
                let prologue_label = self.generate_label("prologue");
                let epilogue_label = self.generate_label("epilogue");
                
                // Function definition
                self.emit(Instruction::Function {
                    name: name.clone(),
                    parameters: parameters.iter().map(|p| p.name.clone()).collect(),
                    body: Vec::new(), // Will be filled later
                });
                
                // Store current function context
                self.current_function = Some(name.clone());
                
                // Generate function prologue
                self.emit(Instruction::Prologue);
                
                // Generate function body
                for stmt in body {
                    self.generate_statement(stmt)?;
                }
                
                // Generate function epilogue
                self.emit(Instruction::Epilogue);
                self.emit(Instruction::Return { value: None });
                
                // Restore current function context
                self.current_function = None;
            },
            
            Statement::Class { name, base, fields, methods } => {
                // Class definition is handled as a special function in IR
                let temp_reg = self.allocate_register("temp");
                
                // Create class object
                self.emit(Instruction::Alloc {
                    destination: temp_reg.clone(),
                    size: Operand::Immediate(8 * fields.len() as i64),
                });
                
                // Store class symbol
                self.store_symbol(name, temp_reg);
                
                // Generate methods
                for method in methods {
                    self.generate_statement(method)?;
                }
            },
            
            Statement::Import { module, alias, items } => {
                // Import statements are handled at a higher level
                // In IR, we might generate calls to import functions
                self.emit(Instruction::Comment(format!("Import: {}", module)));
            },
        }
        
        Ok(())
    }

    fn generate_expression(&mut self, expr: &Expression) -> CompilerResult<()> {
        match expr {
            Expression::Identifier(name) => {
                if let Some(operand) = self.load_symbol(name) {
                    // Load the value into a temporary register
                    let temp_reg = self.allocate_register("temp");
                    self.emit(Instruction::Load {
                        destination: temp_reg,
                        address: operand,
                    });
                }
            },
            
            Expression::Literal(literal) => {
                let temp_reg = self.allocate_register("temp");
                let operand = self.generate_literal(literal);
                self.emit(Instruction::Add {
                    destination: temp_reg,
                    source1: operand,
                    source2: Operand::Immediate(0),
                });
            },
            
            Expression::Binary { left, op, right } => {
                let temp_reg = self.allocate_register("temp");
                
                // Generate left operand
                self.generate_expression(left)?;
                let left_reg = self.allocate_register("left");
                
                // Generate right operand
                self.generate_expression(right)?;
                let right_reg = self.allocate_register("right");
                
                match op {
                    BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide | BinaryOp::Modulo => {
                        self.generate_arithmetic_op(*op, &temp_reg, &left_reg, &right_reg);
                    },
                    
                    BinaryOp::Equal | BinaryOp::NotEqual | BinaryOp::Less | BinaryOp::LessEqual | 
                    BinaryOp::Greater | BinaryOp::GreaterEqual => {
                        self.generate_comparison_op(*op, &temp_reg, &left_reg, &right_reg);
                    },
                    
                    _ => {
                        // For other operations, we'll emit a nop for now
                        self.emit(Instruction::Nop);
                    }
                }
            },
            
            Expression::Unary { op, expr } => {
                let temp_reg = self.allocate_register("temp");
                
                // Generate operand
                self.generate_expression(expr)?;
                let operand_reg = self.allocate_register("operand");
                
                self.generate_unary_op(*op, &temp_reg, &operand_reg);
            },
            
            Expression::Call { function, arguments } => {
                // Generate function arguments
                let mut arg_operands = Vec::new();
                for arg in arguments {
                    self.generate_expression(arg)?;
                    let temp_reg = self.allocate_register("arg");
                    arg_operands.push(temp_reg);
                }
                
                // Generate function call
                self.generate_expression(function)?;
                let func_reg = self.allocate_register("func");
                
                self.emit(Instruction::Call {
                    function: func_reg,
                    arguments: arg_operands,
                });
            },
            
            Expression::Index { expr, index } => {
                // Generate array index operation
                self.generate_expression(expr)?;
                let array_reg = self.allocate_register("array");
                
                self.generate_expression(index)?;
                let index_reg = self.allocate_register("index");
                
                let temp_reg = self.allocate_register("temp");
                self.emit(Instruction::Load {
                    destination: temp_reg,
                    address: Operand::Memory {
                        base: Some(array_reg.to_string()),
                        offset: 0,
                        scale: 8,
                    },
                });
            },
            
            Expression::Member { expr, member } => {
                // Generate member access
                self.generate_expression(expr)?;
                let object_reg = self.allocate_register("object");
                
                let temp_reg = self.allocate_register("temp");
                self.emit(Instruction::Load {
                    destination: temp_reg,
                    address: Operand::Memory {
                        base: Some(object_reg.to_string()),
                        offset: 0, // Would be calculated based on member offset
                        scale: 8,
                    },
                });
            },
            
            Expression::Assignment { target, value, op } => {
                // Generate value
                self.generate_expression(value)?;
                let value_reg = self.allocate_register("value");
                
                // Generate target
                self.generate_expression(target)?;
                let target_reg = self.allocate_register("target");
                
                if let Some(assignment_op) = op {
                    // For compound assignments, generate the operation
                    self.generate_arithmetic_op(*assignment_op, &target_reg, &target_reg, &value_reg);
                } else {
                    // Simple assignment
                    self.emit(Instruction::Add {
                        destination: target_reg.clone(),
                        source1: value_reg,
                        source2: Operand::Immediate(0),
                    });
                }
            },
            
            Expression::Lambda { parameters, return_type: _, body } => {
                // Lambda expressions are treated as anonymous functions
                let lambda_name = format!("lambda_{}", self.label_counter);
                self.label_counter += 1;
                
                // Generate function prologue
                self.emit(Instruction::Function {
                    name: lambda_name.clone(),
                    parameters: parameters.iter().map(|p| p.name.clone()).collect(),
                    body: Vec::new(),
                });
                
                // Generate lambda body
                self.generate_statement(&Statement::Block(body.clone()))?;
                
                // Return the function reference
                let temp_reg = self.allocate_register("temp");
                self.emit(Instruction::Add {
                    destination: temp_reg,
                    source1: Operand::Label(lambda_name),
                    source2: Operand::Immediate(0),
                });
            },
            
            Expression::If { condition, then_branch, else_branch } => {
                let temp_reg = self.allocate_register("temp");
                
                // Generate condition
                self.generate_expression(condition)?;
                
                let else_label = self.generate_label("else");
                let end_label = self.generate_label("endif");
                
                self.emit(Instruction::JumpIfNot {
                    condition: temp_reg,
                    target: else_label.clone(),
                });
                
                // Generate then branch
                self.generate_expression(then_branch)?;
                
                self.emit(Instruction::Jump { target: end_label.clone() });
                
                // Generate else branch if exists
                self.emit(Instruction::Jump { target: else_label.clone() });
                
                if let Some(else_branch) = else_branch {
                    self.generate_expression(else_branch)?;
                }
                
                self.emit(Instruction::Jump { target: end_label });
            },
            
            Expression::Match { expr, arms } => {
                // Generate match expression
                self.generate_expression(expr)?;
                
                let temp_reg = self.allocate_register("temp");
                let end_label = self.generate_label("endmatch");
                
                for arm in arms {
                    // Generate pattern matching (simplified)
                    self.generate_expression(&arm.pattern)?;
                    let temp_reg2 = self.allocate_register("temp");
                    
                    self.emit(Instruction::JumpIf {
                        condition: temp_reg2,
                        target: self.generate_label("match_arm"),
                    });
                }
                
                // Default case
                self.emit(Instruction::Jump { target: end_label.clone() });
                
                // Generate arm bodies
                for arm in arms {
                    self.emit(Instruction::Label(self.generate_label("match_arm")));
                    
                    if let Some(guard) = &arm.guard {
                        self.generate_expression(guard)?;
                        let guard_reg = self.allocate_register("temp");
                        
                        self.emit(Instruction::JumpIfNot {
                            condition: guard_reg,
                            target: end_label.clone(),
                        });
                    }
                    
                    self.generate_expression(&arm.body)?;
                    self.emit(Instruction::Jump { target: end_label.clone() });
                }
                
                self.emit(Instruction::Label(end_label));
            },
        }
        
        Ok(())
    }
}
