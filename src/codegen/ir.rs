#![allow(dead_code)]

use crate::parser::ast::{Program, Statement, Expression, BinaryOp, UnaryOp, Literal};
use crate::utils::error::CompilerResult;
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
    Add { destination: Operand, source1: Operand, source2: Operand },
    Sub { destination: Operand, source1: Operand, source2: Operand },
    Mul { destination: Operand, source1: Operand, source2: Operand },
    Div { destination: Operand, source1: Operand, source2: Operand },
    Mod { destination: Operand, source1: Operand, source2: Operand },
    
    And { destination: Operand, source1: Operand, source2: Operand },
    Or { destination: Operand, source1: Operand, source2: Operand },
    Xor { destination: Operand, source1: Operand, source2: Operand },
    Not { destination: Operand, source: Operand },
    Shl { destination: Operand, source1: Operand, source2: Operand },
    Shr { destination: Operand, source1: Operand, source2: Operand },
    
    Load { destination: Operand, address: Operand },
    Store { address: Operand, source: Operand },
    Alloc { destination: Operand, size: Operand },
    Free { address: Operand },
    
    Jump { target: Operand },
    JumpIf { condition: Operand, target: Operand },
    JumpIfNot { condition: Operand, target: Operand },
    Call { function: Operand, arguments: Vec<Operand> },
    Return { value: Option<Operand> },
    
    Function { name: String, parameters: Vec<String>, body: Vec<Instruction> },
    Prologue,
    Epilogue,
    
    Push { source: Operand },
    Pop { destination: Operand },
    
    Compare { source1: Operand, source2: Operand },
    SetEqual { destination: Operand },
    SetNotEqual { destination: Operand },
    SetLess { destination: Operand },
    SetLessEqual { destination: Operand },
    SetGreater { destination: Operand },
    SetGreaterEqual { destination: Operand },

    /// Marks a label position in the instruction stream.
    Label(String),
    
    ConvertToInt { destination: Operand, source: Operand },
    ConvertToFloat { destination: Operand, source: Operand },
    ConvertToString { destination: Operand, source: Operand },
    ConvertToBool { destination: Operand, source: Operand },
    
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
                self.emit(Instruction::Nop);
            }
        }
    }

    fn generate_comparison_op(&mut self, op: BinaryOp, destination: &Operand, left: &Operand, right: &Operand) {
        self.emit(Instruction::Compare {
            source1: left.clone(),
            source2: right.clone(),
        });
        
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
                self.emit(Instruction::Add {
                    destination: destination.clone(),
                    source1: source.clone(),
                    source2: Operand::Immediate(0),
                });
            },
            UnaryOp::Minus => {
                self.emit(Instruction::Sub {
                    destination: destination.clone(),
                    source1: Operand::Immediate(0),
                    source2: source.clone(),
                });
            },
            UnaryOp::Not => {
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
            Literal::String(_value) => Operand::Immediate(0),
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
        self.emit(Instruction::Comment("Program prologue".to_string()));
        
        for stmt in &program.statements {
            self.generate_statement(stmt)?;
        }
        
        self.emit(Instruction::Comment("Program epilogue".to_string()));
        self.emit(Instruction::Return { value: None });
        
        Ok(())
    }

    fn generate_statement(&mut self, stmt: &Statement) -> CompilerResult<()> {
        match stmt {
            Statement::Expression(expr, _) => {
                self.generate_expression(expr)?;
            },

            Statement::Variable { mutable: _, name, type_annotation: _, value, .. } => {
                let temp_reg = self.allocate_register("temp");
                self.generate_expression(value)?;
                
                self.store_symbol(name, temp_reg);
            },

            Statement::Constant { name, type_annotation: _, value, .. } => {
                let temp_reg = self.allocate_register("temp");
                self.generate_expression(value)?;
                
                self.store_symbol(name, temp_reg);
            },

            Statement::Return(value, _) => {
                if let Some(expr) = value {
                    self.generate_expression(expr)?;
                }
                self.emit(Instruction::Return { value: None });
            },

            Statement::If { condition, then_branch, elif_branches: _, else_branch, .. } => {
                self.generate_expression(condition)?;
                
                let temp_reg = self.allocate_register("temp");
                let else_label = self.generate_label("else");
                let end_label = self.generate_label("endif");
                
                self.emit(Instruction::JumpIfNot {
                    condition: temp_reg,
                    target: else_label.clone(),
                });
                
                for stmt in then_branch {
                    self.generate_statement(stmt)?;
                }
                
                self.emit(Instruction::Jump { target: end_label.clone() });
                
                self.emit(Instruction::Jump { target: else_label.clone() });
                
                if let Some(else_body) = else_branch {
                    for stmt in else_body {
                        self.generate_statement(stmt)?;
                    }
                }
                
                self.emit(Instruction::Jump { target: end_label });
            },

            Statement::While { condition, body, .. } => {
                let loop_start = self.generate_label("while_start");
                let loop_end = self.generate_label("while_end");
                
                self.emit(Instruction::Comment("While loop start".to_string()));
                
                self.emit(Instruction::Jump { target: loop_start.clone() });

                if let Operand::Label(name) = loop_start.clone() {
                    self.emit(Instruction::Label(name));
                }
                
                for stmt in body {
                    self.generate_statement(stmt)?;
                }
                
                self.generate_expression(condition)?;
                let temp_reg = self.allocate_register("temp");
                
                self.emit(Instruction::JumpIf {
                    condition: temp_reg,
                    target: loop_start.clone(),
                });
                
                self.emit(Instruction::Jump { target: loop_end.clone() });
                if let Operand::Label(name) = loop_end {
                    self.emit(Instruction::Label(name));
                }
                self.emit(Instruction::Comment("While loop end".to_string()));
            },
            
            Statement::For { variable, iterable, body, .. } => {
                
                let temp_reg = self.allocate_register("temp");
                self.generate_expression(iterable)?;
                self.store_symbol(variable, temp_reg);
                
                for stmt in body {
                    self.generate_statement(stmt)?;
                }
            },

            Statement::Match { expr, arms, .. } => {
                // Very simple lowering: evaluate the match expression, then each arm's guard/body.
                self.generate_expression(expr)?;

                for arm in arms {
                    if let Some(guard) = &arm.guard {
                        self.generate_expression(guard)?;
                    }
                    self.generate_expression(&arm.body)?;
                }
            },

            Statement::Block(statements, _) => {
                for stmt in statements {
                    self.generate_statement(stmt)?;
                }
            },

            Statement::Function { name, parameters, return_type: _, body, async_flag: _, .. } => {
                let _func_label = self.generate_label("function");
                let _prologue_label = self.generate_label("prologue");
                let _epilogue_label = self.generate_label("epilogue");
                
                self.emit(Instruction::Function {
                    name: name.clone(),
                    parameters: parameters.iter().map(|p| p.name.clone()).collect(),
                    body: Vec::new(),
                });
                
                self.current_function = Some(name.clone());
                
                self.emit(Instruction::Prologue);
                
                for stmt in body {
                    self.generate_statement(stmt)?;
                }
                
                self.emit(Instruction::Epilogue);
                self.emit(Instruction::Return { value: None });
                
                self.current_function = None;
            },

            Statement::Class { name, base: _base, fields, methods, .. } => {
                let temp_reg = self.allocate_register("temp");
                
                self.emit(Instruction::Alloc {
                    destination: temp_reg.clone(),
                    size: Operand::Immediate(8 * fields.len() as i64),
                });
                
                self.store_symbol(name, temp_reg);
                
                for method in methods {
                    self.generate_statement(method)?;
                }
            },
            
            Statement::Import { module, alias: _, items: _, .. } => {
                self.emit(Instruction::Comment(format!("Import: {}", module)));
            },

            Statement::Enum { name, generics: _, variants, .. } => {
                // For enums, we just emit a comment for now
                self.emit(Instruction::Comment(format!("Enum: {}", name)));
                
                // Generate IR for each variant
                for variant in variants {
                    self.emit(Instruction::Comment(format!("Variant: {}", variant.name)));
                }
            },
        }
        
        Ok(())
    }

    fn generate_expression(&mut self, expr: &Expression) -> CompilerResult<()> {
        match expr {
            Expression::Identifier(name, _) => {
                if let Some(operand) = self.load_symbol(name) {
                    let temp_reg = self.allocate_register("temp");
                    self.emit(Instruction::Load {
                        destination: temp_reg,
                        address: operand,
                    });
                }
            },
            
            Expression::Literal(literal, _) => {
                let temp_reg = self.allocate_register("temp");
                let operand = self.generate_literal(literal);
                self.emit(Instruction::Add {
                    destination: temp_reg,
                    source1: operand,
                    source2: Operand::Immediate(0),
                });
            },
            
            Expression::Binary { left, op, right, .. } => {
                let temp_reg = self.allocate_register("temp");
                
                self.generate_expression(left)?;
                let left_reg = self.allocate_register("left");
                
                self.generate_expression(right)?;
                let right_reg = self.allocate_register("right");
                
                match op {
                    BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide | BinaryOp::Modulo => {
                        self.generate_arithmetic_op(op.clone(), &temp_reg, &left_reg, &right_reg);
                    },
                    
                    BinaryOp::Equal | BinaryOp::NotEqual | BinaryOp::Less | BinaryOp::LessEqual | 
                    BinaryOp::Greater | BinaryOp::GreaterEqual => {
                        self.generate_comparison_op(op.clone(), &temp_reg, &left_reg, &right_reg);
                    },
                    
                    _ => {
                        self.emit(Instruction::Nop);
                    }
                }
            },
            
            Expression::Unary { op, expr, .. } => {
                let temp_reg = self.allocate_register("temp");
                
                self.generate_expression(expr)?;
                let operand_reg = self.allocate_register("operand");
                
                self.generate_unary_op(op.clone(), &temp_reg, &operand_reg);
            },
            
            Expression::Call { function, arguments, .. } => {
                let mut arg_operands = Vec::new();
                for arg in arguments {
                    self.generate_expression(arg)?;
                    let temp_reg = self.allocate_register("arg");
                    arg_operands.push(temp_reg);
                }
                
                self.generate_expression(function)?;
                let func_reg = self.allocate_register("func");
                
                self.emit(Instruction::Call {
                    function: func_reg,
                    arguments: arg_operands,
                });
            },
            
            Expression::Index { expr, index, .. } => {
                self.generate_expression(expr)?;
                self.allocate_register("array"); // placeholder register to keep flow intact
                
                self.generate_expression(index)?;
                self.allocate_register("index"); // placeholder register
                
                let dest_reg = self.allocate_register("temp");
                self.emit(Instruction::Load {
                    destination: dest_reg,
                    address: Operand::Memory {
                        base: None,
                        offset: 0,
                        scale: 8,
                    },
                });
            },
            
            Expression::Member { expr, member: _member, .. } => {
                self.generate_expression(expr)?;
                self.allocate_register("object"); // placeholder
                
                let dest_reg = self.allocate_register("temp");
                self.emit(Instruction::Load {
                    destination: dest_reg,
                    address: Operand::Memory {
                        base: None,
                        offset: 0,
                        scale: 8,
                    },
                });
            },
            
            Expression::Assignment { target, value, op, .. } => {
                self.generate_expression(value)?;
                let value_reg = self.allocate_register("value");
                
                self.generate_expression(target)?;
                let target_reg = self.allocate_register("target");
                
                if let Some(assignment_op) = op {
                    self.generate_arithmetic_op(assignment_op.clone(), &target_reg, &target_reg, &value_reg);
                } else {
                    self.emit(Instruction::Add {
                        destination: target_reg.clone(),
                        source1: value_reg,
                        source2: Operand::Immediate(0),
                    });
                }
            },
            
            Expression::Lambda { parameters, return_type: _, body, .. } => {
                let lambda_name = format!("lambda_{}", self.label_counter);
                self.label_counter += 1;
                
                self.emit(Instruction::Function {
                    name: lambda_name.clone(),
                    parameters: parameters.iter().map(|p| p.name.clone()).collect(),
                    body: Vec::new(),
                });
                
                // Generate IR for the lambda body.
                self.generate_statement(&*body)?;
                
                let temp_reg = self.allocate_register("temp");
                self.emit(Instruction::Add {
                    destination: temp_reg,
                    source1: Operand::Label(lambda_name),
                    source2: Operand::Immediate(0),
                });
            },
            
            Expression::If { condition, then_branch, else_branch, .. } => {
                let temp_reg = self.allocate_register("temp");
                
                self.generate_expression(condition)?;
                
                let else_label = self.generate_label("else");
                let end_label = self.generate_label("endif");
                
                self.emit(Instruction::JumpIfNot {
                    condition: temp_reg,
                    target: else_label.clone(),
                });
                
                self.generate_expression(then_branch)?;
                
                self.emit(Instruction::Jump { target: end_label.clone() });
                
                self.emit(Instruction::Jump { target: else_label.clone() });
                
                if let Some(else_branch) = else_branch {
                    self.generate_expression(else_branch)?;
                }
                
                self.emit(Instruction::Jump { target: end_label });
            },
            
            Expression::Match { expr, arms, .. } => {
                self.generate_expression(expr)?;
                
                let _temp_reg = self.allocate_register("temp");
                let end_label = self.generate_label("endmatch");
                
                for arm in arms {
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

                if let Operand::Label(name) = end_label {
                    self.emit(Instruction::Label(name));
                }
            },

            Expression::VariantCall { enum_name, variant_name, arguments, .. } => {
                // For variant calls, we just emit a comment for now
                self.emit(Instruction::Comment(format!("Variant call: {}.{}", enum_name, variant_name)));
                
                // Generate IR for each argument
                for arg in arguments {
                    self.generate_expression(arg)?;
                }
            },

            Expression::Try { expr, .. } => {
                // For try expressions, we just generate the inner expression for now
                self.generate_expression(expr)?;
            },

            Expression::Block { statements, .. } => {
                for stmt in statements {
                    self.generate_statement(stmt)?;
                }
            },
        }
        
        Ok(())
    }
}
