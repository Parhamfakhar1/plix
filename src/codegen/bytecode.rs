use crate::parser::ast::{Program, Statement, Expression};
use crate::utils::error::CompilerResult;

use super::CodeGenerator;

/// Simple placeholder bytecode representation.
#[derive(Debug, Clone)]
pub struct Bytecode;

/// Minimal opcode enum for future extension.
#[derive(Debug, Clone, Copy)]
pub enum OpCode {
    Nop,
}

/// No-op bytecode generator used to keep the pipeline compiling.
#[derive(Default)]
pub struct BytecodeGenerator;

impl BytecodeGenerator {
    pub fn new() -> Self {
        Self
    }
}

impl CodeGenerator for BytecodeGenerator {
    fn generate_program(&mut self, _program: &Program) -> CompilerResult<()> {
        Ok(())
    }

    fn generate_statement(&mut self, _stmt: &Statement) -> CompilerResult<()> {
        Ok(())
    }

    fn generate_expression(&mut self, _expr: &Expression) -> CompilerResult<()> {
        Ok(())
    }
}


