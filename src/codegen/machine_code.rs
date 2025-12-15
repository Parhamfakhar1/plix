use crate::parser::ast::{Program, Statement, Expression};
use crate::utils::error::CompilerResult;

use super::CodeGenerator;

/// Supported target architectures for machine code generation.
#[derive(Debug, Clone, Copy)]
pub enum TargetArchitecture {
    X86_64,
}

/// Stub machine-code generator; currently a no-op implementation.
pub struct MachineCodeGenerator {
    _target: TargetArchitecture,
}

impl MachineCodeGenerator {
    pub fn new(target: TargetArchitecture) -> Self {
        Self { _target: target }
    }
}

impl CodeGenerator for MachineCodeGenerator {
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


