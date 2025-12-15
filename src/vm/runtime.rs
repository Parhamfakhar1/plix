use crate::parser::ast::Program;
use crate::utils::error::CompilerResult;

/// Placeholder runtime environment types.
#[derive(Debug, Default)]
pub struct RuntimeEnvironment;

#[derive(Debug, Default)]
pub struct ExecutionContext;

#[derive(Debug, Default)]
pub struct Runtime;

impl Runtime {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn initialize(&mut self, _program: &Program) -> CompilerResult<()> {
        Ok(())
    }

    pub fn reset(&mut self) {}
}


