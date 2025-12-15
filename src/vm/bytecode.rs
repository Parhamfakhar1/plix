#![allow(dead_code)]

use crate::parser::ast::Program;
use crate::utils::error::CompilerResult;

use super::garbage_collector::GarbageCollector;
use super::runtime::Runtime;

pub type VMResult<T> = Result<T, VMError>;

#[derive(Debug)]
pub enum VMError {
    NotReady,
    RuntimeError(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VMState {
    Initialized,
    Ready,
    Running,
    Finished,
    Error,
}

/// Minimal stub virtual machine that immediately "executes" successfully.
#[derive(Debug)]
pub struct VirtualMachine {
    state: VMState,
}

impl VirtualMachine {
    pub fn new() -> Self {
        Self {
            state: VMState::Initialized,
        }
    }

    pub fn compile(&mut self, _program: &Program) -> CompilerResult<()> {
        // In a real VM this would translate the AST into bytecode.
        self.state = VMState::Ready;
        Ok(())
    }

    pub fn execute(&mut self) -> VMResult<()> {
        // Pretend we executed the compiled program successfully.
        self.state = VMState::Finished;
        Ok(())
    }

    pub fn execute_with_gc<F>(&mut self, _gc: &mut GarbageCollector, f: F) -> VMResult<()>
    where
        F: FnOnce(&mut Self) -> VMResult<()>,
    {
        f(self)
    }

    pub fn execute_async(&mut self, _runtime: &mut Runtime, _gc: &mut GarbageCollector) -> VMResult<()> {
        // For now, just call the synchronous path.
        self.execute()
    }

    pub fn reset(&mut self) {
        self.state = VMState::Initialized;
    }
}

impl std::fmt::Display for VMError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VMError::NotReady => write!(f, "virtual machine is not ready to execute"),
            VMError::RuntimeError(msg) => write!(f, "VM runtime error: {}", msg),
        }
    }
}

impl std::error::Error for VMError {}



