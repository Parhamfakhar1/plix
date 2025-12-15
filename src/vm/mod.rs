#![allow(dead_code)]

pub mod bytecode;
pub mod garbage_collector;
pub mod runtime;

pub use self::bytecode::{VirtualMachine, VMState, VMError, VMResult};
pub use self::garbage_collector::{GarbageCollector, GCStrategy};
#[allow(unused_imports)]
pub use self::runtime::{Runtime, RuntimeEnvironment, ExecutionContext};

use crate::parser::ast::Program;
use crate::utils::error::CompilerResult;

pub struct VirtualMachineEnvironment {
    vm: VirtualMachine,
    gc: GarbageCollector,
    runtime: Runtime,
    state: VMState,
}

impl VirtualMachineEnvironment {
    pub fn new() -> Self {
        Self {
            vm: VirtualMachine::new(),
            gc: GarbageCollector::new(GCStrategy::Generational),
            runtime: Runtime::new(),
            state: VMState::Initialized,
        }
    }

    pub fn load_program(&mut self, program: &Program) -> CompilerResult<()> {
        self.vm.compile(program)?;
        
        self.runtime.initialize(program)?;
        
        Ok(())
    }

    pub fn execute(&mut self) -> VMResult<()> {
        if self.state != VMState::Ready {
            return Err(VMError::NotReady);
        }

        self.state = VMState::Running;
        
        let gc_result = self.vm.execute_with_gc(&mut self.gc, |vm| {
            vm.execute()
        });

        match gc_result {
            Ok(()) => {
                self.state = VMState::Finished;
                Ok(())
            },
            Err(e) => {
                self.state = VMState::Error;
                Err(e)
            }
        }
    }

    pub fn execute_async(&mut self) -> VMResult<()> {
        if self.state != VMState::Ready {
            return Err(VMError::NotReady);
        }

        self.state = VMState::Running;
        
        let result = self.vm.execute_async(&mut self.runtime, &mut self.gc);
        
        match result {
            Ok(()) => {
                self.state = VMState::Finished;
                Ok(())
            },
            Err(e) => {
                self.state = VMState::Error;
                Err(e)
            }
        }
    }

    pub fn get_state(&self) -> &VMState {
        &self.state
    }

    pub fn get_memory_usage(&self) -> usize {
        self.gc.get_memory_usage()
    }

    pub fn get_gc_stats(&self) -> (usize, usize) {
        self.gc.get_stats()
    }

    pub fn reset(&mut self) {
        self.vm.reset();
        self.gc.reset();
        self.runtime.reset();
        self.state = VMState::Initialized;
    }
}

impl Default for VirtualMachineEnvironment {
    fn default() -> Self {
        Self::new()
    }
}
