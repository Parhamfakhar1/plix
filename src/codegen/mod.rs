pub mod ir;
pub mod machine_code;
pub mod bytecode;

#[allow(unused_imports)]
pub use self::ir::{IRGenerator, Instruction, Operand};
#[allow(unused_imports)]
pub use self::machine_code::{MachineCodeGenerator, TargetArchitecture};
#[allow(unused_imports)]
pub use self::bytecode::{BytecodeGenerator, Bytecode, OpCode};

use crate::parser::ast::{Program, Statement, Expression};
use crate::utils::error::CompilerResult;

pub trait CodeGenerator {
    fn generate_program(&mut self, program: &Program) -> CompilerResult<()>;
    fn generate_statement(&mut self, stmt: &Statement) -> CompilerResult<()>;
    fn generate_expression(&mut self, expr: &Expression) -> CompilerResult<()>;
}

pub struct CodeGenerationPipeline {
    generators: Vec<Box<dyn CodeGenerator>>,
}

impl CodeGenerationPipeline {
    pub fn new() -> Self {
        Self {
            generators: Vec::new(),
        }
    }

    pub fn add_generator(&mut self, generator: Box<dyn CodeGenerator>) {
        self.generators.push(generator);
    }

    pub fn generate(&mut self, program: &Program) -> CompilerResult<()> {
        for generator in &mut self.generators {
            generator.generate_program(program)?;
        }
        Ok(())
    }
}

impl Default for CodeGenerationPipeline {
    fn default() -> Self {
        let mut pipeline = Self::new();
        
        pipeline.add_generator(Box::new(IRGenerator::new()));
        pipeline.add_generator(Box::new(BytecodeGenerator::new()));
        pipeline.add_generator(Box::new(MachineCodeGenerator::new(TargetArchitecture::X86_64)));
        
        pipeline
    }
}
