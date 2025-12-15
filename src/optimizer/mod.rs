pub mod tree_shaking;
pub mod constant_folding;
pub mod loop_optimization;

pub use self::tree_shaking::TreeShakingOptimizer;
pub use self::constant_folding::ConstantFoldingOptimizer;
pub use self::loop_optimization::LoopOptimizer;

use crate::parser::ast::{Program, Statement, Expression};
use crate::utils::error::CompilerResult;

pub trait Optimizer {
    fn optimize_program(&mut self, program: &mut Program) -> CompilerResult<()>;
    fn optimize_statement(&mut self, stmt: &mut Statement) -> CompilerResult<()>;
    #[allow(dead_code)]
    fn optimize_expression(&mut self, expr: &mut Expression) -> CompilerResult<()>;
}

pub struct OptimizationPipeline {
    optimizers: Vec<Box<dyn Optimizer>>,
}

impl OptimizationPipeline {
    pub fn new() -> Self {
        Self {
            optimizers: Vec::new(),
        }
    }

    pub fn add_optimizer(&mut self, optimizer: Box<dyn Optimizer>) {
        self.optimizers.push(optimizer);
    }

    pub fn optimize(&mut self, program: &mut Program) -> CompilerResult<()> {
        for optimizer in &mut self.optimizers {
            optimizer.optimize_program(program)?;
        }
        Ok(())
    }
}

impl Default for OptimizationPipeline {
    fn default() -> Self {
        let mut pipeline = Self::new();
        
        pipeline.add_optimizer(Box::new(ConstantFoldingOptimizer::new()));
        pipeline.add_optimizer(Box::new(LoopOptimizer::new()));
        pipeline.add_optimizer(Box::new(TreeShakingOptimizer::new()));
        
        pipeline
    }
}
