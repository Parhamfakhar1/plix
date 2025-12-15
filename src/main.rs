mod lexer;
mod parser;
mod typechecker;
mod optimizer;
mod codegen;
mod vm;
mod utils;

use std::path::Path;
use std::fs::File;
use std::io::Read;
use vm::VirtualMachineEnvironment;
use optimizer::OptimizationPipeline;
use codegen::CodeGenerationPipeline;
use typechecker::TypeChecker;
use parser::Parser;
use lexer::Lexer;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = std::env::args().collect();
    let input_path = if args.len() > 1 {
        &args[1]
    } else {
        "examples/basic.px"
    };
    
    let mut source = String::new();
    if Path::new(input_path).exists() {
        let mut file = File::open(input_path)?;
        file.read_to_string(&mut source)?;
        
        println!("Compiling program from: {}", input_path);
        println!("=====================================");
    } else {
        eprintln!("Warning: File '{}' not found. Using default program.", input_path);
        println!("=====================================");
        
        source = r#"
        let x = 10;
        let y = 20;
        let sum = x + y;
        
        function factorial(n) {
            if (n <= 1) {
                return 1;
            }
            return n * factorial(n - 1);
        }
        
        let result = factorial(5);
        print("Factorial of 5 is: ", result);
        "#.to_string();
    }
    
    println!("1. Lexing...");
    let lexer = Lexer::new(&source);
    let _tokens = lexer.tokenize()?;
    
    println!("2. Parsing...");
    let mut parser = Parser::new(&source)?;
    let program = parser.parse()?;
    
    println!("3. Type checking...");
    let mut type_checker = TypeChecker::new();
    type_checker.check_program(&program)?;
    
    if !type_checker.get_errors().is_empty() {
        eprintln!("Type checking errors found:");
        for error in type_checker.get_errors() {
            eprintln!("  - {}", error);
        }
        return Err("Type checking failed".into());
    }
    
    println!("4. Optimizing...");
    let mut optimizer = OptimizationPipeline::default();
    let mut optimized_program = program.clone();
    optimizer.optimize(&mut optimized_program)?;
    
    println!("5. Code generation...");
    let mut code_generator = CodeGenerationPipeline::default();
    code_generator.generate(&optimized_program)?;
    
    println!("6. Executing...");
    let mut vm_env = VirtualMachineEnvironment::new();
    vm_env.load_program(&optimized_program)?;
    
    match vm_env.execute() {
        Ok(()) => {
            println!("Execution completed successfully!");
        },
        Err(e) => {
            eprintln!("Execution error: {}", e);
            return Err(e.into());
        }
    }
    
    println!("=====================================");
    println!("Execution statistics:");
    println!("  - Memory usage: {} bytes", vm_env.get_memory_usage());
    let (gc_collections, gc_freed) = vm_env.get_gc_stats();
    println!("  - GC collections: {}", gc_collections);
    println!("  - GC freed objects: {}", gc_freed);
    println!("  - VM state: {:?}", vm_env.get_state());
    
    Ok(())
}
