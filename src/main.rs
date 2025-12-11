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
    // Initialize the compiler pipeline
    let mut lexer = Lexer::new("");
    let mut parser = Parser::new(lexer);
    let mut type_checker = TypeChecker::new();
    let mut optimizer = OptimizationPipeline::default();
    let mut code_generator = CodeGenerationPipeline::default();
    let mut vm_env = VirtualMachineEnvironment::new();
    
    // Check if a file path was provided as an argument
    let args: Vec<String> = std::env::args().collect();
    let input_path = if args.len() > 1 {
        &args[1]
    } else {
        "examples/basic.px" // Default example file
    };
    
    // Read the input file
    let mut source = String::new();
    if Path::new(input_path).exists() {
        let mut file = File::open(input_path)?;
        file.read_to_string(&mut source)?;
        
        println!("Compiling program from: {}", input_path);
        println!("=====================================");
    } else {
        eprintln!("Error: File '{}' not found. Using default program.", input_path);
        println!("=====================================");
        
        // Default program if no file is found
        source = r#"
        // Example program demonstrating various features
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
    
    // Lexing phase
    println!("1. Lexing...");
    let mut lexer = Lexer::new(&source);
    let tokens = lexer.lex()?;
    
    // Parsing phase
    println!("2. Parsing...");
    let mut parser = Parser::new(lexer);
    let program = parser.parse(tokens)?;
    
    // Type checking phase
    println!("3. Type checking...");
    type_checker.check_program(&program)?;
    
    if !type_checker.get_errors().is_empty() {
        eprintln!("Type checking errors found:");
        for error in type_checker.get_errors() {
            eprintln!("  - {}", error);
        }
        return Err("Type checking failed".into());
    }
    
    // Optimization phase
    println!("4. Optimizing...");
    optimizer.optimize(&mut program.clone())?;
    
    // Code generation phase
    println!("5. Code generation...");
    code_generator.generate(&program)?;
    
    // Virtual machine execution
    println!("6. Executing...");
    vm_env.load_program(&program)?;
    
    // Execute synchronously
    match vm_env.execute() {
        Ok(()) => {
            println!("Execution completed successfully!");
        },
        Err(e) => {
            eprintln!("Execution error: {}", e);
            return Err(e.into());
        }
    }
    
    // Print execution statistics
    println!("=====================================");
    println!("Execution statistics:");
    println!("  - Memory usage: {} bytes", vm_env.get_memory_usage());
    let (gc_collections, gc_freed) = vm_env.get_gc_stats();
    println!("  - GC collections: {}", gc_collections);
    println!("  - GC freed objects: {}", gc_freed);
    println!("  - VM state: {:?}", vm_env.get_state());
    
    Ok(())
}
