mod lexer;
mod parser;
mod typechecker;
mod optimizer;
mod codegen;
mod vm;
mod utils;

use std::path::Path;
use std::fs;
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
    
    let source: String;
    
    if Path::new(input_path).exists() {
        // Read file with proper encoding handling
        let file_bytes = fs::read(input_path)?;
        
        // Try UTF-8 first, then fall back to UTF-16 and UTF-8 BOM handling
        source = match String::from_utf8(file_bytes.clone()) {
            Ok(s) => s,
            Err(_) => {
                // Handle UTF-16 BOM if present
                if file_bytes.starts_with(b"\xFF\xFE") {
                    // UTF-16 Little Endian
                    if file_bytes.len() >= 2 {
                        match String::from_utf16(
                            file_bytes[2..]
                                .chunks_exact(2)
                                .map(|chunk| u16::from_le_bytes([chunk[0], chunk[1]]))
                                .collect::<Vec<u16>>()
                                .as_slice()
                        ) {
                            Ok(s) => s,
                            Err(_) => String::from_utf8_lossy(&file_bytes).to_string(),
                        }
                    } else {
                        String::from_utf8_lossy(&file_bytes).to_string()
                    }
                } else if file_bytes.starts_with(b"\xFE\xFF") {
                    // UTF-16 Big Endian
                    if file_bytes.len() >= 2 {
                        match String::from_utf16(
                            file_bytes[2..]
                                .chunks_exact(2)
                                .map(|chunk| u16::from_be_bytes([chunk[0], chunk[1]]))
                                .collect::<Vec<u16>>()
                                .as_slice()
                        ) {
                            Ok(s) => s,
                            Err(_) => String::from_utf8_lossy(&file_bytes).to_string(),
                        }
                    } else {
                        String::from_utf8_lossy(&file_bytes).to_string()
                    }
                } else if file_bytes.starts_with(b"\xEF\xBB\xBF") {
                    // UTF-8 BOM
                    String::from_utf8_lossy(&file_bytes[3..]).to_string()
                } else {
                    // Try to recover as much valid UTF-8 as possible
                    String::from_utf8_lossy(&file_bytes).to_string()
                }
            }
        };
        
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
    let _tokens = lexer.tokenize()?; // Tokens are currently unused but kept for future parser integration
    
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
    
    // Check if VM is ready before executing
    if vm_env.get_state() != &vm::VMState::Ready {
        eprintln!("Virtual machine is not ready to execute");
        return Err("VM not ready".into());
    }
    
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
