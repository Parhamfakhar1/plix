use criterion::{black_box, criterion_group, criterion_main, Criterion};
use plix_compiler::Lexer;

fn lexer_benchmark(c: &mut Criterion) {
    let source_code = r#"
def fibonacci(n: number) -> number:
    if n <= 1:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)

const result = fibonacci(10)
print("Result: ${result}")
"#;
    
    c.bench_function("lexer basic", |b| {
        b.iter(|| {
            let lexer = Lexer::new(black_box(source_code));
            let _ = lexer.tokenize();
        })
    });
    
    c.bench_function("lexer large", |b| {
        let large_source = source_code.repeat(100);
        b.iter(|| {
            let lexer = Lexer::new(black_box(&large_source));
            let _ = lexer.tokenize();
        })
    });
}

criterion_group!(benches, lexer_benchmark);
criterion_main!(benches);