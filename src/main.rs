extern crate clap;
extern crate thiserror;

pub mod bytecode;
pub mod interpreter;
#[allow(dead_code)]
pub mod jit;
pub mod lexer;
pub mod optimizer;
pub mod parser;

use std::{
    collections::HashSet,
    hash::Hash,
    io::{self},
    time::Instant,
};

use clap::{arg, command, Parser, ValueEnum};
use colored::Colorize;
use lexer::lexer::Lexer;

use crate::{
    bytecode::bytecode::to_bytecode,
    interpreter::{
        ast_interpreter::AstInterpreter, bytecode_interpreter::ByteCodeInterpreter,
        mir_interpreter::MirInterpreter, Runtime,
    },
    optimizer::optimize,
};

/// Brainf**k compiler/optimizer/JIT/AOT/interpreter
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// The file to operate on
    #[arg()]
    file: String,

    #[arg(value_enum)]
    commands: Vec<Commands>,

    #[arg(short, long, value_enum)]
    optimizations: Vec<Optimizations>,

    #[arg(short, long)]
    all_optimizations: bool,

    #[arg(short, long, default_value_t = 30_000)]
    default_runtime_size: usize,
}

#[derive(ValueEnum, Debug, Clone, Hash, PartialEq, Eq)]
pub enum Optimizations {
    /// Fold calculations into a constant
    ConstantFolding,
    CommentBlock,
}

#[derive(ValueEnum, Debug, Clone, Hash, PartialEq, Eq)]
enum Commands {
    /// Output the lexer
    Tokens,
    /// Output the ast
    Ast,
    /// Output Bytecode
    Bytecode,
    /// Output MIR ast
    MirAst,

    /// Run the AST as is
    AstInterpreter,
    /// Run the MIR interpreter
    MirInterpreter,
    /// Run the bytecode interpreter
    BytecodeInterpreter,
}

fn main() -> Result<(), ()> {
    let args = Args::parse();
    let commands: HashSet<Commands> = HashSet::from_iter(args.commands.into_iter());
    let mut optimizations: HashSet<Optimizations> =
        HashSet::from_iter(args.optimizations.into_iter());
    // TODO: Loop through all enum variants to avoid this duplication
    if args.all_optimizations {
        optimizations.insert(Optimizations::CommentBlock);
        optimizations.insert(Optimizations::ConstantFolding);
    }

    println!("Running {}", args.file);

    let text = std::fs::read_to_string(args.file).unwrap();
    let mut lexer = Lexer::new(&text);

    println!("{}", "Starting lexing".blue());
    let now = Instant::now();
    let result = lexer.collect_results().unwrap();
    println!("{} {:.2?}", "Finished lexing in".green(), now.elapsed());

    if commands.contains(&Commands::Tokens) {
        for token in result.iter() {
            print!(
                "{}",
                match &token {
                    lexer::LexerTokenKind::Increment => "<",
                    lexer::LexerTokenKind::Decrement => ">",
                    lexer::LexerTokenKind::DerefIncrement => "+",
                    lexer::LexerTokenKind::DerefDecrement => "-",
                    lexer::LexerTokenKind::Write => ".",
                    lexer::LexerTokenKind::Read => ",",
                    lexer::LexerTokenKind::JumpStart => "[",
                    lexer::LexerTokenKind::JumpEnd => "]",
                    lexer::LexerTokenKind::EOF => "\n",
                    lexer::LexerTokenKind::Comment(c) => {
                        print!("\t\t");
                        c.as_str()
                    }
                }
            );
        }
        println!();
    }

    let mut ast = parser::parser::Parser::new(&result);

    println!("{}", "Starting parsing".blue());
    let mut now: Instant = Instant::now();
    let program = ast.parse_program();
    println!("{} {:.2?}", "Finished parsing in".green(), now.elapsed());

    if commands.contains(&Commands::Ast) {
        println!("{:#?}", program);
    }

    let stdin = io::stdin();
    let stdout = io::stdout();

    // TODO: Let runtime/multiple interpreters be run at the same time
    let mut runtime = Runtime::new(args.default_runtime_size, Box::new(stdin), Box::new(stdout));
    if commands.contains(&Commands::AstInterpreter) {
        println!("{}", "Starting ast-interpreter".blue());
        now = Instant::now();
        AstInterpreter::new().interpret(&mut runtime, &program);
        runtime.reset();
        println!();
        println!(
            "{} {:.2?}",
            "Finished ast-interpreter in".green(),
            now.elapsed()
        );
    }

    println!("{} {:?}", "Starting optimizations".blue(), &optimizations);
    now = Instant::now();
    let optimized_program = optimize(&program, &optimizations);
    println!(
        "{} in {:.2?}",
        "Finished optimizations".green(),
        now.elapsed()
    );

    if commands.contains(&Commands::MirAst) {
        println!("{:#?}", optimized_program);
    }

    if commands.contains(&Commands::MirInterpreter) {
        println!("{}", "Starting mir-interpreter".blue());
        now = Instant::now();
        MirInterpreter::new().interpret(&mut runtime, &optimized_program);
        runtime.reset();
        println!();
        println!(
            "{} {:.2?}",
            "Finished mir-interpreter in".green(),
            now.elapsed()
        );
    }

    println!("{} {:?}", "Starting bytecode".blue(), optimizations);
    now = Instant::now();
    let bytecode = to_bytecode(&optimized_program);
    println!(
        "{} {} in {:.2?}",
        "Finished bytecode conversion with optimized length".green(),
        bytecode.len(),
        now.elapsed()
    );

    if commands.contains(&Commands::Bytecode) {
        println!("{:?}", bytecode);
    }

    if commands.contains(&Commands::BytecodeInterpreter) {
        println!("{}", "Starting bytecode-interpreter".blue());
        now = Instant::now();
        // TODO: Remove this once we have a better way of benchmarking this (using rust benchmarks)
        for _ in 0..10000 {
            ByteCodeInterpreter::new().run(&mut runtime, &bytecode);
            runtime.reset();
        }
        println!();
        println!(
            "{} {:.2?}",
            "Finished bytecode-interpreter in".green(),
            now.elapsed()
        );
    }

    Ok(())
}
