extern crate clap;
extern crate thiserror;

pub mod parser;
pub mod lexer;
pub mod interpreter;

use std::{collections::HashSet, hash::Hash, time::Instant, io::{Stdin, self}};

use clap::{command, arg, Parser, ValueEnum};
use colored::Colorize;
use lexer::lexer::Lexer;

use crate::interpreter::{Runtime, ast_interpreter::AstInterpreter, Interpreter};

/// Brainf**k compiler/optimizer/JIT/AOT/interpreter
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// The file to operate on
    #[arg()]
    file: String,

    #[arg(value_enum)]
    commands: Vec<Commands>,

    #[arg(short, long, default_value_t=30_000)]
    default_runtime_size: usize,
}

#[derive(ValueEnum, Debug, Clone, Hash, PartialEq, Eq)]
enum Commands {
    /// Run the lexer
    Lexer,
    /// Run the parser
    Parser,
    /// Run the AST as is
    AstInterpreter,
}

fn main() -> Result<(), ()> {
    let args = Args::parse();
    let commands: HashSet<Commands> = HashSet::from_iter(args.commands.into_iter());
    
    println!("Running {}", args.file);

    let text = std::fs::read_to_string(args.file).unwrap();
    let mut lexer = Lexer::new(&text);

    println!("{}", "Starting lexing".green());
    let now = Instant::now();
    let result = lexer.collect_results().unwrap();
    println!("{} {:.2?}", "Finished lexing in".green(), now.elapsed());

    if commands.contains(&Commands::Lexer) {
        for token in result.iter() {
            print!("{}", match &token {
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
                },
            });
        }
        println!();
    }

    let mut ast = parser::parser::Parser::new(&result);
    
    println!("{}", "Starting parsing".green());
    let now = Instant::now();
    let program = ast.parse_program();
    println!("{} {:.2?}", "Finished parsing in".green(), now.elapsed());

    if commands.contains(&Commands::Parser) {
        println!("{:#?}", program);
    }

    let stdin = io::stdin();
    let stdout = io::stdout();
    // TODO: Let runtime/multiple interpreters be run at the same time
    let mut runtime = Runtime::new(args.default_runtime_size, Box::new(stdin), Box::new(stdout));
    if commands.contains(&Commands::AstInterpreter) {
        println!("{}", "Starting ast-interpreter".green());
        AstInterpreter::new().interpret(&mut runtime, &program);
        println!();
        println!("{} {:.2?}", "Finished ast-interpreter in".green(), now.elapsed());
    }

    Ok(())
}
