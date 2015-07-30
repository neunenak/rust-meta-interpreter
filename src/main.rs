use std::io;
use std::io::Write;
use std::io::BufRead;
use std::process;

use tokenizer::tokenize;
use parser::{parse, ParseResult};
use evaluate::{evaluate, Environment};

mod tokenizer;
mod parser;
mod evaluate;


fn main() {
    println!("Unnamed language 0.01");
    repl();
}

fn repl() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut buf = String::with_capacity(20);
    loop {
        buf.clear();
        print!(">> ");
        stdout.flush().ok();
        let line = stdin.lock().read_line(&mut buf);

        match line {
            Ok(_) => {
                if buf.is_empty() {
                    break;
                }

                if handle_interpreter_directive(&buf) {
                    continue;
                }

                let tokens = tokenize(&buf);
                println!("Tokens: {:?}", tokens);

                match parse(tokens) {
                    ParseResult::Ok(ast) => {
                        println!("AST: {:?}", ast);

                        let env = Environment::new();
                        let eval = evaluate(ast, env);
                        println!("{}", eval);
                    },
                    ParseResult::Err(err) => println!("Error: {}", err)
                }
            },
            Err(err) => {
                println!("Error: {}", err);
            }
        }
    }
}

fn handle_interpreter_directive(input: &str) -> bool {

    match input.chars().nth(0) {
        Some('.') => (),
        _ => return false
    }

    let commands: Vec<&str> = input.split(|c: char| c.is_whitespace()).collect();
    match commands.get(0) {
        Some(s) if *s == ".quit" => {
            println!("Siturei simasu");
            process::exit(0);
        },
        Some(s) => {
            println!("Unknown directive: {}", s);
        },
        None => () //should never happen
    }

    return true;
}
