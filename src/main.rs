use std::io;
use std::io::Write;
use std::io::BufRead;

use tokenizer::tokenize;
use parser::{parse, ParseResult};

mod tokenizer;
mod parser;


fn main() {
    println!("Unnamed language 0.01");
    repl();
}

fn repl() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut buf = String::with_capacity(20);
    loop {
        print!(">> ");
        stdout.flush().ok();
        let line = stdin.lock().read_line(&mut buf);
        match line {
            Ok(_) => {
                if buf.is_empty() {
                    break;
                }
                let tokens = tokenize(&buf);
                buf.clear();
                println!("Tokens: {:?}", tokens);

                match parse(tokens) {
                    ParseResult::Ok(ast) => println!("AST: {:?}", ast),
                    ParseResult::Err(err) => println!("Error: {}", err)
                }

                /*
                let eval = evaluate(&ast);
                println!("{}", eval);
                */
            },
            Err(err) => {
                println!("Error: {}", err);
            }
        }
    }
}
