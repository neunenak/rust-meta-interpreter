#![feature(advanced_slice_patterns, slice_patterns, box_patterns)]

extern crate simplerepl;

use std::path::Path;
use std::fs::File;
use std::io::Read;

use simplerepl::{REPL, ReplState};

use tokenizer::tokenize;
mod tokenizer;

use parser::{parse};
mod parser;

use eval::{Evaluator};
mod eval;

use compilation::{compile_ast};
mod compilation;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if let Some(filename) = args.get(1) {
        run_noninteractive(filename);
    } else {
        run_repl();
    }
}

fn run_noninteractive(filename: &String) {
    let mut source_file = File::open(&Path::new(filename)).unwrap();
    let mut buffer = String::new();
    source_file.read_to_string(&mut buffer).unwrap();

    let tokens = match tokenize(&buffer) {
        Some(t) => t,
        None => { println!("Tokenization error"); return; }
    };

    let ast = match parse(&tokens, &[]) {
        Ok(ast) => ast,
        Err(err) => { println!("Parse error: {:?}", err); return; }
    };

    let compile = true;

    if compile {
        compile_ast(ast);
    } else {
        let mut evaluator = Evaluator::new();
        let results = evaluator.run(ast);
        for result in results.iter() {
            println!("{}", result);
        }
    }
}

fn run_repl() {
    println!("Schala v 0.02");
    let initial_state = InterpreterState {
        show_tokens: false,
        show_parse: false,
        evaluator: Evaluator::new(),
    };
    REPL::with_prompt_and_state(Box::new(repl_handler), ">> ", initial_state)
        .run();
}

struct InterpreterState {
    show_tokens: bool,
    show_parse: bool,
    evaluator: Evaluator,
}

impl ReplState for InterpreterState {
    fn update_state(&mut self, input: &Vec<&str>) {
        match input[..] {
            ["set", "show", "tokens", "true"] => {
                self.show_tokens = true;
            },
            ["set", "show", "tokens", "false"] => {
                self.show_tokens = false;
            },
            ["set", "show", "parse", "true"] => {
                self.show_parse = true;
            },
            ["set", "show", "parse", "false"] => {
                self.show_parse = false;
            },
            _ => ()
        }
    }
}

fn repl_handler(input: &str, state: &mut InterpreterState) -> String {
    let mut result = String::new();

    let tokens = match tokenize(input) {
        None => return format!("Tokenization error"),
        Some(t) => t
    };

    if state.show_tokens {
        result.push_str(&format!("Tokens: {:?}\n", tokens));
    }

    let ast = match parse(&tokens, &[]) {
        Ok(ast) => ast,
        Err(err) => return format!("Parse error: {}", err.msg),
    };

    if state.show_parse {
        result.push_str(&format!("AST: {:?}\n", ast));
    }

    let mut output: Vec<String> = state.evaluator.run(ast);

    //for now only handle last output
    let interpreter_result = output.pop().unwrap_or("".to_string());
    result.push_str(&interpreter_result);
    result
}
