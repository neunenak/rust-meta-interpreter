#![feature(advanced_slice_patterns, slice_patterns)]

extern crate simplerepl;

use std::path::Path;
use std::fs::File;
use std::io::Read;

use simplerepl::{REPL, ReplState};

use tokenizer::tokenize;
mod tokenizer;

use parser::{ParseResult, parse};
mod parser;

use evaluator::{evaluate};
mod evaluator;


fn main() {
    let args: Vec<String> = std::env::args().collect();
    println!("Schala v 0.02");
    if let Some(filename) = args.get(1) {
        let mut source_file = File::open(&Path::new(filename)).unwrap();
        let mut buffer = String::new();
        source_file.read_to_string(&mut buffer).unwrap();
        panic!("Not implemented yet");
    } else {
        let initial_state = InterpreterState { show_tokens: false, show_parse: false };
        REPL::with_prompt_and_state(Box::new(repl_handler), ">> ", initial_state)
              .run();
    }
}

struct InterpreterState {
    show_tokens: bool,
    show_parse: bool,
}

impl ReplState for InterpreterState {
    fn update_state(&mut self, input: &Vec<&str>) {
        match &input[..] {
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
    if state.show_tokens {
        println!("Tokens: {:?}", tokenize(input))
    }

    if state.show_parse {
        println!("Parse: {:?}", parse(tokenize(input)))
    }

    let parse_result = parse(tokenize(input));
    match parse_result {
        Ok(ast) => {
            format!("{}", evaluate(ast))
        },
        Err(err) => {
            format!("Parse error: {:?}", err)
        }
    }
}
