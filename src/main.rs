#![feature(advanced_slice_patterns, slice_patterns, box_patterns)]
extern crate simplerepl;
extern crate getopts;

use std::path::Path;
use std::fs::File;
use std::io::Read;

use simplerepl::{REPL, ReplState};

use tokenizer::tokenize;
mod tokenizer;

use parser::parse;
mod parser;

use eval::Evaluator;
mod eval;

use compilation::compilation_sequence;
mod compilation;
mod llvm_wrap;

fn main() {
    let option_matches =
        program_options().parse(std::env::args()).expect("Could not parse options");
    match option_matches.free[..] {
        [] | [_] => {
            run_repl();
        }
        [_, ref filename, _..] => {
            run_noninteractive(filename, !option_matches.opt_present("i"));
        }
    };
}

fn program_options() -> getopts::Options {
    let mut options = getopts::Options::new();
    options.optflag("i",
                    "interpret",
                    "Interpret source file instead of compiling");
    options
}

fn run_noninteractive(filename: &str, compile: bool) {
    let mut source_file = File::open(&Path::new(filename)).unwrap();
    let mut buffer = String::new();
    source_file.read_to_string(&mut buffer).unwrap();

    let tokens = match tokenize(&buffer) {
        Ok(t) => t,
        Err(e) => {
            println!("Tokenization error: {}", e.msg);
            std::process::exit(1)
        }
    };

    let ast = match parse(&tokens, &[]) {
        Ok(ast) => ast,
        Err(err) => {
            println!("Parse error: {:?}", err.msg);
            println!("Remaining tokens: {:?}", err.remaining_tokens);
            std::process::exit(1)
        }
    };

    if compile {
        compilation_sequence(ast, filename);
    } else {
        let mut evaluator = Evaluator::new(None);
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
        evaluator: Evaluator::new(None),
    };
    REPL::with_prompt_and_state(Box::new(repl_handler), ">> ", initial_state).run();
}

struct InterpreterState<'a> {
    show_tokens: bool,
    show_parse: bool,
    evaluator: Evaluator<'a>,
}

impl<'a> ReplState for InterpreterState<'a> {
    fn update_state(&mut self, input: &Vec<&str>) {
        match input[..] {
            ["set", "show", "tokens", "true"] => {
                self.show_tokens = true;
            }
            ["set", "show", "tokens", "false"] => {
                self.show_tokens = false;
            }
            ["set", "show", "parse", "true"] => {
                self.show_parse = true;
            }
            ["set", "show", "parse", "false"] => {
                self.show_parse = false;
            }
            _ => (),
        }
    }
}

fn repl_handler(input: &str, state: &mut InterpreterState) -> String {
    let mut result = String::new();

    let tokens = match tokenize(input) {
        Err(e) => return format!("Tokenization error: {}", e.msg),
        Ok(t) => t,
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

    // for now only handle last output
    let interpreter_result = output.pop().unwrap_or("".to_string());
    result.push_str(&interpreter_result);
    result
}
