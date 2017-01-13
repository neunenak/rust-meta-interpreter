#![feature(advanced_slice_patterns, slice_patterns, box_patterns)]
extern crate getopts;
extern crate linefeed;

use std::path::Path;
use std::fs::File;
use std::io::Read;
use std::process;

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
        match program_options().parse(std::env::args()) {
            Ok(o) => o,
            Err(e) => {
                println!("{:?}", e);
                std::process::exit(1);
            }
        };
    let trace = option_matches.opt_present("t");
    match option_matches.free[..] {
        [] | [_] => {
            let mut repl = Repl::new(trace);
            repl.run();
        }
        [_, ref filename, _..] => {
            run_noninteractive(filename, !option_matches.opt_present("i"), trace);
        }
    };
}

fn program_options() -> getopts::Options {
    let mut options = getopts::Options::new();
    options.optflag("i",
                    "interpret",
                    "Interpret source file instead of compiling");
    options.optflag("t",
                    "trace-evaluation",
                    "Print out trace of evaluation");
    options
}

fn run_noninteractive(filename: &str, compile: bool, trace_evaluation: bool) {
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
        let mut evaluator = Evaluator::new_with_opts(None, trace_evaluation);
        let results = evaluator.run(ast);
        for result in results.iter() {
            println!("{}", result);
        }
    }
}

struct Repl<'a> {
    show_tokens: bool,
    show_parse: bool,
    show_eval: bool,
    input_history: Vec<String>,
    output_history: Vec<String>,
    evaluator: Evaluator<'a>,
    interpreter_directive_sigil: char,
}

impl<'a> Repl<'a> {
    fn new(trace_evaluation: bool) -> Repl<'a> {
        Repl {
            show_tokens: false,
            show_parse: false,
            show_eval: false,
            input_history: vec![],
            output_history: vec![],
            evaluator: Evaluator::new_with_opts(None, trace_evaluation),
            interpreter_directive_sigil: '.',
        }
    }
    fn run(&mut self) {
        use linefeed::ReadResult::*;
        println!("Schala v 0.02");
        let mut reader = linefeed::Reader::new("oi").unwrap();
        reader.set_prompt(">> ");
        loop {
            match reader.read_line() {
                Err(e) => {
                    println!("Terminal read error: {:?}", e);
                    break;
                },
                Ok(Eof) => {
                    println!("Exiting...");
                    break;
                }
                Ok(Input(ref input)) => {
                    let output = self.input_handler(input);
                    println!("{}", output);
                }
                Ok(Signal(signal)) => {
                    println!("Received signal: {:?}", signal);
                },
            }
        }
    }

    fn input_handler(&mut self, input: &str) -> String {
        let mut result = String::new();

        let tokens = match tokenize(input) {
            Err(e) => return format!("Tokenization error: {}", e.msg),
            Ok(t) => t,
        };

        if self.show_tokens {
            result.push_str(&format!("Tokens: {:?}\n", tokens));
        }

        let ast = match parse(&tokens, &[]) {
            Ok(ast) => ast,
            Err(err) => return format!("Parse error: {}", err.msg),
        };

        if self.show_parse {
            result.push_str(&format!("AST: {:?}\n", ast));
        }

        let mut output: Vec<String> = self.evaluator.run(ast);

        // for now only handle last output
        let interpreter_result = output.pop().unwrap_or("".to_string());
        result.push_str(&interpreter_result);
        result
    }

    fn handle_interpreter_directive(&mut self, input: &str) -> bool {
        match input.chars().nth(0) {
            Some(ch) if ch == self.interpreter_directive_sigil => (),
            _ => return false
        }

        let trimmed_sigil: String = input.chars()
            .skip(1)
            .collect();
        let commands: Vec<&str> =  trimmed_sigil
            .split_whitespace()
            .collect();

        let cmd: &str = match commands.get(0).clone() { None => return true, Some(s) => s };

        match cmd {
            "exit" | "quit"  => process::exit(0),
            "history"  => {
                println!("history:");
                for cmd in self.input_history.iter() {
                    println!("{}", cmd);
                }
            },
            "output_history" => {
                println!("output history:");
                for cmd in self.output_history.iter() {
                    println!("{}", cmd);
                }
            },
            _ => {
                self.update_state(&commands);
            }
        }
        return true;
    }

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
            ["set", "show", "eval", "true"] => {
                self.evaluator.trace_evaluation = true;
            }
            ["set", "show", "eval", "false"] => {
                self.evaluator.trace_evaluation = false;
            }
            _ => (),
        }
    }
}

