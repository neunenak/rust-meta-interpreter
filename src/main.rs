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

type LineReader = linefeed::Reader<linefeed::terminal::DefaultTerminal>;
struct Repl<'a> {
    show_tokens: bool,
    show_parse: bool,
    evaluator: Evaluator<'a>,
    interpreter_directive_sigil: char,
    reader: LineReader,
}

impl<'a> Repl<'a> {
    fn new(trace_evaluation: bool) -> Repl<'a> {
        let mut reader: linefeed::Reader<_> = linefeed::Reader::new("Schala").unwrap();
        reader.set_prompt(">> ");
        Repl {
            show_tokens: false,
            show_parse: false,
            evaluator: Evaluator::new_with_opts(None, trace_evaluation),
            interpreter_directive_sigil: '.',
            reader: reader,
        }
    }
    fn run(&mut self) {
        use linefeed::ReadResult::*;
        println!("Schala v 0.02");
        loop {
            match self.reader.read_line() {
                Err(e) => {
                    println!("Terminal read error: {}", e);
                },
                Ok(Eof) => {
                    break;
                }
                Ok(Input(ref input)) => {
                    self.reader.add_history(input.clone());
                    if self.handle_interpreter_directive(input) {
                        continue;
                    }
                    let output = self.input_handler(input);
                    println!("=> {}", output);
                }
                _ => (),
            }
        }
        println!("Exiting...");
    }

    fn input_handler(&mut self, input: &str) -> String {
        let mut result = String::new();
        let intermediate: Result<String, String> =
            tokenize(input)
            .map_err(|e| format!("Tokenization error: {}", e.msg))
            .and_then(
                |tokens| {
                    if self.show_tokens {
                        result.push_str(&format!("Tokens: {:?}\n", tokens));
                    }
                    parse(&tokens, &[]).map_err(|e| format!("Parse error: {}", e.msg))
                })
        .and_then(
            |ast| {
                if self.show_parse {
                    result.push_str(&format!("AST: {:?}\n", ast));
                }
                // for now only handle last output
                let mut full_output: Vec<String> = self.evaluator.run(ast);
                Ok(full_output.pop().unwrap_or("".to_string()))
            });
        match intermediate {
            Ok(s) | Err(s) => result.push_str(&s),
        };
        result
    }

    fn handle_interpreter_directive(&mut self, input: &str) -> bool {
        match input.chars().nth(0) {
            Some(ch) if ch == self.interpreter_directive_sigil => (),
            _ => return false
        }

        let mut iter = input.chars();
        iter.next();
        let trimmed_sigil: &str = iter.as_str();

        let commands: Vec<&str> = trimmed_sigil
            .split_whitespace()
            .collect();

        let cmd: &str = match commands.get(0).clone() {
            None => return true,
            Some(s) => s
        };

        match cmd {
            "exit" | "quit"  => process::exit(0),
            "history"  => {
                for item in self.reader.history() {
                    println!("{}", item);
                }
            },
            "set" => {
                let show = match commands[1] {
                    "show" => true,
                    "hide" => false,
                    e => {
                        println!("Bad `set` argument: {}", e);
                        return true;
                    }
                };
                match commands[2] {
                    "tokens" => self.show_tokens = show,
                    "parse" => self.show_parse = show,
                    "eval" => self.evaluator.trace_evaluation = show,
                    e => {
                        println!("Bad `show`/`hide` argument: {}", e);
                        return true;
                    }
                }
            },
            e => println!("Unknown command: {}", e)
        }
        return true;
    }
}
