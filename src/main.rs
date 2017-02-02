#![feature(advanced_slice_patterns, slice_patterns, box_patterns)]
extern crate getopts;
extern crate linefeed;

use std::path::Path;
use std::fs::File;
use std::io::Read;
use std::process;
use std::io::Write;
use std::default::Default;

mod schala_lang;
use schala_lang::SchalaEvaluator;
use schala_lang::Schala;

mod maaru_lang;

mod language;
use language::{ProgrammingLanguage, LanguageInterface, LLVMCodeString, EvaluationMachine};

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
    let show_llvm = option_matches.opt_present("l");
    match option_matches.free[..] {
        [] | [_] => {
            let mut repl = Repl::new(trace, show_llvm);
            repl.run();
        }
        [_, ref filename, _..] => {
            let language = Schala::new();
            run_noninteractive(filename, !option_matches.opt_present("i"), trace, &language);
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
    options.optflag("l",
                    "llvm-in-repl",
                    "Show LLVM IR in REPL");
    options
}

fn run_noninteractive<'a, T: ProgrammingLanguage>(filename: &str, compile: bool, trace_evaluation: bool, language: &T) {
    let mut source_file = File::open(&Path::new(filename)).unwrap();
    let mut buffer = String::new();
    source_file.read_to_string(&mut buffer).unwrap();

    let tokens = match T::tokenize(&buffer) {
        Ok(t) => t,
        Err(e) => {
            println!("Tokenization error: {}", e.msg);
            std::process::exit(1)
        }
    };

    let ast = match T::parse(tokens) {
        Ok(ast) => ast,
        Err(err) => {
            println!("Parse error: {:?}", err.msg);
            /*println!("Remaining tokens: {:?}", err.remaining_tokens);*/
            std::process::exit(1)
        }
    };

    if compile {
        compilation_sequence(T::compile(ast), filename);
    } else {
        let mut evaluator = <T as ProgrammingLanguage>::Evaluator::new();
        if trace_evaluation {
            evaluator.set_option("trace_evaluation", true);
        }
        let results = T::evaluate(ast, &mut evaluator);
        for result in results.iter() {
            println!("{}", result);
        }
    }
}

type LineReader = linefeed::Reader<linefeed::terminal::DefaultTerminal>;
struct Repl {
    show_tokens: bool,
    show_parse: bool,
    show_llvm_ir: bool,
    languages: Vec<Box<LanguageInterface>>,
    interpreter_directive_sigil: char,
    reader: LineReader,
}

impl Repl {
    fn new(trace_evaluation: bool, show_llvm: bool) -> Repl {
        let mut reader: linefeed::Reader<_> = linefeed::Reader::new("Schala").unwrap();
        reader.set_prompt(">> ");

        Repl {
            show_tokens: false,
            show_parse: false,
            show_llvm_ir: show_llvm,
            languages: vec![Box::new((Schala::new(), SchalaEvaluator::new(None)))],
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
        let ref mut language = self.languages[0];
        language.evaluate_in_repl(input, language::LanguageInterfaceOptions::default())
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
                    "eval" => { /*self.evaluator.set_option("trace_evaluation", show);*/ },
                    "llvm" => self.show_llvm_ir = show,
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

pub fn compilation_sequence(llvm_code: LLVMCodeString, sourcefile: &str) {
    use std::process::Command;

    let ll_filename = "out.ll";
    let obj_filename = "out.o";
    let q: Vec<&str> = sourcefile.split('.').collect();
    let bin_filename = match &q[..] {
        &[name, "schala"] => name,
        _ => panic!("Bad filename {}", sourcefile),
    };

    let LLVMCodeString(llvm_str) = llvm_code;

    println!("Compilation process finished for {}", ll_filename);
    File::create(ll_filename)
        .and_then(|mut f| f.write_all(llvm_str.as_bytes()))
        .expect("Error writing file");

    let llc_output = Command::new("llc")
        .args(&["-filetype=obj", ll_filename, "-o", obj_filename])
        .output()
        .expect("Failed to run llc");


    if !llc_output.status.success() {
        println!("{}", String::from_utf8_lossy(&llc_output.stderr));
    }

    let gcc_output = Command::new("gcc")
        .args(&["-o", bin_filename, &obj_filename])
        .output()
        .expect("failed to run gcc");

    if !gcc_output.status.success() {
        println!("{}", String::from_utf8_lossy(&gcc_output.stdout));
        println!("{}", String::from_utf8_lossy(&gcc_output.stderr));
    }

    for filename in [obj_filename].iter() {
        Command::new("rm")
            .arg(filename)
            .output()
            .expect(&format!("failed to run rm {}", filename));
    }
}

