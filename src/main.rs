#![feature(advanced_slice_patterns, slice_patterns, box_patterns)]
extern crate getopts;
extern crate linefeed;

use std::path::Path;
use std::fs::File;
use std::io::Read;
use std::process;
use std::io::Write;
use std::default::Default;

/*
mod schala_lang;
use schala_lang::SchalaEvaluator;
use schala_lang::Schala;
*/

mod schala_lang;
mod maaru_lang;
mod robo_lang;

mod language;
use language::{ProgrammingLanguage, LanguageInterface, ProgrammingLanguageInterface, EvalOptions, LLVMCodeString, EvaluationMachine};

mod llvm_wrap;

mod virtual_machine;
use virtual_machine::{run_vm, run_assembler};

fn main() {
    let languages: Vec<Box<ProgrammingLanguageInterface>> =
        vec![
            Box::new(maaru_lang::Maaru::new()),
            Box::new(robo_lang::Robo::new()),
            /*
            Box::new((schala_lang::Schala::new(), schala_lang::SchalaEvaluator::new())),
            Box::new((maaru_lang::Maaru::new(), maaru_lang::MaaruEvaluator::new(None))),
            */
        ];

    let option_matches =
        match program_options().parse(std::env::args()) {
            Ok(o) => o,
            Err(e) => {
                println!("{:?}", e);
                std::process::exit(1);
            }
        };
    if option_matches.opt_present("list-languages") {
        for lang in languages {
            println!("{}", lang.get_language_name());
        }
        std::process::exit(1);
    }

    if option_matches.opt_present("h") {
        println!("{}", program_options().usage("Schala metainterpreter"));
        std::process::exit(0);
    }

    if option_matches.opt_present("m") {
        let file_name = option_matches.free.get(1);
        run_vm(file_name);
        std::process::exit(0);
    }

    if option_matches.opt_present("a") {
        let file_name = option_matches.free.get(1);
        run_assembler(file_name);
        std::process::exit(0);
    }

    let language_names: Vec<String> = languages.iter().map(|lang| {lang.get_language_name()}).collect();
    let initial_index: usize =
        option_matches.opt_str("l")
            .and_then(|lang| { language_names.iter().position(|x| { *x == lang }) })
            .unwrap_or(0);

    let show_llvm_ir = option_matches.opt_present("v");
    let compile = !option_matches.opt_present("i");
    let trace_evaluation = option_matches.opt_present("t");

    match option_matches.free[..] {
        [] | [_] => {
            let mut repl = Repl::new(languages, initial_index);
            repl.show_llvm_ir = show_llvm_ir;
            repl.run();
        }
        [_, ref filename, _..] => {
            let mut language = maaru_lang::Maaru::new();
            run_noninteractive(filename, &mut language, trace_evaluation, compile);
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
    options.optflag("v",
                    "llvm-in-repl",
                    "Show LLVM IR in REPL");
    options.optflag("",
                    "list-languages",
                    "Show a list of all supported languages");
    options.optopt("l",
                    "lang",
                    "Start up REPL in a language",
                    "LANGUAGE");
    options.optflag("h",
                    "help",
                    "Show help text");
    options.optflag("m",
                   "virtual-machine",
                   "Start up a virtual machine instead of an interpreter");
    options.optflag("a",
                    "assembler",
                    "Assemble file into bytecode");
    options
}

fn run_noninteractive<T: ProgrammingLanguageInterface>(filename: &str, language: &mut T, trace_evaluation: bool, compile: bool) {
  let mut source_file = File::open(&Path::new(filename)).unwrap();
  let mut buffer = String::new();
  source_file.read_to_string(&mut buffer).unwrap();

  let options = EvalOptions::default();
  let interpretor_output = language.evaluate_in_repl(&buffer, options);
  for line in interpretor_output {
    println!("{}", line);
  }
      /*
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
        if let Some(r) = results.iter().last() {
            println!("{}", r);
        }
    }
    */
}

type LineReader = linefeed::Reader<linefeed::terminal::DefaultTerminal>;
struct Repl {
    pub show_tokens: bool,
    pub show_parse: bool,
    pub show_llvm_ir: bool,
    languages: Vec<Box<ProgrammingLanguageInterface>>,
    current_language_index: usize,
    interpreter_directive_sigil: char,
    reader: LineReader,
}

impl Repl {
    fn new(languages: Vec<Box<ProgrammingLanguageInterface>>, initial_index: usize) -> Repl {
        let mut reader: linefeed::Reader<_> = linefeed::Reader::new("Metainterpreter").unwrap();
        reader.set_prompt(">> ");
        let i = if initial_index < languages.len() { initial_index } else { 0 };
        Repl {
            show_tokens: false,
            show_parse: false,
            show_llvm_ir: false,
            languages: languages,
            current_language_index: i,
            interpreter_directive_sigil: '.',
            reader: reader,
        }
    }
    fn run(&mut self) {
        use linefeed::ReadResult::*;
        println!("MetaInterpreter v 0.05");
        println!("Using language: {}", self.languages[self.current_language_index].get_language_name());
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
      let ref mut language = self.languages[self.current_language_index];

      let mut options = language::EvalOptions::default();
      /*
         options.show_tokens = self.show_tokens;
         options.show_parse = self.show_parse;
         options.show_llvm_ir = self.show_llvm_ir;
         */


      let interpretor_output = language.evaluate_in_repl(input, options);
      let mut acc = String::new();
      for i in interpretor_output {
        acc.push_str(&i)
      }
      acc
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
            "lang" => {
                match commands.get(1) {
                    Some(&"show") => {
                        for (i, lang) in self.languages.iter().enumerate() {
                            if i == self.current_language_index {
                                println!("* {}", lang.get_language_name());
                            } else {
                                println!("{}", lang.get_language_name());
                            }
                        }
                    },
                    Some(&"next") => {
                        self.current_language_index = (self.current_language_index + 1) % self.languages.len();
                        println!("Switching to {}", self.languages[self.current_language_index].get_language_name());
                    }
                    Some(&"prev") | Some(&"previous") => {
                        self.current_language_index = if self.current_language_index == 0 { self.languages.len() - 1 } else { self.current_language_index - 1 };
                        println!("Switching to {}", self.languages[self.current_language_index].get_language_name());
                    },
                    Some(e) => println!("Bad `lang` argument: {}", e),
                    None => println!("`lang` - valid arguments `show`, `next`, `prev`|`previous`"),
                }
            },
            "set" => {
                let show = match commands.get(1) {
                    Some(&"show") => true,
                    Some(&"hide") => false,
                    Some(e) => {
                        println!("Bad `set` argument: {}", e);
                        return true;
                    }
                    None => {
                        println!("`set` - valid arguments `show {{option}}`, `hide {{option}}`");
                        return true;
                    }
                };
                match commands.get(2) {
                    Some(&"tokens") => self.show_tokens = show,
                    Some(&"parse") => self.show_parse = show,
                    Some(&"eval") => {
                        let ref mut language = self.languages[self.current_language_index];
                        language.set_option("trace_evaluation", show);
                    },
                    Some(&"llvm") => self.show_llvm_ir = show,
                    Some(e) => {
                        println!("Bad `show`/`hide` argument: {}", e);
                        return true;
                    }
                    None => {
                        println!("`show`/`hide` requires an argument");
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
        &[name, "maaru"] => name,
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

