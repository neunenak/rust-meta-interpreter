#![feature(advanced_slice_patterns, slice_patterns, box_patterns, box_syntax)]
#![feature(plugin)]
#![plugin(rocket_codegen)]
extern crate getopts;
extern crate linefeed;
extern crate itertools;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate maplit;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;
extern crate rocket;
extern crate rocket_contrib;
extern crate includedir;
extern crate phf;

use std::path::Path;
use std::fs::File;
use std::io::{Read, Write};
use std::process::exit;
use std::default::Default;

mod schala_lang;
mod maaru_lang;
mod robo_lang;

mod language;
use language::{ProgrammingLanguageInterface, EvalOptions, LLVMCodeString};

mod webapp;
mod llvm_wrap;

include!(concat!(env!("OUT_DIR"), "/static.rs"));

fn main() {
  let languages: Vec<Box<ProgrammingLanguageInterface>> =
    vec![
      Box::new(schala_lang::Schala::new()),
      Box::new(maaru_lang::Maaru::new()),
      Box::new(robo_lang::Robo::new()),
    ];

  let option_matches = program_options().parse(std::env::args()).unwrap_or_else(|e| {
    println!("{:?}", e);
    exit(1);
  });

  if option_matches.opt_present("list-languages") {
    for lang in languages {
      println!("{}", lang.get_language_name());
    }
    exit(1);
  }

  if option_matches.opt_present("help") {
    println!("{}", program_options().usage("Schala metainterpreter"));
    exit(0);
  }

  if option_matches.opt_present("webapp") {
    webapp::web_main();
    exit(0);
  }

  let language_names: Vec<String> = languages.iter().map(|lang| {lang.get_language_name()}).collect();
  let initial_index: usize =
    option_matches.opt_str("lang")
    .and_then(|lang| { language_names.iter().position(|x| { *x == lang }) })
    .unwrap_or(0);

  let mut options = EvalOptions::default();
  options.compile = match option_matches.opt_str("eval-style") {
    Some(ref s) if s == "compile" => true,
    _ => false
  };

  match option_matches.free[..] {
    [] | [_] => {
      let mut repl = Repl::new(languages, initial_index);
      repl.options.show_llvm_ir = true; //TODO make this be configurable
      repl.run();
    }
    [_, ref filename, _..] => {

      run_noninteractive(filename, languages, options);
    }
  };
}

fn run_noninteractive(filename: &str, languages: Vec<Box<ProgrammingLanguageInterface>>, options: EvalOptions) {
  let path = Path::new(filename);
  let ext = path.extension().and_then(|e| e.to_str()).unwrap_or_else(|| {
    println!("Source file lacks extension");
    exit(1);
  });
  let mut language = Box::new(languages.into_iter().find(|lang| lang.get_source_file_suffix() == ext)
    .unwrap_or_else(|| {
      println!("Extension .{} not recognized", ext);
      exit(1);
    }));

  let mut source_file = File::open(path).unwrap();
  let mut buffer = String::new();

  source_file.read_to_string(&mut buffer).unwrap();

  if options.compile {
    if !language.can_compile() {
      panic!("Trying to compile a non-compileable language");
    } else {
      let llvm_bytecode = language.compile(&buffer);
      compilation_sequence(llvm_bytecode, filename);
    }
  } else {
    let interpretor_output = language.evaluate_in_repl(&buffer, &options);
    interpretor_output.print_to_screen();
  }
}

type LineReader = linefeed::Reader<linefeed::terminal::DefaultTerminal>;
struct Repl {
  options: EvalOptions,
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
      options: Repl::get_options(),
      languages: languages,
      current_language_index: i,
      interpreter_directive_sigil: '.',
      reader: reader,
    }
  }

  fn get_options() -> EvalOptions {
    File::open(".schala_repl")
      .and_then(|mut file| {
        let mut contents = String::new();
        file.read_to_string(&mut contents)?;
        Ok(contents)
      })
      .and_then(|contents| {
        let options: EvalOptions = serde_json::from_str(&contents)?;
        Ok(options)
      }).unwrap_or(EvalOptions::default())
  }

  fn save_options(&self) {
    let ref options = self.options;
    let read = File::create(".schala_repl")
      .and_then(|mut file| {
        let buf = serde_json::to_string(options).unwrap();
        file.write_all(buf.as_bytes())
      });

    if let Err(err) = read {
      println!("Error saving .schala_repl file {}", err);
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
    let interpretor_output = language.evaluate_in_repl(input, &self.options);
    interpretor_output.to_string()
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
      "exit" | "quit"  => {
        self.save_options();
        exit(0)
      },
      "history"  => {
        for item in self.reader.history() {
          println!("{}", item);
        }
      },
      "help" => {
        println!("Commands:");
        println!("exit | quit");
        println!("lang [show|next|previous]");
        println!("set [show|hide] [tokens|parse|symbols|eval|llvm]");
      }
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
          Some(&"tokens") => self.options.debug_tokens = show,
          Some(&"parse") => self.options.debug_parse = show,
          Some(&"symbols") => self.options.debug_symbol_table = show,
          Some(&"eval") => {
            //let ref mut language = self.languages[self.current_language_index];
            //language.set_option("trace_evaluation", show);
          },
          Some(&"llvm") => self.options.show_llvm_ir = show,
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

fn program_options() -> getopts::Options {
  let mut options = getopts::Options::new();
  options.optopt("s",
                 "eval-style",
                 "Specify whether to compile (if supported) or interpret the language. If not specified, the default is language-specific",
                 "[compile|interpret]"
                 );
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
  options.optflag("w",
                  "webapp",
                  "Start up web interpreter");
  options
}

