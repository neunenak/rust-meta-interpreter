#![feature(link_args)]
#![feature(slice_patterns, box_patterns, box_syntax)]
#![feature(plugin)]
#![plugin(rocket_codegen)]
extern crate getopts;
extern crate rustyline;
extern crate itertools;
extern crate colored;

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
use std::fmt::Write as FmtWrite;

use itertools::Itertools;
use rustyline::error::ReadlineError;
use rustyline::Editor;

mod language;
mod webapp;
pub mod llvm_wrap;

const VERSION_STRING: &'static str = "0.1.0";

include!(concat!(env!("OUT_DIR"), "/static.rs"));

pub use language::{LLVMCodeString, ProgrammingLanguageInterface, EvalOptions, ExecutionMethod, TraceArtifact, LanguageOutput, FinishedComputation, UnfinishedComputation};
pub type PLIGenerator = Box<Fn() -> Box<ProgrammingLanguageInterface> + Send + Sync>;

pub fn repl_main(generators: Vec<PLIGenerator>) {
  let languages: Vec<Box<ProgrammingLanguageInterface>> = generators.iter().map(|x| x()).collect();

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
    webapp::web_main(generators);
    exit(0);
  }

  let mut options = EvalOptions::default();
  if let Some(ref ltrs) = option_matches.opt_str("debug") {
    options.debug.tokens = ltrs.contains("l");
    options.debug.ast = ltrs.contains("a");
    options.debug.parse_tree = ltrs.contains("r");
    options.debug.symbol_table = ltrs.contains("s");
  }

  let language_names: Vec<String> = languages.iter().map(|lang| {lang.get_language_name()}).collect();
  let initial_index: usize =
    option_matches.opt_str("lang")
    .and_then(|lang| { language_names.iter().position(|x| { x.to_lowercase() == lang.to_lowercase() }) })
    .unwrap_or(0);

  options.execution_method = match option_matches.opt_str("eval-style") {
    Some(ref s) if s == "compile" => ExecutionMethod::Compile,
    _ => ExecutionMethod::Interpret,
  };

  match option_matches.free[..] {
    [] | [_] => {
      let mut repl = Repl::new(languages, initial_index);
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

  match options.execution_method {
    ExecutionMethod::Compile => {
      /*
      let llvm_bytecode = language.compile(&buffer);
      compilation_sequence(llvm_bytecode, filename);
      */
      panic!("Not ready to go yet");
    },
    ExecutionMethod::Interpret => {
      let output = language.execute_pipeline(&buffer, &options);
      output.to_noninteractive().map(|text| println!("{}", text));
    }
  }
}

struct Repl {
  options: EvalOptions,
  languages: Vec<Box<ProgrammingLanguageInterface>>,
  current_language_index: usize,
  interpreter_directive_sigil: char,
  console: rustyline::Editor<()>,
}

impl Repl {
  fn new(languages: Vec<Box<ProgrammingLanguageInterface>>, initial_index: usize) -> Repl {
    let i = if initial_index < languages.len() { initial_index } else { 0 };

    let console = Editor::<()>::new();

    Repl {
      options: Repl::get_options(),
      languages: languages,
      current_language_index: i,
      interpreter_directive_sigil: ':',
      console
    }
  }

  fn get_cur_language(&self) -> &ProgrammingLanguageInterface {
    self.languages[self.current_language_index].as_ref()
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
    println!("Schala MetaInterpreter version {}", VERSION_STRING);
    println!("Type {}help for help with the REPL", self.interpreter_directive_sigil);

    self.console.get_history().load(".schala_history").unwrap_or(());

    loop {
      let language_name = self.languages[self.current_language_index].get_language_name();
      let prompt_str = format!("{} >> ", language_name);

      match self.console.readline(&prompt_str) {
        Err(ReadlineError::Eof) | Err(ReadlineError::Interrupted) => break,
        Err(e) => {
          println!("Terminal read error: {}", e);
        },
        Ok(ref input) => {
          let output = match input.chars().nth(0) {
            Some(ch) if ch == self.interpreter_directive_sigil => self.handle_interpreter_directive(input),
            _ => {
              self.console.get_history().add(input);
              Some(self.input_handler(input))
            }
          };
          if let Some(o) = output {
            println!("=> {}", o);
          }
        }
      }
    }
    self.console.get_history().save(".schala_history").unwrap_or(());
    self.save_options();
    println!("Exiting...");
  }

  fn input_handler(&mut self, input: &str) -> String {
    let ref mut language = self.languages[self.current_language_index];
    let interpreter_output = language.execute_pipeline(input, &self.options);
    interpreter_output.to_repl()
  }

  fn handle_interpreter_directive(&mut self, input: &str) -> Option<String> {
    let mut iter = input.chars();
    iter.next();
    let commands: Vec<&str> = iter
      .as_str()
      .split_whitespace()
      .collect();

    let cmd: &str = match commands.get(0).clone() {
      None => return None,
      Some(s) => s
    };

    match cmd {
      "exit" | "quit"  => {
        self.save_options();
        exit(0)
      },
      "lang" | "language" => match commands.get(1) {
        Some(&"show") => {
          let mut buf = String::new();
          for (i, lang) in self.languages.iter().enumerate() {
            write!(buf, "{}{}\n", if i == self.current_language_index { "* "} else { "" }, lang.get_language_name()).unwrap();
          }
          Some(buf)
        },
        Some(&"go") => match commands.get(2) {
          None => Some(format!("Must specify a language name")),
          Some(&desired_name) => {
            for (i, _) in self.languages.iter().enumerate() {
              let lang_name = self.languages[i].get_language_name();
              if lang_name.to_lowercase() == desired_name.to_lowercase() {
                self.current_language_index = i;
                return Some(format!("Switching to {}", self.languages[self.current_language_index].get_language_name()));
              }
            }
            Some(format!("Language {} not found", desired_name))
          }
        },
        Some(&"next") | Some(&"n") => {
          self.current_language_index = (self.current_language_index + 1) % self.languages.len();
          Some(format!("Switching to {}", self.languages[self.current_language_index].get_language_name()))
        },
        Some(&"previous") | Some(&"p") | Some(&"prev") => { 
          self.current_language_index = if self.current_language_index == 0 { self.languages.len() - 1 } else { self.current_language_index - 1 };
          Some(format!("Switching to {}", self.languages[self.current_language_index].get_language_name()))
        },
        Some(e) => Some(format!("Bad `lang(uage)` argument: {}", e)),
        None => Some(format!("Valid arguments for `lang(uage)` are `show`, `next`|`n`, `previous`|`prev`|`n`"))
      },
      "help" =>  {
        let mut buf = String::new();
        let ref lang = self.languages[self.current_language_index];

        writeln!(buf, "MetaInterpreter options").unwrap();
        writeln!(buf, "-----------------------").unwrap();
        writeln!(buf, "exit | quit - exit the REPL").unwrap();
        writeln!(buf, "lang [prev|next|go <name> |show] - toggle to previous or next language, go to a specific language by name, or show all languages").unwrap();
        writeln!(buf, "Language-specific help for {}", lang.get_language_name()).unwrap();
        writeln!(buf, "-----------------------").unwrap();
        writeln!(buf, "{}", lang.custom_interpreter_directives_help()).unwrap();
        Some(buf)
      },
      "debug" => self.handle_debug(commands),
      e => self.languages[self.current_language_index]
        .handle_custom_interpreter_directives(&commands)
        .or(Some(format!("Unknown command: {}", e)))
    }
  }
  fn handle_debug(&mut self, commands: Vec<&str>) -> Option<String> {
    let stages = self.get_cur_language().get_stages();
    match commands.get(1) {
      Some(&"stages") => Some(stages.into_iter().intersperse(format!(" -> ")).collect()),
      b @ Some(&"show") | b @ Some(&"hide") => {
        let show = b == Some(&"show");
        let debug_stage: String = match commands.get(2) {
          Some(s) => s.to_string(),
          None => return Some(format!("Must specify a stage to debug")),
        };
        if let Some(stage) = stages.iter().find(|stage_name| **stage_name == debug_stage) {
          let msg = format!("{} debug for stage {}", if show { "Enabling" } else { "Disabling" }, debug_stage);
          if show {
            self.options.debug_stages.insert(stage.clone());
          } else {
            self.options.debug_stages.remove(stage);
          }
          Some(msg)
        } else {
          Some(format!("Couldn't find stage: {}", debug_stage))
        }
      },
      _ => Some(format!("Unknown debug command"))
    }
      /*
       * {
        let show = match commands.get(1) {
          Some(&"show") => true,
          Some(&"hide") => false,
          Some(e) => {
            return Some(format!("Bad `set` argument: {}", e));
          }
          None => {
            return Some(format!("`set` - valid arguments `show {{option}}`, `hide {{option}}`"));
          }
        };
        match commands.get(2) {
          Some(&"tokens") => self.options.debug.tokens = show,
          Some(&"parse") => self.options.debug.parse_tree = show,
          Some(&"ast") => self.options.debug.ast = show,
          Some(&"symbols") => self.options.debug.symbol_table = show,
          Some(&"llvm") => self.options.debug.llvm_ir = show,
          Some(e) => return Some(format!("Bad `show`/`hide` argument: {}", e)),
          None => return Some(format!("`show`/`hide` requires an argument")),
        };
        None
      },

      AND DEBUG OPTIONS

      "options" => {
        let ref d = self.options.debug;
        let tokens = if d.tokens { "true".green() } else { "false".red() };
        let parse_tree = if d.parse_tree { "true".green() } else { "false".red() };
        let ast = if d.ast { "true".green() } else { "false".red() };
        let symbol_table = if d.symbol_table { "true".green() } else { "false".red() };
        Some(format!(r#"Debug:
tokens: {}, parse: {}, ast: {}, symbols: {}"#, tokens, parse_tree, ast, symbol_table))
      },

      */
  }
}

/*
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
*/

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
  options.optopt("d",
                  "debug",
                  "Debug a stage (l = tokenizer, a = AST, r = parse trace, s = symbol table)",
                  "[l|a|r|s]");
  options
}
