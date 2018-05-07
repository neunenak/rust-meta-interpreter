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

use colored::*;
use itertools::Itertools;
use rustyline::error::ReadlineError;
use rustyline::Editor;

mod language;
mod webapp;
pub mod llvm_wrap;

const VERSION_STRING: &'static str = "0.1.0";

include!(concat!(env!("OUT_DIR"), "/static.rs"));

pub use language::{LLVMCodeString, ProgrammingLanguageInterface, EvalOptions, ExecutionMethod, TraceArtifact, FinishedComputation, UnfinishedComputation};
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
  if let Some(_) = option_matches.opt_str("debug") {
    /* TODO - put some debug handling code here */
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

struct TabCompleteHandler { 
  passes: Vec<String>,
  sigil: char,
}

impl TabCompleteHandler {
  fn complete_interpreter_directive(&self, line: &str, pos: usize) -> rustyline::Result<(usize, Vec<String>)> {
    let mut iter = line.chars();
    iter.next();
    let commands: Vec<&str> = iter
      .as_str()
      .split_whitespace()
      .collect();
    println!("POS {}---", pos);
    let completes = match &commands[..] {
      &["debug", "show"] | &["debug", "hide"] => self.passes.clone(),
      &["debug"] | &["debug", _] => vec!["passes".to_string(), "show".to_string(), "hide".to_string()],
      &[_cmd] => vec!["debug".to_string()],
      _ => vec![],
    };
    Ok((pos, completes))
  }
}

impl rustyline::completion::Completer for TabCompleteHandler {
  fn complete(&self, line: &str, pos: usize) -> rustyline::Result<(usize, Vec<String>)> {
    if line.starts_with(&format!("{}", self.sigil)) {
      self.complete_interpreter_directive(line, pos)
    } else {
      Ok((pos, vec!(format!("tab-completion-no-done"), format!("tab-completion-still-not-done"))))
    }
  }
}

struct Repl {
  options: EvalOptions,
  languages: Vec<Box<ProgrammingLanguageInterface>>,
  current_language_index: usize,
  interpreter_directive_sigil: char,
  console: rustyline::Editor<TabCompleteHandler>,
}

impl Repl {
  fn new(languages: Vec<Box<ProgrammingLanguageInterface>>, initial_index: usize) -> Repl {
    let i = if initial_index < languages.len() { initial_index } else { 0 };

    let console = Editor::<TabCompleteHandler>::new();

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
      let tab_complete_handler = TabCompleteHandler { sigil: self.interpreter_directive_sigil, passes: self.get_cur_language().get_passes() };
      self.console.set_completer(Some(tab_complete_handler));
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
        writeln!(buf, "debug [show|hide] <pass_name> - show or hide debug info for a given pass").unwrap();
        writeln!(buf, "debug passes - display the names of all passes").unwrap();
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
    let passes = self.get_cur_language().get_passes();
    match commands.get(1) {
      Some(&"passes") => Some(
        passes.into_iter()
        .map(|p| {
          if self.options.debug_passes.contains(&p) {
            let color = "green";
            format!("*{}", p.color(color))
          } else {
            p
          }
        })
        .intersperse(format!(" -> "))
        .collect()),
      b @ Some(&"show") | b @ Some(&"hide") => {
        let show = b == Some(&"show");
        let debug_pass: String = match commands.get(2) {
          Some(s) => s.to_string(),
          None => return Some(format!("Must specify a stage to debug")),
        };
        if let Some(stage) = passes.iter().find(|stage_name| **stage_name == debug_pass) {
          let msg = format!("{} debug for stage {}", if show { "Enabling" } else { "Disabling" }, debug_pass);
          if show {
            self.options.debug_passes.insert(stage.clone());
          } else {
            self.options.debug_passes.remove(stage);
          }
          Some(msg)
        } else {
          Some(format!("Couldn't find stage: {}", debug_pass))
        }
      },
      _ => Some(format!("Unknown debug command"))
    }
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
