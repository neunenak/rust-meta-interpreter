#![feature(link_args)]
#![feature(slice_patterns, box_patterns, box_syntax)]
#![feature(plugin)]
#![plugin(rocket_codegen)]
extern crate getopts;
extern crate linefeed;
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

mod language;
mod webapp;
pub mod llvm_wrap;

const VERSION_STRING: &'static str = "0.1.0";

include!(concat!(env!("OUT_DIR"), "/static.rs"));

pub use language::{LLVMCodeString, ProgrammingLanguageInterface, EvalOptions,
  ExecutionMethod, TraceArtifact, FinishedComputation, UnfinishedComputation, PassDebugDescriptor, PassDescriptor};

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
  let debug_passes = if let Some(opts) = option_matches.opt_str("debug") {
    let output: Vec<String> = opts.split_terminator(",").map(|s| s.to_string()).collect();
    output
  } else {
    vec![]
  };

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

      run_noninteractive(filename, languages, options, debug_passes);
    }
  };
}

fn run_noninteractive(filename: &str, languages: Vec<Box<ProgrammingLanguageInterface>>, mut options: EvalOptions, debug_passes: Vec<String>) {
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

  for pass in debug_passes.into_iter() {
    if let Some(_) = language.get_passes().iter().find(|desc| desc.name == pass) {
      options.debug_passes.insert(pass, PassDebugDescriptor { opts: vec![] });
    }
  }

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

enum CommandTree {
  Terminal(String, Option<String>),
  NonTerminal(String, Vec<CommandTree>, Option<String>),
  Top(Vec<CommandTree>),
}

impl CommandTree {
  fn term(s: &str, help: Option<&str>) -> CommandTree {
    CommandTree::Terminal(s.to_string(), help.map(|x| x.to_string()))
  }
  fn get_cmd(&self) -> String {
    match self {
      CommandTree::Terminal(s, _) => s.to_string(),
      CommandTree::NonTerminal(s, _, _) => s.to_string(),
      CommandTree::Top(_) => "".to_string(),
    }
  }
  fn get_help(&self) -> String {
    match self {
      CommandTree::Terminal(_, h) => h.as_ref().map(|h| h.clone()).unwrap_or(format!("")),
      CommandTree::NonTerminal(_, _, h) => h.as_ref().map(|h| h.clone()).unwrap_or(format!("")),
      CommandTree::Top(_) => "".to_string(),
    }
  }
  fn get_children(&self) -> Vec<String> {
    match self {
      CommandTree::Terminal(_, _) => vec![],
      CommandTree::NonTerminal(_, children, _) => children.iter().map(|x| x.get_cmd()).collect(),
      CommandTree::Top(children) => children.iter().map(|x| x.get_cmd()).collect(),
    }
  }
}

struct TabCompleteHandler {
  sigil: char,
  top_level_commands: CommandTree,
}

use linefeed::complete::{Completion, Completer};
use linefeed::terminal::Terminal;

impl TabCompleteHandler {
  fn new(sigil: char, top_level_commands: CommandTree) -> TabCompleteHandler {
    TabCompleteHandler {
      top_level_commands,
      sigil,
    }
  }
}

impl<T: Terminal> Completer<T> for TabCompleteHandler {
  fn complete(&self, word: &str, prompter: &linefeed::prompter::Prompter<T>, start: usize, _end: usize) -> Option<Vec<Completion>> {
    let line = prompter.buffer();

    if line.starts_with(&format!("{}", self.sigil)) {
      let mut words = line[1..(if start == 0 { 1 } else { start })].split_whitespace();
      let mut completions = Vec::new();
      let mut command_tree: Option<&CommandTree> = Some(&self.top_level_commands);

      loop {
        match words.next() {
          None => {
            let top = match command_tree {
              Some(CommandTree::Top(_)) => true,
              _ => false
            };
            let word = if top { word.get(1..).unwrap() } else { word };
            for cmd in command_tree.map(|x| x.get_children()).unwrap_or(vec![]).into_iter() {
              if cmd.starts_with(word) {
                completions.push(Completion {
                    completion: format!("{}{}", if top { ":" } else { "" }, cmd),
                    display: Some(cmd.clone()),
                    suffix: linefeed::complete::Suffix::Some(' ')
                })
              }
            }
            break;
          },
          Some(s) => {
            let new_ptr: Option<&CommandTree> = command_tree.and_then(|cm| match cm {
              CommandTree::Top(children) => children.iter().find(|c| c.get_cmd() == s),
              CommandTree::NonTerminal(_, children, _) => children.iter().find(|c| c.get_cmd() == s),
              CommandTree::Terminal(_, _) => None,
            });
            command_tree = new_ptr;
          }
        }
      }
      Some(completions)
    } else {
      None
    }
  }
}

struct Repl {
  options: EvalOptions,
  languages: Vec<Box<ProgrammingLanguageInterface>>,
  current_language_index: usize,
  interpreter_directive_sigil: char,
  line_reader: linefeed::interface::Interface<linefeed::terminal::DefaultTerminal>,
}

impl Repl {
  fn new(languages: Vec<Box<ProgrammingLanguageInterface>>, initial_index: usize) -> Repl {
    use linefeed::Interface;
    let i = if initial_index < languages.len() { initial_index } else { 0 };

    let line_reader = Interface::new("schala-repl").unwrap();

    Repl {
      options: Repl::get_options(),
      languages: languages,
      current_language_index: i,
      interpreter_directive_sigil: ':',
      line_reader
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
    use linefeed::ReadResult;

    println!("Schala MetaInterpreter version {}", VERSION_STRING);
    println!("Type {}help for help with the REPL", self.interpreter_directive_sigil);

    self.line_reader.load_history(".schala_history").unwrap_or(());

    loop {
      let language_name = self.languages[self.current_language_index].get_language_name();
      let directives = self.get_directives();
      let tab_complete_handler = TabCompleteHandler::new(self.interpreter_directive_sigil, directives);
      self.line_reader.set_completer(std::sync::Arc::new(tab_complete_handler));

      let prompt_str = format!("{} >> ", language_name);
      self.line_reader.set_prompt(&prompt_str);

      match self.line_reader.read_line() {
        Err(e) => {
          println!("Terminal read error: {}", e);
        },
        Ok(ReadResult::Eof) => break,
        Ok(ReadResult::Signal(_)) => break,
        Ok(ReadResult::Input(ref input)) => {
          self.line_reader.add_history_unique(input.to_string());
          let output = match input.chars().nth(0) {
            Some(ch) if ch == self.interpreter_directive_sigil => self.handle_interpreter_directive(input),
            _ => Some(self.input_handler(input)),
          };
          if let Some(o) = output {
            println!("=> {}", o);
          }
        }
      }
    }
    self.line_reader.save_history(".schala_history").unwrap_or(());
    self.save_options();
    println!("Exiting...");
  }

  fn input_handler(&mut self, input: &str) -> String {
    let ref mut language = self.languages[self.current_language_index];
    let interpreter_output = language.execute_pipeline(input, &self.options);
    interpreter_output.to_repl()
  }

  fn get_directives(&self) -> CommandTree {
    let ref passes = self.get_cur_language().get_passes();
    CommandTree::Top(vec![
      CommandTree::term("exit", Some("exit the REPL")),
      CommandTree::term("quit", Some("exit the REPL")),
      CommandTree::term("help", Some("Print this help message")),
      CommandTree::NonTerminal(format!("debug"), vec![
        CommandTree::term("passes", None),
        CommandTree::NonTerminal(format!("show"), passes.iter().map(|p| CommandTree::term(&p.name, None)).collect(), None),
        CommandTree::NonTerminal(format!("hide"), passes.iter().map(|p| CommandTree::term(&p.name, None)).collect(), None),
      ], Some(format!("show or hide pass info for a given pass, or display the names of all passes"))),
      CommandTree::NonTerminal(format!("lang"), vec![
        CommandTree::term("next", None),
        CommandTree::term("prev", None),
        CommandTree::NonTerminal(format!("go"), vec![], None)//TODO
      ], Some(format!("switch between languages, or go directly to a langauge by name"))),
    ])
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
        let directives = match self.get_directives() {
            CommandTree::Top(children) => children,
            _ => panic!("Top-level CommandTree not Top")
        };

        writeln!(buf, "MetaInterpreter options").unwrap();
        writeln!(buf, "-----------------------").unwrap();

        for directive in directives {
          let trailer = " ";
          writeln!(buf, "{}{}- {}", directive.get_cmd(), trailer, directive.get_help()).unwrap();
        }

        writeln!(buf, "").unwrap();
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
        .map(|desc| {
          if self.options.debug_passes.contains_key(&desc.name) {
            let color = "green";
            format!("*{}", desc.name.color(color))
          } else {
            desc.name 
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
        let pass_opt = commands.get(3);
        if let Some(desc) = passes.iter().find(|desc| desc.name == debug_pass) {
          let mut opts = vec![];
          if let Some(opt) = pass_opt {
            opts.push(opt.to_string());
          }
          let msg = format!("{} debug for pass {}", if show { "Enabling" } else { "Disabling" }, debug_pass);
          if show {
            self.options.debug_passes.insert(desc.name.clone(), PassDebugDescriptor { opts });
          } else {
            self.options.debug_passes.remove(&desc.name);
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
