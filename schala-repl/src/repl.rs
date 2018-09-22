use std::fmt::Write as FmtWrite;
use std::io::{Read, Write};
use std::fs::File;

use colored::*;
use itertools::Itertools;
use language::{ProgrammingLanguageInterface, EvalOptions,
  PassDebugOptionsDescriptor};

pub struct Repl {
  options: EvalOptions,
  languages: Vec<Box<ProgrammingLanguageInterface>>,
  current_language_index: usize,
  interpreter_directive_sigil: char,
  line_reader: ::linefeed::interface::Interface<::linefeed::terminal::DefaultTerminal>,
}

impl Repl {
  pub fn new(languages: Vec<Box<ProgrammingLanguageInterface>>, initial_index: usize) -> Repl {
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
        let options: EvalOptions = ::serde_json::from_str(&contents)?;
        Ok(options)
      }).unwrap_or(EvalOptions::default())
  }

  fn save_options(&self) {
    let ref options = self.options;
    let read = File::create(".schala_repl")
      .and_then(|mut file| {
        let buf = ::serde_json::to_string(options).unwrap();
        file.write_all(buf.as_bytes())
      });

    if let Err(err) = read {
      println!("Error saving .schala_repl file {}", err);
    }
  }

  pub fn run(&mut self) {
    use linefeed::ReadResult;

    println!("Schala MetaInterpreter version {}", ::VERSION_STRING);
    println!("Type {}help for help with the REPL", self.interpreter_directive_sigil);

    self.line_reader.load_history(".schala_history").unwrap_or(());

    loop {
      let language_name = self.get_cur_language().get_language_name();
      let directives = self.get_directives();
      let tab_complete_handler = TabCompleteHandler::new(self.interpreter_directive_sigil, directives);
      self.line_reader.set_completer(::std::sync::Arc::new(tab_complete_handler));

      let prompt_str = format!("{} >> ", language_name);
      self.line_reader.set_prompt(&prompt_str).unwrap();

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

    let passes_directives: Vec<CommandTree> = passes.iter()
      .map(|pass_descriptor| {
        let name = &pass_descriptor.name;
        if pass_descriptor.debug_options.len() == 0 {
          CommandTree::term(name, None)
        } else {
          let sub_opts: Vec<CommandTree> = pass_descriptor.debug_options.iter()
            .map(|o| CommandTree::term(o, None)).collect();
          CommandTree::NonTerminal(
            name.clone(),
            sub_opts,
            None
          )
        }
      }).collect();

    CommandTree::Top(vec![
      CommandTree::term("exit", Some("exit the REPL")),
      CommandTree::term("quit", Some("exit the REPL")),
      CommandTree::term("help", Some("Print this help message")),
      CommandTree::NonTerminal(format!("debug"), vec![
        CommandTree::term("passes", None),
        CommandTree::NonTerminal(format!("show"), passes_directives.clone(), None),
        CommandTree::NonTerminal(format!("hide"), passes_directives.clone(), None),
      ], Some(format!("show or hide pass info for a given pass, or display the names of all passes"))),
      CommandTree::NonTerminal(format!("lang"), vec![
        CommandTree::term("next", None),
        CommandTree::term("prev", None),
        CommandTree::NonTerminal(format!("go"), vec![], None)//TODO
      ], Some(format!("switch between languages, or go directly to a langauge by name"))),
      CommandTree::term("doc", Some("Get language-specific help for an item")),
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
        ::std::process::exit(0)
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
      "doc" => self.languages[self.current_language_index]
        .get_doc(&commands)
        .or(Some(format!("No docs implemented"))),
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
            self.options.debug_passes.insert(desc.name.clone(), PassDebugOptionsDescriptor { opts });
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
  fn complete(&self, word: &str, prompter: &::linefeed::prompter::Prompter<T>, start: usize, _end: usize) -> Option<Vec<Completion>> {
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
                    suffix: ::linefeed::complete::Suffix::Some(' ')
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

#[derive(Clone)]
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
