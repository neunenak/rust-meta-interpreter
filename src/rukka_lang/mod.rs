use itertools::Itertools;
use schala_lib::{ProgrammingLanguageInterface, EvalOptions, ReplOutput};

pub struct Rukka { }

impl Rukka {
  pub fn new() -> Rukka { Rukka { } }
}

impl ProgrammingLanguageInterface for Rukka {
  fn get_language_name(&self) -> String {
    "Rukka".to_string()
  }

  fn get_source_file_suffix(&self) -> String {
    format!("rukka")
  }

  fn evaluate_in_repl(&mut self, input: &str, _eval_options: &EvalOptions) -> ReplOutput {
    let mut output = ReplOutput::default();
    output.add_output(parse(input).and_then(|x| eval(x)).unwrap());
    output
  }
}

fn eval(ast: Sexp) -> Result<String, String> {
  Ok(format!("Everything is ()"))
}

enum Token {
  LParen,
  RParen,
  Symbol(String)
}

fn tokenize(input: &str) -> Vec<Token> {
  let mut iter = input.chars().peekable();
  let mut tokens = Vec::new();
  loop {
    match iter.next() {
      None => break,
      Some('(') => tokens.push(LParen),
      Some(')') => tokens.push(RParen),
      Some(c) if c.is_whitespace() => continue,
      Some(c) => {
        let mut sym = String::new();
        loop {
          match iter.peek() {
            None | Some('(') | Some(')') => break,
            Some(c) if c.is_whitespace() => break,
            Some(c) => sym.push_char(c),
          }
          iter.next();
        }
        tokens.push(Symbol(sym));
      }
    }
  }
  tokens
}

fn parse(input: &str) -> Result<Sexp, String> {
  let _tokens = tokenize(input);
  Ok(Sexp::Atom(AtomT::Number(1)))
}

#[derive(Debug)]
enum Sexp {
  Atom(AtomT),
  List(Vec<Sexp>),
}

#[derive(Debug)]
enum AtomT {
  Symbol(String),
  Number(u64),
}

#[derive(Debug)]
struct List<'a> {
  next: Option<&'a List<'a>>,
  data: usize
}

