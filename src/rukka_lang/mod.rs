use itertools::Itertools;
use schala_lib::{ProgrammingLanguageInterface, EvalOptions, ReplOutput};
use std::iter::Peekable;
use std::vec::IntoIter;
use std::str::Chars;

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
    let sexps = match read(input) {
      Err(err) => {
        output.add_output(format!("Error: {}", err));
        return output;
      },
      Ok(sexps) => sexps
    };

    let mut text = String::new();
    for (i, sexp) in sexps.into_iter().enumerate() {
      match eval(sexp) {
        Ok(result) => text.push_str(&format!("{}: {}", i, result)),
        Err(err) => text.push_str(&format!("{} Error: {}", i, err)),
      }
    }
    output.add_output(text);
    output
  }
}

fn eval(ast: Sexp) -> Result<String, String> {
  Ok(format!("{:?}", ast))
}

fn read(input: &str) -> Result<Vec<Sexp>, String> {
  let mut chars: Peekable<Chars> = input.chars().peekable();
  let mut tokens = tokenize(&mut chars).into_iter().peekable();
  let mut sexps = Vec::new();
  loop {
    sexps.push(parse(&mut tokens)?);
    if let None = tokens.peek() {
      break;
    }
  }
  Ok(sexps)
}

#[derive(Debug)]
enum Token {
  LParen,
  RParen,
  Symbol(String)
}

fn tokenize(input: &mut Peekable<Chars>) -> Vec<Token> {
  use self::Token::*;
  let mut tokens = Vec::new();
  loop {
    match input.next() {
      None => break,
      Some('(') => tokens.push(LParen),
      Some(')') => tokens.push(RParen),
      Some(c) if c.is_whitespace() => continue,
      Some(c) => {
        let mut sym = String::new();
        sym.push(c);
        loop {
          match input.peek().map(|x| *x) {
            Some('(') | Some(')') | None => break,
            Some(c) if c.is_whitespace() => break,
            Some(c) => {
              sym.push(c);
              input.next();
            }
          }
        }
        tokens.push(Symbol(sym));
      }
    }
  }
  tokens
}

fn parse(tokens: &mut Peekable<IntoIter<Token>>) -> Result<Sexp, String> {
  use self::Token::*;
  match tokens.next() {
    Some(Symbol(s)) => Ok(Sexp::Atom(AtomT::Symbol(s))),
    Some(LParen) => parse_sexp(tokens),
    Some(RParen) => Err(format!("Unexpected ')'")),
    None => Err(format!("Unexpected end of input")),
  }
}

fn parse_sexp(tokens: &mut Peekable<IntoIter<Token>>) -> Result<Sexp, String> {
  use self::Token::*;
  let mut vec = Vec::new();
  loop {
    match tokens.peek() {
      None => return Err(format!("Unexpected end of input")),
      Some(&RParen) => { tokens.next(); break},
      _ => vec.push(parse(tokens)?),
    }
  }
  Ok(Sexp::List(vec))
}

#[derive(Debug)]
enum Sexp {
  Atom(AtomT),
  List(Vec<Sexp>),
}

#[derive(Debug)]
enum AtomT {
  Symbol(String),
  //Number(u64),
}

#[derive(Debug)]
struct PointerList<'a> {
  next: Option<&'a PointerList<'a>>,
  data: usize
}

