use itertools::Itertools;
use schala_lib::{ProgrammingLanguageInterface, EvalOptions, ReplOutput};
use std::iter::Peekable;
use std::vec::IntoIter;
use std::str::Chars;

pub struct EvaluatorState { }

pub struct Rukka { 
  state: EvaluatorState
}

impl Rukka {
  pub fn new() -> Rukka { Rukka { state: EvaluatorState::new() } }
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

    let output_str: String = sexps.into_iter().enumerate().map(|(i, sexp)| {
      match self.state.eval(sexp) {
        Ok(result) => format!("{}: {}", i, result.print()),
        Err(err) => format!("{} Error: {}", i, err),
      }
    }).intersperse(format!("\n")).collect();
    output.add_output(output_str);
    output
  }
}


impl EvaluatorState {
  fn new() -> EvaluatorState { EvaluatorState { } }
  fn eval(&mut self, expr: Sexp) -> Result<Sexp, String> {
    use self::Sexp::*; use self::AtomT::*;
    Ok(match expr {
      Atom(atom) => match atom {
        Symbol(s) => Atom(Symbol(s)),
        LangString(s) => Atom(LangString(s)),
        Number(s) => Atom(Number(s)),
      },
      List(items) => unimplemented!(),
    })
  }
}

fn read(input: &str) -> Result<Vec<Sexp>, String> {
  let mut chars: Peekable<Chars> = input.chars().peekable();
  let mut tokens = tokenize(&mut chars).into_iter().peekable();
  let mut sexps = Vec::new();
  while let Some(_) = tokens.peek() {
    sexps.push(parse(&mut tokens)?);
  }
  Ok(sexps)
}

#[derive(Debug)]
enum Token {
  LParen,
  RParen,
  Quote,
  Word(String),
  StringLiteral(String),
  NumLiteral(u64),
}

#[derive(Debug)]
enum Sexp {
  Atom(AtomT),
  List(Vec<Sexp>),
}

impl Sexp {
  fn print(&self) -> String {
    use self::Sexp::*; use self::AtomT::*;
    match self {
      &Atom(ref atom) => match atom {
        &Symbol(ref s) => format!("{}", s),
        &LangString(ref s) => format!("\"{}\"", s),
        &Number(ref n) => format!("{}", n),
      },
      _ => format!("<unprintable>")
    }
  }
}

#[derive(Debug)]
enum AtomT {
  Symbol(String),
  LangString(String),
  Number(u64),
}

fn tokenize(input: &mut Peekable<Chars>) -> Vec<Token> {
  use self::Token::*;
  let mut tokens = Vec::new();
  loop {
    match input.next() {
      None => break,
      Some('(') => tokens.push(LParen),
      Some(')') => tokens.push(RParen),
      Some('\'') => tokens.push(Quote),
      Some(c) if c.is_whitespace() => continue,
      Some(c) if c.is_numeric() => {
        let tok: String = input.peeking_take_while(|next| next.is_numeric()).collect();
        let n: u64 = format!("{}{}", c, tok).parse().unwrap();
        tokens.push(NumLiteral(n));
      },
      Some('"') => {
        let string: String = input.scan(false, |escape, cur_char| {
          let seen_escape = *escape;
          *escape = cur_char == '\\' && !seen_escape;

          if cur_char == '"' && !seen_escape {
            None
          } else if cur_char == '\\' && !seen_escape {
            Some(None)
          } else {
            Some(Some(cur_char))
          }
        }).filter_map(|x| x).collect();
        tokens.push(StringLiteral(string));
      }
      Some(c) => {
        let sym: String = input.peeking_take_while(|next| {
          match *next {
            '(' | ')' => false,
            c if c.is_whitespace() => false,
            _ => true
          }
        }).collect();
        tokens.push(Word(format!("{}{}", c, sym)));
      }
    }
  }
  tokens
}

fn parse(tokens: &mut Peekable<IntoIter<Token>>) -> Result<Sexp, String> {
  use self::Token::*;
  match tokens.next() {
    Some(Word(s)) => Ok(Sexp::Atom(AtomT::Symbol(s))),
    Some(StringLiteral(s)) => Ok(Sexp::Atom(AtomT::LangString(s))),
    Some(LParen) => parse_sexp(tokens),
    Some(RParen) => Err(format!("Unexpected ')'")),
    Some(Quote) => {
        let quoted = parse(tokens)?;
        Ok(Sexp::List(vec![Sexp::Atom(AtomT::Symbol(format!("quote"))), quoted]))
    },
    Some(NumLiteral(n)) => Ok(Sexp::Atom(AtomT::Number(n))),
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

