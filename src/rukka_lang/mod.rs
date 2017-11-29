use itertools::Itertools;
use schala_lib::{ProgrammingLanguageInterface, EvalOptions, ReplOutput};
use std::iter::Peekable;
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
    match parse(input).and_then(|x| eval(x)) {
      Ok(s) => output.add_output(s),
      Err(e) => output.add_output(format!("Error: {}", e))
    };
    output
  }
}

fn eval(ast: Sexp) -> Result<String, String> {
  Ok(format!("{:?}", ast))
}

fn parse(input: &str) -> Result<Sexp, String> {
  let mut iter: Peekable<Chars> = input.chars().peekable();
  let output = read_sexp(&mut iter);
  match iter.next() {
    None => output,
    Some(c) => Err(format!("Expected end of input, got {}", c)),
  }
}

fn read_sexp(input: &mut Peekable<Chars>) -> Result<Sexp, String> {
  if input.next() != Some('(') {
    return Err(format!("Expected '('"));
  }
  let mut v = Vec::new();
  loop {
    let c = input.peek().map(|x| *x);
    match c {
      Some(')') | None => break,
      Some('(') => v.push(read_sexp(input)?),
      Some(c) if c.is_whitespace() => { input.next(); continue; },
      Some(c) => v.push(read_sym(input)?)
    }
  }
  if input.next() != Some(')') {
    return Err(format!("Expected ')'"));
  }
  Ok(Sexp::List(v))
}

fn read_sym(input: &mut Peekable<Chars>) -> Result<Sexp, String> {
  let mut sym = String::new();
  loop {
    sym.push(input.next().unwrap());
    match input.peek().map(|x| *x) {
      Some('(') | Some(')') | None => break,
      Some(c) if c.is_whitespace() => break,
      _ => continue,
    }
  }
  Ok(Sexp::Atom(AtomT::Symbol(sym)))
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
struct List<'a> {
  next: Option<&'a List<'a>>,
  data: usize
}

