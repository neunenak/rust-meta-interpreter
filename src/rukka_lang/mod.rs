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
  Ok(format!("Everything is ()"))
}

fn parse(input: &str) -> Result<Sexp, String> {
  let mut iter: Peekable<Chars> = input.chars().peekable();
  read_sexp(iter)
}

fn read_sexp(mut input: Peekable<Chars>) -> Result<Sexp, String> {
  if input.next() != Some('(') {
    return Err(format!("Expected '('"));
  }
  Ok(Sexp::Atom(AtomT::Number(4)))
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

