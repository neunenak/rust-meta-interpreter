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
    output.add_output(eval(input));
    output
  }
}

fn eval(input: &str) -> String {
  let a = List { next: None };
  let b = List { next: Some(&a) };

  format!("Everything is () {:?}", b)
}

#[derive(Debug)]
struct List<'a> {
  next: Option<&'a List<'a>>,
}
