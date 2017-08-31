use language::{ProgrammingLanguageInterface, EvalOptions, ParseError, TokenError, LLVMCodeString};

mod parsing;

pub struct Schala { 
}

impl Schala {
  pub fn new() -> Schala {
    Schala { }
  }
}

impl ProgrammingLanguageInterface for Schala {
  fn get_language_name(&self) -> String {
    "Schala".to_string()
  }

  fn evaluate_in_repl(&mut self, input: &str, eval_options: EvalOptions) -> Vec<String> {
    vec!(format!("evaluation"))
  }
}
