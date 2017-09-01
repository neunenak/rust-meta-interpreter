use language::{ProgrammingLanguageInterface, EvalOptions};

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

  fn evaluate_in_repl(&mut self, input: &str, _eval_options: EvalOptions) -> Vec<String> {
    let mut output = vec!(format!("test eval"));

    let tokens = match parsing::tokenize(input) {
      Ok(tokens) => tokens,
      Err(e) => { output.push(format!("{}", e.msg));
        return output;
      }
    };

    let _ast = match parsing::parse(tokens)  {
      Ok(ast) => ast,
      Err(e) => { output.push(format!("{}", e.msg));
        return output;
      }
    };

    output
  }
}
