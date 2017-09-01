use language::{ProgrammingLanguageInterface, EvalOptions, TraceArtifact, ReplOutput};

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

  fn evaluate_in_repl(&mut self, input: &str, options: &EvalOptions) -> ReplOutput {
    let mut output = ReplOutput::default();
    let tokens = match parsing::tokenize(input) {
      Ok(tokens) => {
        if options.debug_tokens {
          output.add_artifact(TraceArtifact::new("tokens", format!("{:?}", tokens)));
        }
        tokens
      },
      Err(err) => {
        output.add_output(format!("Tokenization error: {:?}\n", err.msg));
        return output;
      }
    };

    let ast = match parsing::parse(tokens) {
      Ok(ast) => {
        if options.debug_parse {
          output.add_artifact(TraceArtifact::new("ast", format!("{:?}", ast)));
        }
        ast
      },
      Err(err) => {
        output.add_output(format!("Parse error: {:?}\n", err.msg));
        return output;
      }
    };

    let evaluation_output = format!("test eval");
    output.add_output(evaluation_output);
    return output;
  }
}
