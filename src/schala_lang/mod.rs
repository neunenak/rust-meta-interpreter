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
    let tokens = parsing::tokenize(input);
    if options.debug_tokens {
      output.add_artifact(TraceArtifact::new("tokens", format!("{:?}", tokens)));
    }

    {
      let token_errors: Vec<&String> = tokens.iter().filter_map(|t| t.get_error()).collect();
      if token_errors.len() != 0 {
        output.add_output(format!("Tokenization error: {:?}\n", token_errors));
        return output;
      }
    }

    let ast = match parsing::parse(tokens) {
      Ok(ast) => {
        if options.debug_parse {
          output.add_artifact(TraceArtifact::new("Recursive descent calls:", format!("{:?}", "OI")));
          output.add_artifact(TraceArtifact::new("ast", format!("{:?}", ast)));
        }
        ast
      },
      Err(err) => {
        output.add_output(format!("Parse error: {:?}\n", err.msg));
        return output;
      }
    };

    let evaluation_output = format!("{:?}", ast);
    output.add_output(evaluation_output);
    return output;
  }
}
