use itertools::Itertools;
use language::{ProgrammingLanguageInterface, EvalOptions, TraceArtifact, ReplOutput};

mod parsing;
mod eval;

use self::eval::TypeCheck;

pub struct Schala {
  state: eval::ReplState
}

impl Schala {
  pub fn new() -> Schala {
    Schala {
      state: eval::ReplState::new(),
    }
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
      let token_string = tokens.iter().map(|t| format!("{:?}<{}>", t.token_type, t.offset)).join(", ");
      output.add_artifact(TraceArtifact::new("tokens", format!("{:?}", token_string)));

    }

    {
      let token_errors: Vec<&String> = tokens.iter().filter_map(|t| t.get_error()).collect();
      if token_errors.len() != 0 {
        output.add_output(format!("Tokenization error: {:?}\n", token_errors));
        return output;
      }
    }

    let ast = match parsing::parse(tokens) {
      (Ok(ast), trace) => {
        if options.debug_parse {
          output.add_artifact(TraceArtifact::new_parse_trace(trace));
          output.add_artifact(TraceArtifact::new("ast", format!("{:?}", ast)));
        }
        ast
      },
      (Err(err), trace) => {
        output.add_artifact(TraceArtifact::new_parse_trace(trace));
        output.add_output(format!("Parse error: {:?}\n", err.msg));
        return output;
      }
    };

    match self.state.type_check(&ast) {
      TypeCheck::OK => (),
      TypeCheck::Error(s) => {
        output.add_artifact(TraceArtifact::new("type_check", s));
        output.add_output(format!("Type error"));
        return output;
      }
    }

    let evaluation_output = self.state.evaluate(ast);
    output.add_output(evaluation_output);
    return output;
  }
}
