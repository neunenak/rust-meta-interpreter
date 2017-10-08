use itertools::Itertools;
use language::{ProgrammingLanguageInterface, EvalOptions, TraceArtifact, ReplOutput};

mod parsing;
mod type_check;
mod eval;

use self::type_check::{TypeContext};

pub struct Schala {
  state: eval::ReplState,
  type_context: TypeContext
}

impl Schala {
  pub fn new() -> Schala {
    Schala {
      state: eval::ReplState::new(),
      type_context: TypeContext::new(),
    }
  }
}

impl ProgrammingLanguageInterface for Schala {
  fn get_language_name(&self) -> String {
    "Schala".to_string()
  }

  fn get_source_file_suffix(&self) -> String {
    format!("schala")
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

    if options.debug_symbol_table {
      let text = self.type_context.debug_symbol_table();
      output.add_artifact(TraceArtifact::new("symbol_table", text));
    }

    match self.type_context.type_check(&ast) {
      Ok(_) => (),
      Err(msg) => {
        output.add_artifact(TraceArtifact::new("type_check", msg));
        output.add_output(format!("Type error"));
        return output;
      }
    }

    let evaluation_output = self.state.evaluate(ast);
    let mut acc = String::new();
    let mut iter = evaluation_output.iter().peekable();
    while let Some(s) = iter.next() {
      acc.push_str(&s);
      if let Some(_) = iter.peek() {
        acc.push_str("\n");
      }
    }

    output.add_output(acc);
    return output;
  }
}
