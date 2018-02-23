use itertools::Itertools;
use schala_lib::{ProgrammingLanguageInterface, EvalOptions, TraceArtifact, ReplOutput};

macro_rules! bx {
  ($e:expr) => { Box::new($e) }
}

mod builtin;

mod tokenizing;
mod parsing;
//mod type_check;
mod typechecking;
mod eval;

use self::typechecking::{TypeContext};

pub struct Schala {
  state: eval::State,
  type_context: TypeContext
}

impl Schala {
  pub fn new() -> Schala {
    Schala {
      state: eval::State::new(),
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
    let tokens = tokenizing::tokenize(input);
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

    match self.type_context.type_check_ast(&ast) {
      Ok(ty) => {
        output.add_artifact(TraceArtifact::new("type_check", format!("{:?}", ty)));
      },
      Err(msg) => {
        output.add_artifact(TraceArtifact::new("type_check", msg));
        output.add_output(format!("Type error"));
        return output;
      }
    }

    /*
    self.type_context.add_symbols(&ast);

    if options.debug_symbol_table {
      let text = self.type_context.debug_symbol_table();
      output.add_artifact(TraceArtifact::new("symbol_table", text));
    }

    match self.type_context.type_check(&ast) {
      Ok(ty) => {
        output.add_artifact(TraceArtifact::new("type_check", format!("type: {:?}", ty)));
      },
      Err(msg) => {
        output.add_artifact(TraceArtifact::new("type_check", msg));
        output.add_output(format!("Type error"));
        return output;
      }
    }
    */

    let evaluation_outputs = self.state.evaluate(ast);
    let text_output: String = evaluation_outputs.into_iter().intersperse(format!("\n")).collect();
    output.add_output(text_output);
    return output;
  }
}
