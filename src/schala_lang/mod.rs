use itertools::Itertools;
use schala_lib::{ProgrammingLanguageInterface, EvalOptions, TraceArtifact, LanguageOutput, UnfinishedComputation, FinishedComputation};

macro_rules! bx {
  ($e:expr) => { Box::new($e) }
}

mod builtin;

mod tokenizing;
mod parsing;
mod typechecking;
mod eval;


use self::typechecking::{TypeContext};

pub struct Schala {
  state: eval::State<'static>,
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

  fn repl_evaluate(&mut self, input: &str, options: &EvalOptions) -> FinishedComputation {
    let mut evaluation = UnfinishedComputation::default();

    //tokenzing
    let tokens = tokenizing::tokenize(input);
    let token_string = tokens.iter().map(|t| format!("{:?}<L:{},C:{}>", t.token_type, t.offset.0, t.offset.1)).join(", ");
    evaluation.add_artifact(TraceArtifact::new("tokens", token_string));

    {
      let token_errors: Vec<&String> = tokens.iter().filter_map(|t| t.get_error()).collect();
      if token_errors.len() != 0 {
        return evaluation.output(Err(format!("Tokenization error: {:?}\n", token_errors)));
      }
    }

    // parsing
    let ast = match parsing::parse(tokens) {
      (Ok(ast), trace) => {
        evaluation.add_artifact(TraceArtifact::new_parse_trace(trace));
        evaluation.add_artifact(TraceArtifact::new("ast", format!("{:?}", ast)));
        ast
      },
      (Err(err), trace) => {
        evaluation.add_artifact(TraceArtifact::new_parse_trace(trace));
        return evaluation.output(Err(format!("Parse error: {:?}\n", err.msg)));
      }
    };


    //symbol table
    match self.type_context.add_top_level_types(&ast) {
      Ok(()) => (),
      Err(msg) => {
        evaluation.add_artifact(TraceArtifact::new("type_check", msg));
      }
    };

    //typechecking
    match self.type_context.type_check_ast(&ast) {
      Ok(ty) => evaluation.add_artifact(TraceArtifact::new("type_check", format!("{:?}", ty))),
      Err(msg) => evaluation.add_artifact(TraceArtifact::new("type_check", msg)),
    };

    let text = self.type_context.debug_symbol_table();
    evaluation.add_artifact(TraceArtifact::new("symbol_table", text));

    let evaluation_outputs = self.state.evaluate(ast);
    let text_output: String = evaluation_outputs.into_iter().intersperse(format!("\n")).collect();
    evaluation.output(Ok(text_output))
  }
}
