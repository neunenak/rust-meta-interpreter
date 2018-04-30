#![feature(box_patterns)]

extern crate schala_repl;

mod tokenizer;
mod parser;
mod eval;
mod compilation;

use schala_repl::{ProgrammingLanguageInterface, EvalOptions, UnfinishedComputation, FinishedComputation, TraceArtifact};

#[derive(Debug)]
pub struct TokenError {
    pub msg: String,
}

impl TokenError {
    pub fn new(msg: &str) -> TokenError {
        TokenError { msg: msg.to_string() }
    }
}

pub use self::eval::Evaluator as MaaruEvaluator;

pub struct Maaru<'a> {
  evaluator: MaaruEvaluator<'a>
}

impl<'a> Maaru<'a> {
  pub fn new() -> Maaru<'a> {
    Maaru {
      evaluator: MaaruEvaluator::new(None),
    }
  }
}

impl<'a> ProgrammingLanguageInterface for Maaru<'a> {
  fn get_language_name(&self) -> String {
    "Maaru".to_string()
  }
  fn get_source_file_suffix(&self) -> String {
    format!("maaru")
  }

  fn execute_pipeline(&mut self, input: &str, options: &EvalOptions) -> FinishedComputation {
    let mut output = UnfinishedComputation::default();

    let tokens = match tokenizer::tokenize(input) {
      Ok(tokens) => {
        if options.debug.tokens {
          output.add_artifact(TraceArtifact::new("tokens", format!("{:?}", tokens)));
        }
        tokens
      },
      Err(err) => {
        return output.finish(Err(format!("Tokenization error: {:?}\n", err.msg)))
      }
    };

    let ast = match parser::parse(&tokens, &[]) {
      Ok(ast) => {
        if options.debug.ast {
          output.add_artifact(TraceArtifact::new("ast", format!("{:?}", ast)));
        }
        ast
      },
      Err(err) => {
        return output.finish(Err(format!("Parse error: {:?}\n", err.msg)))
      }
    };
    let mut evaluation_output = String::new();
    for s in self.evaluator.run(ast).iter() {
      evaluation_output.push_str(s);
    }
    output.finish(Ok(evaluation_output))
  }

  /* TODO make this work with new framework */
  /*
  fn can_compile(&self) -> bool {
    true
  }

  fn compile(&mut self, input: &str) -> LLVMCodeString {
    let tokens = match tokenizer::tokenize(input) {
      Ok(tokens) =>  tokens,
      Err(err) => {
        let msg = format!("Tokenization error: {:?}\n", err.msg);
        panic!("{}", msg);
      }
    };

    let ast = match parser::parse(&tokens, &[]) {
      Ok(ast) => ast,
      Err(err) => {
        let msg = format!("Parse error: {:?}\n", err.msg);
        panic!("{}", msg);
      }
    };
    compilation::compile_ast(ast)
  }
  */
}
