pub mod tokenizer;
pub mod parser;
pub mod eval;
pub mod compilation;

use language::{ProgrammingLanguageInterface, EvalOptions, ReplOutput, TraceArtifact, LLVMCodeString};

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

  fn evaluate_in_repl(&mut self, input: &str, options: &EvalOptions) -> ReplOutput {
    let mut output = ReplOutput::default();

    let tokens = match tokenizer::tokenize(input) {
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

    let ast = match parser::parse(&tokens, &[]) {
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
    let mut evaluation_output = String::new();
    for s in self.evaluator.run(ast).iter() {
      evaluation_output.push_str(s);
    }
    output.add_output(evaluation_output);
    return output;
  }

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
}
