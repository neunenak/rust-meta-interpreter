pub mod tokenizer;
pub mod parser;
pub mod eval;
pub mod compilation;

use language::{ProgrammingLanguageInterface, EvalOptions, LLVMCodeString};

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

  fn evaluate_in_repl(&mut self, input: &str, options: EvalOptions) -> Vec<String> {
    let mut output = vec![];
    let tokens = match tokenizer::tokenize(input) {
      Ok(tokens) => {
        if options.debug_tokens {
          output.push(format!("{:?}", tokens));
        }
        tokens
      },
      Err(err) => {
        let msg = format!("Tokenization error: {:?}\n", err.msg);
        output.push(msg);
        return output;
      }
    };

    let ast = match parser::parse(&tokens, &[]) {
      Ok(ast) => {
        if options.debug_parse {
          output.push(format!("{:?}", ast));
        }
        ast
      },
      Err(err) => {
        let msg = format!("Parse error: {:?}\n", err.msg);
        output.push(msg);
        return output;
      }
    };

    let evaluation_output = self.evaluator.run(ast);
    output.extend(evaluation_output);

    return output;
  }
}
