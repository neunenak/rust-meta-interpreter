use ::std::marker::PhantomData;
pub mod tokenizer;
pub mod parser;
pub mod eval;
pub mod compilation;

use language::{ProgrammingLanguageInterface, EvalOptions, ProgrammingLanguage, EvaluationMachine, ParseError, TokenError, LLVMCodeString};

pub use self::eval::Evaluator as MaaruEvaluator;

pub struct NewMaaru<'a> {
  evaluator: MaaruEvaluator<'a>
}

impl<'a> NewMaaru<'a> {
  pub fn new() -> NewMaaru<'a> {
    NewMaaru {
      evaluator: MaaruEvaluator::new(None),
    }
  }
}

impl<'a> ProgrammingLanguageInterface for NewMaaru<'a> {
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

pub struct Maaru<'a> { marker: PhantomData<&'a ()> }
impl<'a> Maaru<'a> {
    pub fn new() -> Maaru <'a> {
        Maaru { marker: PhantomData }
    }
}

impl<'a> ProgrammingLanguage for Maaru<'a> {
    type Token = tokenizer::Token;
    type AST = parser::AST;
    type Evaluator = MaaruEvaluator<'a>;

    fn name() -> String {
        "Maaru".to_string()
    }

    fn tokenize(input: &str) -> Result<Vec<Self::Token>, TokenError> {
        tokenizer::tokenize(input)
    }
    fn parse(input: Vec<Self::Token>) -> Result<Self::AST, ParseError> {
        parser::parse(&input, &[]).map_err(|x| ParseError { msg: x.msg })
    }
    fn evaluate(ast: Self::AST, evaluator: &mut Self::Evaluator) -> Vec<String> {
        evaluator.run(ast)
    }
    fn compile(ast: Self::AST) -> LLVMCodeString {
        compilation::compile_ast(ast)
    }
}

impl<'a> EvaluationMachine for MaaruEvaluator<'a> {
    fn set_option(&mut self, option: &str, value: bool) -> bool {
        if option == "trace_evaluation" {
            self.trace_evaluation = value;
            return true;
        }

        false
    }

    fn new() -> MaaruEvaluator<'a> {
        MaaruEvaluator::new(None)
    }
}
