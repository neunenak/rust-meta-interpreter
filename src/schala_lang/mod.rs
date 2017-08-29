use language::{ProgrammingLanguage, EvaluationMachine, ParseError, TokenError, LLVMCodeString};

pub struct Schala { 
}

#[derive(Debug)]
enum Token { }
#[derive(Debug)]
enum AST { }

impl Schala {
  pub fn new() -> Schala {
    Schala { }
  }
}

impl ProgrammingLanguage for Schala {
  type Token = Token;
  type AST = AST;
  type Evaluator = SchalaEvaluator;

  fn name() -> String {
    "Schala".to_string()
  }

  fn tokenize(input: &str) -> Result<Vec<Self::Token>, TokenError> {
    unimplemented!()
  }
  fn parse(input: Vec<Self::Token>) -> Result<Self::AST, ParseError> {
    unimplemented!()
  }
  fn evaluate(ast: Self::AST, evaluator: &mut Self::Evaluator) -> Vec<String> {
    unimplemented!()
  }
  fn compile(ast: Self::AST) -> LLVMCodeString {
    unimplemented!()
  }
}

pub struct SchalaEvaluator { }

impl EvaluationMachine for SchalaEvaluator {
  fn new() -> SchalaEvaluator {
    SchalaEvaluator { }
  }
  fn set_option(&mut self, option: &str, value: bool) -> bool {
    false
  }
}


