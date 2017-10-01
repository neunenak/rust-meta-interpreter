use schala_lang::parsing::AST;

pub struct ReplState {
}

pub enum TypeCheck {
  OK,
  Error(String)
}

impl ReplState {
  pub fn new() -> ReplState {
    ReplState { }
  }

  pub fn evaluate(&mut self, ast: AST) -> String {
    format!("Evaluated AST: {:?}", ast)
  }

  pub fn type_check(&mut self, _ast: &AST) -> TypeCheck {
    //TypeCheck::Error("type lol".to_string())
    TypeCheck::OK
  }
}

