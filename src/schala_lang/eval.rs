use schala_lang::parsing::AST;

pub struct ReplState {
}

impl ReplState {
  pub fn new() -> ReplState {
    ReplState { }
  }

  pub fn evaluate(&mut self, ast: AST) -> String {
    format!("Evaluated AST: {:?}", ast)
  }
}

