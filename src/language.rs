#[derive(Debug)]
pub struct TokenError {
    pub msg: String,
}

impl TokenError {
    pub fn new(msg: &str) -> TokenError {
        TokenError { msg: msg.to_string() }
    }
}

#[derive(Debug)]
pub struct ParseError {
    pub msg: String,
}

pub struct LLVMCodeString(pub String);

#[derive(Debug, Default)]
pub struct EvalOptions {
  pub debug_tokens: bool,
  pub debug_parse: bool,
  pub debug_type: bool,
  pub trace_evaluation: bool,
}

pub trait ProgrammingLanguageInterface {
  fn evaluate_in_repl(&mut self, input: &str, eval_options: EvalOptions) -> Vec<String>;
  fn get_language_name(&self) -> String;
  fn compile(&mut self, _input: &str) -> LLVMCodeString {
    LLVMCodeString("".to_string())
  }
  fn can_compile(&self) -> bool {
    false
  }
}
