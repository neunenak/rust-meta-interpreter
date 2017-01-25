use std::fmt::Debug;

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

pub trait ProgrammingLanguage<E: EvaluationMachine> {
    type Token: Debug;
    type AST: Debug;

    fn tokenize(input: &str) -> Result<Vec<Self::Token>, TokenError>;
    fn parse(input: Vec<Self::Token>) -> Result<Self::AST, ParseError>;
    fn evaluate(ast: Self::AST, evaluator: &mut E) -> Vec<String>;
    fn compile(ast: Self::AST) -> LLVMCodeString;
}

pub trait EvaluationMachine {
    fn set_option(&mut self, option: &str, value: bool) -> bool;
    fn new() -> Self;
}
