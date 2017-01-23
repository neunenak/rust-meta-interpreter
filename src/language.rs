use std::fmt::Debug;

pub struct TokenError {
    pub msg: String,
}

pub struct ParseError {
    pub msg: String,
}

pub trait ProgrammingLanguage<Evaluator> {
    type Token: Debug;
    type AST: Debug;

    fn tokenize(input: &str) -> Result<Vec<Self::Token>, TokenError>;
    fn parse(input: Vec<Self::Token>) -> Result<Self::AST, ParseError>;
    fn evaluate(ast: Self::AST, evaluator: &mut Evaluator) -> Vec<String>;
    fn compile(ast: Self::AST) -> String;
}
