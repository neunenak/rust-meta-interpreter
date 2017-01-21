
pub struct TokenError {
    pub msg: String,
}

pub struct ParseError {
    pub msg: String,
}

pub trait ProgrammingLanguage {
    type Token;
    type AST;

    fn tokenize(input: &str) -> Result<Vec<Self::Token>, TokenError>;
    fn parse(input: Vec<Self::Token>) -> Result<Self::AST, ParseError>;
    fn evaluate(input: &Self::AST);
    fn compile(input: &Self::AST);
}
