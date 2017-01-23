pub mod tokenizer;
pub mod parser;
pub mod eval;
pub mod compilation;

use language::{ProgrammingLanguage, ParseError, TokenError};

pub struct Schala { }

impl<'a> ProgrammingLanguage<eval::Evaluator<'a>> for Schala {
    type Token = tokenizer::Token;
    type AST = parser::AST;

    fn tokenize(input: &str) -> Result<Vec<Self::Token>, TokenError> {
        tokenizer::tokenize(input).map_err(|x| TokenError { msg: x.msg })
    }

    fn parse(input: Vec<Self::Token>) -> Result<Self::AST, ParseError> {
        parser::parse(&input, &[]).map_err(|x| ParseError { msg: x.msg })
    }
    fn evaluate(ast: Self::AST, evaluator: &mut eval::Evaluator) -> Vec<String> {
        evaluator.run(ast)
    }
    fn compile(ast: Self::AST) -> String {
        compilation::compile_ast(ast)
    }
}

