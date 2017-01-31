use ::std::marker::PhantomData;
pub mod tokenizer;
pub mod parser;
pub mod eval;
pub mod compilation;

use language::{ProgrammingLanguage, EvaluationMachine, ParseError, TokenError, LLVMCodeString};

pub use self::eval::Evaluator as SchalaEvaluator;

pub struct Schala<'a> { marker: PhantomData<&'a ()> }
impl<'a> Schala<'a> {
    pub fn new() -> Schala<'a> {
        Schala { marker: PhantomData }
    }
}

impl<'a> ProgrammingLanguage for Schala<'a> {
    type Token = tokenizer::Token;
    type AST = parser::AST;
    type Evaluator = SchalaEvaluator<'a>;

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

impl<'a> EvaluationMachine for SchalaEvaluator<'a> {
    fn set_option(&mut self, option: &str, value: bool) -> bool {
        if option == "trace_evaluation" {
            self.trace_evaluation = value;
            return true;
        }

        false
    }

    fn new() -> SchalaEvaluator<'a> {
        SchalaEvaluator::new(None)
    }
}
