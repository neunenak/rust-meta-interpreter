use ::std::marker::PhantomData;
pub mod tokenizer;
pub mod parser;
pub mod eval;
pub mod compilation;

use language::{ProgrammingLanguage, EvaluationMachine, ParseError, TokenError, LLVMCodeString};

pub use self::eval::Evaluator as MaaruEvaluator;

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
