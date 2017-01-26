use language::{ProgrammingLanguage, EvaluationMachine, ParseError, TokenError, LLVMCodeString};

pub struct Maaru {

}

pub struct MaaruEvaluator {
    pub trace_evaluation: bool,
}

#[derive(Debug)]
struct Token { }

#[derive(Debug)]
struct AST { }

impl ProgrammingLanguage<MaaruEvaluator> for Maaru {
    type Token = Token;
    type AST = AST;

    fn tokenize(input: &str) -> Result<Vec<Self::Token>, TokenError> {
        Ok(vec![Token { }])
    }

    fn parse(input: Vec<Self::Token>) -> Result<Self::AST, ParseError> {
        Ok(AST { })
    }
    fn evaluate(ast: Self::AST, evaluator: &mut MaaruEvaluator) -> Vec<String> {
        vec!["Unimplemented".to_string()]
    }
    fn compile(ast: Self::AST) -> LLVMCodeString {
        unimplemented!()
    }
}

impl EvaluationMachine for MaaruEvaluator {
    fn set_option(&mut self, option: &str, value: bool) -> bool {
        if option == "trace_evaluation" {
            self.trace_evaluation = value;
            return true;
        }

        false
    }

    fn new() -> MaaruEvaluator {
        MaaruEvaluator {
            trace_evaluation: false,
        }
    }
}
