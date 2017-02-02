use language::{ProgrammingLanguage, EvaluationMachine, ParseError, TokenError, LLVMCodeString};

pub struct Maaru {
}

impl Maaru {
    pub fn new() -> Maaru {
        Maaru { }
    }
}

pub struct MaaruEvaluator {
    pub trace_evaluation: bool,
}

#[derive(Debug)]
pub struct Token { }

#[derive(Debug)]
pub struct AST { }

impl ProgrammingLanguage for Maaru {
    type Token = Token;
    type AST = AST;
    type Evaluator = MaaruEvaluator;

    fn name() -> String {
        "Maaru".to_string()
    }

    fn tokenize(_input: &str) -> Result<Vec<Self::Token>, TokenError> {
        Ok(vec![Token { }])
    }

    fn parse(_input: Vec<Self::Token>) -> Result<Self::AST, ParseError> {
        Ok(AST { })
    }
    fn evaluate(_ast: Self::AST, _evaluator: &mut Self::Evaluator) -> Vec<String> {
        vec!["Unimplemented".to_string()]
    }
    fn compile(_ast: Self::AST) -> LLVMCodeString {
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
