use std::default::Default;
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

pub trait ProgrammingLanguage {
    type Token: Debug;
    type AST: Debug;
    type Evaluator: EvaluationMachine;

    fn tokenize(input: &str) -> Result<Vec<Self::Token>, TokenError>;
    fn parse(input: Vec<Self::Token>) -> Result<Self::AST, ParseError>;
    fn evaluate(ast: Self::AST, evaluator: &mut Self::Evaluator) -> Vec<String>;
    fn compile(ast: Self::AST) -> LLVMCodeString;
}

pub trait EvaluationMachine {
    fn set_option(&mut self, option: &str, value: bool) -> bool;
    fn new() -> Self;
}

#[derive(Default)]
pub struct LanguageInterfaceOptions {
    pub show_parse: bool,
    pub show_tokens: bool,
    pub show_llvm_ir: bool,
}

pub trait LanguageInterface {
    fn evaluate_in_repl(&mut self, input: &str, options: LanguageInterfaceOptions) -> String;
}

impl<PL, T, A, E> LanguageInterface for PL where PL: ProgrammingLanguage<Token=T, AST=A, Evaluator=E>, T: Debug, A: Debug, E: EvaluationMachine {
    fn evaluate_in_repl(&mut self, input: &str, options: LanguageInterfaceOptions) -> String {
        let mut output = String::new();

        let tokens = match PL::tokenize(input) {
            Ok(tokens) => tokens,
            Err(err) => {
                output.push_str(&format!("Tokenization error: {}\n", err.msg));
                return output;
            }
        };

        if options.show_tokens {
            output.push_str(&format!("Tokens: {:?}\n", tokens));
        }

        let ast = match PL::parse(tokens) {
            Ok(ast) => ast,
            Err(err) => {
                output.push_str(&format!("Parse error: {:?}\n", err.msg));
                return output;
            }
        };

        if options.show_parse {
            output.push_str(&format!("AST: {:?}\n", ast));
        }

        if options.show_llvm_ir {
            let LLVMCodeString(s) = PL::compile(ast);
            output.push_str(&s);
        } else {
            // for now only handle last output
            let mut evaluator = PL::Evaluator::new();
            let mut full_output: Vec<String> = PL::evaluate(ast, &mut evaluator);
            output.push_str(&full_output.pop().unwrap_or("".to_string()));
        }
        output
    }
}
