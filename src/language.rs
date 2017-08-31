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

    fn name() -> String;
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
    fn get_language_name(&self) -> String;
    fn set_option(&mut self, option: &str, value: bool) -> bool;
}

impl<PL, T, A, E> LanguageInterface for (PL, PL::Evaluator) where PL: ProgrammingLanguage<Token=T, AST=A, Evaluator=E>, T: Debug, A: Debug, E: EvaluationMachine {
    fn set_option(&mut self, option: &str, value: bool) -> bool {
        self.1.set_option(option, value)
    }
    fn get_language_name(&self) -> String {
        PL::name()
    }
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
            let ref mut evaluator = self.1;
            let mut full_output: Vec<String> = PL::evaluate(ast, evaluator);
            output.push_str(&full_output.pop().unwrap_or("".to_string()));
        }
        output
    }
}

/* below here is new versions of everything */


#[derive(Debug, Default)]
pub struct EvalOptions {
  pub debug_tokens: bool,
  pub debug_parse: bool,
  pub debug_type: bool,
  pub debug_eval: bool,
}

pub trait ProgrammingLanguageInterface {
  fn evaluate_in_repl(&mut self, input: &str, eval_options: EvalOptions) -> Vec<String>;
  fn get_language_name(&self) -> String;
  fn set_option(&mut self, option: &str, value: bool) -> bool {
    false
  }
}

pub trait CompileableLanguage : ProgrammingLanguageInterface {
  fn compile(&mut self) -> LLVMCodeString;
}
