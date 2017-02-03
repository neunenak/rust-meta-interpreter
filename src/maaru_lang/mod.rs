extern crate itertools;
use self::itertools::Itertools;

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
pub enum Token {
    StrLiteral(String),
    Newline,
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Comma,
    Identifier(String),
    Operator(String),
    NumLiteral(Number),
}

#[derive(Debug)]
pub enum Number {
    Integer(u64),
}

#[derive(Debug)]
pub struct AST { }

impl ProgrammingLanguage for Maaru {
    type Token = Token;
    type AST = AST;
    type Evaluator = MaaruEvaluator;

    fn name() -> String {
        "Maaru".to_string()
    }

    fn tokenize(input: &str) -> Result<Vec<Self::Token>, TokenError> {
        use self::Token::*;
        let mut tokens = Vec::new();
        let mut iter = input.chars().peekable();
        while let Some(c) = iter.next() {
            if c == ';' {
                while let Some(c) = iter.next() {
                    if c == '\n' {
                        break;
                    }
                }
                continue;
            }
            let cur_tok = match c {
                c if char::is_whitespace(c) && c != '\n' => continue,
                '\n' => Newline,
                '(' => LParen,
                ')' => RParen,
                '[' => LBracket,
                ']' => RBracket,
                '{' => LBrace,
                '}' => RBrace,
                ',' => Comma,
                c if char::is_alphanumeric(c) => {
                    let mut buffer = String::new();
                    buffer.push(c);
                    buffer.extend(iter.peeking_take_while(|x| char::is_whitespace(*x)));
                    Identifier(buffer)
                },
                _ => unimplemented!(),
            };
            tokens.push(cur_tok);
        }

        Ok(tokens)
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
