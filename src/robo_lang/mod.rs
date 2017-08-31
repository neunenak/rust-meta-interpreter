extern crate itertools;
use self::itertools::Itertools;

use language::{ProgrammingLanguageInterface, EvalOptions, ProgrammingLanguage, EvaluationMachine, ParseError, TokenError, LLVMCodeString};

pub struct Robo {
}

impl Robo {
    pub fn new() -> Robo {
        Robo { }
    }
}

pub struct RoboEvaluator {
    pub trace_evaluation: bool,
}

#[derive(Debug)]
pub enum Token {
    StrLiteral(String),
    Backtick,
    Newline,
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Period,
    Comma,
    Colon,
    Semicolon,
    SingleQuote,
    Identifier(String),
    Operator(String),
    NumLiteral(Number),
}

#[derive(Debug)]
pub enum Number {
    IntegerRep(String),
    FloatRep(String)
}

pub type AST = Vec<ASTNode>;

#[derive(Debug)]
pub enum ASTNode {
    FunctionDefinition(String, Expression),
    ImportStatement(String),
}

#[derive(Debug)]
pub enum Expression {

}

fn tokenize(input: &str) -> Result<Vec<Token>, TokenError> {
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
      ':' => Colon,
      ';' => Semicolon,
      '.' => Period,
      '`' => Backtick,
      '\'' => SingleQuote,
      '"' => {
        let mut buffer = String::new();
        loop {
          match iter.next() {
            Some(x) if x == '"' => break,
            Some(x) => buffer.push(x),
            None => return Err(TokenError::new("Unclosed quote")),
          }
        }
        StrLiteral(buffer)
      }
      c if c.is_digit(10) => {
        let mut integer = true;
        let mut buffer = String::new();
        buffer.push(c);
        buffer.extend(iter.peeking_take_while(|x| x.is_digit(10)));
        if let Some(&'.') = iter.peek() {
          buffer.push(iter.next().unwrap());
          integer = false;
        }
        buffer.extend(iter.peeking_take_while(|x| x.is_digit(10)));
        let inner = if integer {
          Number::IntegerRep(buffer)
        } else {
          Number::FloatRep(buffer)
        };
        NumLiteral(inner)
      },
      c if char::is_alphanumeric(c) => {
        let mut buffer = String::new();
        buffer.push(c);
        buffer.extend(iter.peeking_take_while(|x| char::is_alphanumeric(*x)));
        Identifier(buffer)
      },
      c => {
        let mut buffer = String::new();
        buffer.push(c);
        buffer.extend(iter.peeking_take_while(|x| !char::is_whitespace(*x)));
        Operator(buffer)
      }
    };
    tokens.push(cur_tok);
  }

  Ok(tokens)
}

impl ProgrammingLanguageInterface for Robo {
  fn get_language_name(&self) -> String {
    "Robo".to_string()
  }

  fn evaluate_in_repl(&mut self, input: &str, eval_options: EvalOptions) -> Vec<String> {
    let mut output = vec!();
    let tokens = match tokenize(input) {
      Ok(tokens) => tokens,
      Err(e) => {
        output.push(format!("Tokenize error: {:?}", e));
        return output;
      }
    };

    output.push(format!("{:?}", tokens));
    output
  }
}

impl EvaluationMachine for RoboEvaluator {
    fn set_option(&mut self, option: &str, value: bool) -> bool {
        if option == "trace_evaluation" {
            self.trace_evaluation = value;
            return true;
        }

        false
    }

    fn new() -> RoboEvaluator {
        RoboEvaluator {
            trace_evaluation: false,
        }
    }
}
