#![feature(box_patterns)]

extern crate itertools;
extern crate schala_repl;

use itertools::Itertools;
use schala_repl::{ProgrammingLanguageInterface, EvalOptions, FinishedComputation, UnfinishedComputation};

pub struct Robo {
}

impl Robo {
  pub fn new() -> Robo {
    Robo { }
  }
}

#[derive(Debug)]
pub struct TokenError {
    pub msg: String,
}

impl TokenError {
    pub fn new(msg: &str) -> TokenError {
        TokenError { msg: msg.to_string() }
    }
}

#[allow(dead_code)]
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

#[allow(dead_code)]
#[derive(Debug)]
pub enum Number {
    IntegerRep(String),
    FloatRep(String)
}

#[allow(dead_code)]
pub type AST = Vec<ASTNode>;

#[allow(dead_code)]
#[derive(Debug)]
pub enum ASTNode {
    FunctionDefinition(String, Expression),
    ImportStatement(String),
}

#[allow(dead_code)]
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

  fn get_source_file_suffix(&self) -> String {
    format!("robo")
  }

  fn execute_pipeline(&mut self, input: &str, _eval_options: &EvalOptions) -> FinishedComputation {
    let output = UnfinishedComputation::default();
    let tokens = match tokenize(input) {
      Ok(tokens) => tokens,
      Err(e) => {
        return output.finish(Err(format!("Tokenize error: {:?}", e)));
      }
    };

    output.finish(Ok(format!("{:?}", tokens)))
  }
}

