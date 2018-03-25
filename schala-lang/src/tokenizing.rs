use itertools::Itertools;
use std::collections::HashMap;
use std::rc::Rc;
use std::iter::{Iterator, Peekable};
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
  Newline, Semicolon,

  LParen, RParen,
  LSquareBracket, RSquareBracket,
  LAngleBracket, RAngleBracket,
  LCurlyBrace, RCurlyBrace,
  Pipe,

  Comma, Period, Colon, Underscore,
  Slash,

  Operator(Rc<String>),
  DigitGroup(Rc<String>), HexLiteral(Rc<String>), BinNumberSigil,
  StrLiteral(Rc<String>),
  Identifier(Rc<String>),
  Keyword(Kw),

  EOF,

  Error(String),
}
use self::TokenType::*;

impl fmt::Display for TokenType {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      &Operator(ref s) => write!(f, "Operator({})", **s),
      &DigitGroup(ref s) => write!(f, "DigitGroup({})", s),
      &HexLiteral(ref s) => write!(f, "HexLiteral({})", s),
      &StrLiteral(ref s) => write!(f, "StrLiteral({})", s),
      &Identifier(ref s) => write!(f, "Identifier({})", s),
      &Error(ref s) => write!(f, "Error({})", s),
      other => write!(f, "{:?}", other),
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Kw {
  If, Else,
  Func,
  For,
  Match,
  Var, Const, Let, In,
  Return,
  Alias, Type, SelfType, SelfIdent,
  Trait, Impl,
  True, False,
  Module
}

lazy_static! {
  static ref KEYWORDS: HashMap<&'static str, Kw> =
    hashmap! {
      "if" => Kw::If,
      "else" => Kw::Else,
      "fn" => Kw::Func,
      "for" => Kw::For,
      "match" => Kw::Match,
      "var" => Kw::Var,
      "const" => Kw::Const,
      "let" => Kw::Let,
      "in" => Kw::In,
      "return" => Kw::Return,
      "alias" => Kw::Alias,
      "type" => Kw::Type,
      "Self" => Kw::SelfType,
      "self" => Kw::SelfIdent,
      "trait" => Kw::Trait,
      "impl" => Kw::Impl,
      "true" => Kw::True,
      "false" => Kw::False,
      "module" => Kw::Module,
    };
}

#[derive(Debug, Clone)]
pub struct Token {
  pub token_type: TokenType,
  pub offset: (usize, usize),
}

impl Token {
  pub fn get_error(&self) -> Option<&String> {
    match self.token_type {
      TokenType::Error(ref s) => Some(s),
      _ => None,
    }
  }
  pub fn to_string_with_metadata(&self) -> String {
    format!("{}(L:{},c:{})", self.token_type, self.offset.0, self.offset.1)
  }
}

const OPERATOR_CHARS: [char; 18] = ['!', '$', '%', '&', '*', '+', '-', '.', ':', '<', '>', '=', '?', '@', '^', '|', '~', '`'];
fn is_operator(c: &char) -> bool {
  OPERATOR_CHARS.iter().any(|x| x == c)
}

type CharIter<I: Iterator<Item=(usize,usize,char)>> = Peekable<I>;

pub fn tokenize(input: &str) -> Vec<Token> {
  let mut tokens: Vec<Token> = Vec::new();

  let mut input  = input.lines().enumerate()
      .intersperse((0, "\n"))
      .flat_map(|(line_idx, ref line)| {
          line.chars().enumerate().map(move |(ch_idx, ch)| (line_idx, ch_idx, ch))
      })
      .peekable();

  while let Some((line_idx, ch_idx, c)) = input.next() {
    let cur_tok_type = match c {
      '/' => match input.peek().map(|t| t.2) {
        Some('/') => {
          while let Some((_, _, c)) = input.next() {
            if c == '\n' {
              break;
            }
          }
          continue;
        },
        Some('*') => {
          input.next();
          let mut comment_level = 1;
          while let Some((_, _, c)) = input.next() {
            if c == '*'  && input.peek().map(|t| t.2) == Some('/') {
              input.next();
              comment_level -= 1;
            } else if c == '/' && input.peek().map(|t| t.2) == Some('*') {
              input.next();
              comment_level += 1;
            }
            if comment_level == 0 {
              break;
            }
          }
          continue;
        },
        _ => Slash
      },
      c if c.is_whitespace() && c != '\n' => continue,
      '\n' => Newline, ';' => Semicolon,
      ':' => Colon, ',' => Comma,
      '(' => LParen, ')' => RParen,
      '{' => LCurlyBrace, '}' => RCurlyBrace,
      '[' => LSquareBracket, ']' => RSquareBracket,
      '"' => handle_quote(&mut input),
      c if c.is_digit(10) => handle_digit(c, &mut input),
      c if c.is_alphabetic() || c == '_' => handle_alphabetic(c, &mut input), //TODO I'll probably have to rewrite this if I care about types being uppercase, also type parameterization
      c if is_operator(&c) => handle_operator(c, &mut input),
      unknown => Error(format!("Unexpected character: {}", unknown)),
    };
    tokens.push(Token { token_type: cur_tok_type, offset: (line_idx, ch_idx) });
  }
  tokens
}

fn handle_digit<I: Iterator<Item=(usize,usize,char)>>(c: char, input: &mut CharIter<I>) -> TokenType {
  if c == '0' && input.peek().map_or(false, |&(_, _, c)| { c == 'x' }) {
    input.next();
    let rest: String = input.peeking_take_while(|&(_, _, ref c)| c.is_digit(16) || *c == '_').map(|(_, _, c)| { c }).collect();
    HexLiteral(Rc::new(rest))
  } else if c == '0' && input.peek().map_or(false, |&(_, _, c)| { c == 'b' }) {
    input.next();
    BinNumberSigil
  } else {
    let mut buf = c.to_string();
    buf.extend(input.peeking_take_while(|&(_, _, ref c)| c.is_digit(10)).map(|(_, _, c)| { c }));
    DigitGroup(Rc::new(buf))
  }
}

fn handle_quote<I: Iterator<Item=(usize,usize,char)>>(input: &mut CharIter<I>) -> TokenType {
  let mut buf = String::new();
  loop {
    match input.next().map(|(_, _, c)| { c }) {
      Some('"') => break,
      Some('\\') => {
        let next = input.peek().map(|&(_, _, c)| { c });
        if next == Some('n') {
          input.next();
          buf.push('\n')
        } else if next == Some('"') {
          input.next();
          buf.push('"');
        } else if next == Some('t') {
          input.next();
          buf.push('\t');
        }
      },
      Some(c) => buf.push(c),
      None => return TokenType::Error(format!("Unclosed string")),
    }
  }
  TokenType::StrLiteral(Rc::new(buf))
}

fn handle_alphabetic<I: Iterator<Item=(usize,usize,char)>>(c: char, input: &mut CharIter<I>) -> TokenType {
  let mut buf = String::new();
  buf.push(c);
  if c == '_' && input.peek().map(|&(_, _, c)| { !c.is_alphabetic() }).unwrap_or(true) {
    return TokenType::Underscore
  }

  loop {
    match input.peek().map(|&(_, _, c)| { c }) {
      Some(c) if c.is_alphanumeric() => {
        input.next();
        buf.push(c);
      },
      _ => break,
    }
  }

  match KEYWORDS.get(buf.as_str()) {
    Some(kw) => TokenType::Keyword(*kw),
    None => TokenType::Identifier(Rc::new(buf)),
  }
}

fn handle_operator<I: Iterator<Item=(usize,usize,char)>>(c: char, input: &mut CharIter<I>) -> TokenType {
  match c {
    '<' | '>' | '|' | '.' => {
      let ref next = input.peek().map(|&(_, _, c)| { c });
      if !next.map(|n| { is_operator(&n) }).unwrap_or(false) {
        return match c {
          '<' => LAngleBracket,
          '>' => RAngleBracket,
          '|' => Pipe,
          '.' => Period,
          _ => unreachable!(),
        }
      }
    },
    _ => (),
  };

  let mut buf = String::new();
  buf.push(c);
  loop {
    match input.peek().map(|&(_, _, c)| { c }) {
      Some(c) if is_operator(&c) => {
        input.next();
        buf.push(c);
      },
      _ => break
    }
  }
  TokenType::Operator(Rc::new(buf))
}

#[cfg(test)]
mod schala_tokenizer_tests {
  use super::*;
  use super::Kw::*;

  macro_rules! digit { ($ident:expr) => { DigitGroup(Rc::new($ident.to_string())) } }
  macro_rules! ident { ($ident:expr) => { Identifier(Rc::new($ident.to_string())) } }
  macro_rules! op { ($ident:expr) => { Operator(Rc::new($ident.to_string())) } }

  #[test]
  fn tokens() {
    let a = tokenize("let a: A<B> = c ++ d");
    let token_types: Vec<TokenType> = a.into_iter().map(move |t| t.token_type).collect();
    assert_eq!(token_types, vec![Keyword(Let), ident!("a"), Colon, ident!("A"),
      LAngleBracket, ident!("B"), RAngleBracket, op!("="), ident!("c"), op!("++"), ident!("d")]);
  }

  #[test]
  fn underscores() {
    let token_types: Vec<TokenType> = tokenize("4_8").into_iter().map(move |t| t.token_type).collect();
    assert_eq!(token_types, vec![digit!("4"), Underscore, digit!("8")]);
  }

  #[test]
  fn comments() {
    let token_types: Vec<TokenType> = tokenize("1  + /* hella /* bro */ */ 2").into_iter().map(move |t| t.token_type).collect();
    assert_eq!(token_types, vec![digit!("1"), op!("+"), digit!("2")]);
  }
}
