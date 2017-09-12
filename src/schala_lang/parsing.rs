extern crate itertools;

use std::collections::HashMap;
use std::rc::Rc;
use std::iter::{Enumerate, Peekable};
use self::itertools::Itertools;
use std::vec::IntoIter;
use std::str::Chars;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
  Newline, Semicolon,

  LParen, RParen,
  LSquareBracket, RSquareBracket,
  LAngleBracket, RAngleBracket,
  LCurlyBrace, RCurlyBrace,
  Pipe,

  Comma, Period, Colon, Underscore,

  Operator(Rc<String>),
  DigitGroup(Rc<String>), HexNumberSigil, BinNumberSigil,
  StrLiteral(Rc<String>),
  Identifier(Rc<String>),
  Keyword(Kw),

  EOF,

  Error(String),
}
use self::TokenType::*;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Kw {
  If, Else,
  Func,
  For,
  Match,
  Var, Const, Let, In,
  Alias, Type, SelfType, SelfIdent,
  Trait, Impl,
  True, False
}
use self::Kw::*;

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
      "alias" => Kw::Alias,
      "type" => Kw::Type,
      "Self" => Kw::SelfType,
      "self" => Kw::SelfIdent,
      "trait" => Kw::Trait,
      "impl" => Kw::Impl,
      "true" => Kw::True,
      "false" => Kw::False,
    };
}

#[derive(Debug)]
pub struct Token {
  token_type: TokenType,
  offset: usize,
}

impl Token {
  pub fn get_error(&self) -> Option<&String> {
    match self.token_type {
      TokenType::Error(ref s) => Some(s),
      _ => None,
    }
  }
}

fn is_digit(c: &char) -> bool {
  c.is_digit(10)
}

type CharIter<'a> = Peekable<Enumerate<Chars<'a>>>;

pub fn tokenize(input: &str) -> Vec<Token> {
  let mut tokens: Vec<Token> = Vec::new();
  let mut input: CharIter = input.chars().enumerate().peekable();

  while let Some((idx, c)) = input.next() {
    let cur_tok_type = match c {
      '#' => {
        if let Some(&(_, '{')) = input.peek() {
        } else {
          while let Some((_, c)) = input.next() {
            if c == '\n' {
              break;
            }
          }
        }
        continue;
      },
      c if c.is_whitespace() && c != '\n' => continue,
      '\n' => Newline, ';' => Semicolon,
      ':' => Colon, ',' => Comma, '.' => Period,
      '(' => LParen, ')' => RParen,
      '{' => LCurlyBrace, '}' => RCurlyBrace,
      '<' => LAngleBracket, '>' => RAngleBracket,
      '[' => LSquareBracket, ']' => RSquareBracket,
      '|' => Pipe,
      '"' => handle_quote(&mut input),
      c if is_digit(&c) => handle_digit(c, &mut input),
      c if c.is_alphabetic() || c == '_' => handle_alphabetic(c, &mut input), //TODO I'll probably have to rewrite this if I care about types being uppercase, also type parameterization
      c => handle_operator(c, &mut input),
    };
    tokens.push(Token { token_type: cur_tok_type, offset: idx });
  }
  tokens
}

fn handle_digit(c: char, input: &mut CharIter) -> TokenType {
  if c == '0' && input.peek().map_or(false, |&(_, c)| { c == 'x' }) {
    input.next();
    HexNumberSigil
  } else if c == '0' && input.peek().map_or(false, |&(_, c)| { c == 'b' }) {
    input.next();
    BinNumberSigil
  } else {
    let mut buf = c.to_string();
    buf.extend(input.peeking_take_while(|&(_, ref c)| is_digit(c)).map(|(_, c)| { c }));
    DigitGroup(Rc::new(buf))
  }
}

fn handle_quote(input: &mut CharIter) -> TokenType {
  let mut buf = String::new();
  loop {
    match input.next().map(|(_, c)| { c }) {
      Some('"') => break,
      Some('\\') => {
        let next = input.peek().map(|&(_, c)| { c });
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

fn handle_alphabetic(c: char, input: &mut CharIter) -> TokenType {
  let mut buf = String::new();
  buf.push(c);
  if c == '_' && input.peek().map(|&(_, c)| { !c.is_alphabetic() }).unwrap_or(true) {
    return TokenType::Underscore
  }

  loop {
    match input.peek().map(|&(_, c)| { c }) {
      Some(c) if c.is_alphanumeric() => {
        input.next();
        buf.push(c);
      },
      _ => break,
    }
  }

  match KEYWORDS.get(buf.as_str()) {
    Some(kw) => TokenType::Keyword(kw.clone()),
    None => TokenType::Identifier(Rc::new(buf)),
  }
}

fn handle_operator(c: char, input: &mut CharIter) -> TokenType {
  let mut buf = String::new();
  buf.push(c);
  loop {
    match input.peek().map(|&(_, c)| { c }) {
      Some(c) if !c.is_alphabetic() && !c.is_whitespace() => {
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
  use super::TokenType::*;
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
}

/*
Schala (PROVISIONAL!!) EBNF grammar

'' = literal, all other symbols are nonterminals

program := (statement delimiter ?)*
delimiter := 'Newline' | ';'
statement := declaration | expression

declaration := module | function | type_decl

type_decl := 'type' type_format
type_format := 'alias' '=' type | type_constructor
type_constructor := capital_ident '=' type_rhs
type_rhs := struct_decl | type_variant ('|' type_variant)*
struct_decl := 'struct' '{' (ident ':' type)* '}' 
type_variant := capital_ident | tuple_type | capital_ident struct_decl
tuple_type := // something like Variant(a,b)
type := // something like Type[A[b]]

ascription := expression (':' type)+

function := 'fn' prototype '{' (statement)* '}'
prototype := identifier '(' identlist ')'
identlist := identifier (',' identifier)* | ε


declaration :=  FN prototype LCurlyBrace (statement)* RCurlyBrace
prototype := identifier LParen identlist RParen
identlist := Ident (Comma Ident)* | ε
exprlist  := Expression (Comma Expression)* | ε
itemlist  := Ident COLON Expression (Comma Ident COLON Expression)* | ε

expression := postop_expression (op postop_expression)*
postop_expression := primary_expression postop
primary_expression :=  number_expr | String | identifier_expr | paren_expr | conditional_expr | while_expr | lambda_expr | list_expr | struct_expr
number_expr := (PLUS | MINUS ) number_expr | Number
identifier_expr := call_expression | Variable
list_expr := LSquareBracket exprlist RSquareBracket
struct_expr := LCurlyBrace itemlist RCurlyBrace
call_expression := Identifier LParen exprlist RParen
while_expr := WHILE primary_expression LCurlyBrace (expression delimiter)* RCurlyBrace
paren_expr := LParen expression RParen
conditional_expr := IF expression LCurlyBrace (expression delimiter)* RCurlyBrace (LCurlyBrace (expresion delimiter)* RCurlyBrace)?
lambda_expr := FN LParen identlist RParen LCurlyBrace (expression delimiter)* RCurlyBrace
lambda_call :=  | LParen exprlist RParen
postop := ε | LParen exprlist RParen | LBracket expression RBracket
op := '+', '-', etc.
*/


/* Schala EBNF Grammar */
/*

program := (statement delimiter)* EOF
delimiter := NEWLINE | SEMICOLON
statement := expression | declaration

declaration := type_declaration | func_declaration

type_declaration := TYPE identifier
func_declaration := FN identifier LParen param_list RParen

param_list := (identifier type_anno+ Comma)*

type_anno := Colon type

expression := precedence_expr
precedence_expr := primary
primary := literal
literal := TRUE | FALSE | number_literal | str_literal

identifier := IDENTIFIER

// a float_literal can still be assigned to an int in type-checking
number_literal := int_literal | float_literal
int_literal = (HEX_SIGIL | BIN_SIGIL) digits
float_literal := digits (PERIOD digits)
digits := (digit_group underscore)+

*/

type TokenIter = Peekable<IntoIter<Token>>;

#[derive(Debug)]
pub struct ParseError {
  pub msg: String,
}

impl ParseError {
  fn new<T>(msg: &str) -> ParseResult<T> {
    Err(ParseError { msg: msg.to_string() })
  }
}

pub type ParseResult<T> = Result<T, ParseError>;

struct Parser {
  tokens: TokenIter,
}

impl Parser {
  fn new(input: Vec<Token>) -> Parser {
    Parser { tokens: input.into_iter().peekable() }
  }

  fn peek(&mut self) -> TokenType {
    self.tokens.peek().map(|ref t| { t.token_type.clone() }).unwrap_or(TokenType::EOF)
  }

  fn next(&mut self) -> TokenType {
    self.tokens.next().map(|ref t| { t.token_type.clone() }).unwrap_or(TokenType::EOF)
  }
}

macro_rules! expect {
  ($self:expr, $token_type:pat, $message:expr) => {
    match $self.peek() {
      $token_type => $self.next(),
      _ => return Err(ParseError { msg: $message.to_string() }),
    }
  }
}

#[derive(Debug, PartialEq)]
pub struct AST(Vec<Statement>);

#[derive(Debug, PartialEq)]
pub enum Statement {
  Expression(Expression),
  Declaration(Declaration),
}

#[derive(Debug, PartialEq)]
pub enum Declaration {
  FuncDecl,
  TypeDecl(Rc<String>, TypeBody)
}

#[derive(Debug, PartialEq)]
pub enum TypeBody {
  TypeBody
}

#[derive(Debug, PartialEq)]
pub enum Expression {
  IntLiteral(u64),
  FloatLiteral(f64),
  BinExp(Operation, Box<Expression>, Box<Expression>)
}

#[derive(Debug, PartialEq)]
pub struct Operation {
  op: Rc<String>
}

impl Operation {
  fn min_precedence() -> i32 {
    i32::min_value()
  }

  fn get_precedence(op: Rc<String>) -> i32 {
    let c: char = op.chars().next().unwrap();
    match c {
      '+' | '-' => 10,
      '*' | '/' | '%' => 20,
      _ => 30,
    }
  }
}

impl Parser {
  fn program(&mut self) -> ParseResult<AST> {
    let mut statements = Vec::new();
    loop {
      match self.peek() {
        EOF => break,
        Newline | Semicolon => {
          self.next();
          continue;
        },
        _ => statements.push(self.statement()?),
      }
    }
    Ok(AST(statements))
  }

  fn statement(&mut self) -> ParseResult<Statement> {
    //TODO handle error recovery here
    match self.peek() {
      Keyword(Type) => self.type_declaration().map(|decl| { Statement::Declaration(decl) }),
      Keyword(Func)=> self.func_declaration().map(|func| { Statement::Declaration(func) }),
      _ => self.expression().map(|expr| { Statement::Expression(expr) } ),
    }
  }

  fn type_declaration(&mut self) -> ParseResult<Declaration> {
    expect!(self, Keyword(Type), "Expected 'type'");
    let name = self.identifier()?;
    Ok(Declaration::TypeDecl(name, TypeBody::TypeBody))
  }

  fn func_declaration(&mut self) -> ParseResult<Declaration> {
    expect!(self, Keyword(Func), "Expected 'fn'");
    let name = self.identifier()?;
    expect!(self, LParen, "Expected '('");
    let params = self.param_list();
    expect!(self, RParen, "Expected ')'");
    Ok(Declaration::FuncDecl)
  }

  fn param_list(&mut self) -> ParseResult<Vec<Rc<String>>> {
    Ok(vec!())
  }

  fn expression(&mut self) -> ParseResult<Expression> {
    self.precedence_expr(Operation::min_precedence())
  }

  // this implements Pratt parsing, see http://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/
  fn precedence_expr(&mut self, precedence: i32) -> ParseResult<Expression> {
    use self::Expression::*;

    //TODO clean this up
    let mut lhs = self.primary()?;
    loop {
      let op_str = match self.peek() {
        Operator(op) => op,
        _ => break,
      };
      println!("Opstr: {}", op_str);
      let new_precedence = Operation::get_precedence(op_str);
      println!("new {} and old {} precedence", new_precedence, precedence);
      if precedence >= new_precedence {
        break;
      }
      let op_str = match self.next() {
        Operator(op) => op,
        _ => unreachable!(),
      };
      let rhs = self.precedence_expr(new_precedence)?;
      let operation = Operation { op: op_str };
      lhs = BinExp(operation, Box::new(lhs), Box::new(rhs));
    }
    Ok(lhs)
  }

  fn primary(&mut self) -> ParseResult<Expression> {
    self.literal()
  }

  fn identifier(&mut self) -> ParseResult<Rc<String>> {
    match self.next() {
      Identifier(s) => Ok(s),
      p => ParseError::new(&format!("Expected an identifier, got {:?}", p)),
    }
  }

  fn literal(&mut self) -> ParseResult<Expression> {
    match self.peek() {
      DigitGroup(_) | HexNumberSigil | BinNumberSigil | Period => self.number_literal(),
      t => panic!("trying to parse {:?}", t),
      }
    }
  fn number_literal(&mut self) -> ParseResult<Expression> {
    match self.peek() {
      HexNumberSigil | BinNumberSigil => self.int_literal(),
      _ => self.float_literal(),
    }
  }

  fn int_literal(&mut self) -> ParseResult<Expression> {
    use self::Expression::*;
    match self.next() {
      BinNumberSigil => {
        let digits = self.digits()?;
        let n = parse_binary(digits)?;
        Ok(IntLiteral(n))
      },
      HexNumberSigil => {
        unimplemented!()
      },
      _ => return ParseError::new("Expected '0x' or '0b'"),
    }
  }

  fn float_literal(&mut self) -> ParseResult<Expression> {
    use self::Expression::*;
    let mut digits = self.digits()?;
    if let TokenType::Period = self.peek() {
      self.next();
      digits.push_str(".");
      digits.push_str(&self.digits()?);
      match digits.parse::<f64>() {
        Ok(f) => Ok(FloatLiteral(f)),
        Err(e) => unimplemented!("Float didn't parse with error: {}", e),

      }
    } else {
      match digits.parse::<u64>() {
        Ok(d) => Ok(IntLiteral(d)),
        Err(e) => unimplemented!("Need to handle numbers that don't parse to a Rust u64 {}", e),
      }
    }
  }

  fn digits(&mut self) -> ParseResult<String> {
    let mut ds = String::new();
    loop {
      match self.peek() {
        Underscore => { self.next(); continue; },
        DigitGroup(ref s) => { self.next(); ds.push_str(s)},
        _ => break,
      }
    }
    Ok(ds)
  }
}

fn parse_binary(digits: String) -> ParseResult<u64> {
  let mut result: u64 = 0;
  let mut multiplier = 1;
  for d in digits.chars().rev() {
    match d {
      '1' => result += multiplier,
      '0' => (),
      _ => return ParseError::new("Encountered a character not '1' or '0 while parsing a binary literal"),
    }
    multiplier *= 2;
  }
  Ok(result)
}

pub fn parse(input: Vec<Token>) -> Result<AST, ParseError> {
  let mut parser = Parser::new(input);
  parser.program()
}

#[cfg(test)]
mod parse_tests {
  use super::*;
  use super::Statement::*;
  use super::Expression::*;
  use super::ParseError;

  macro_rules! parse_test {
    ($string:expr, $correct:expr) => { assert_eq!(parse(tokenize($string)).unwrap(), $correct) }
  }
  macro_rules! binexp {
    ($op:expr, $lhs:expr, $rhs:expr) => { BinExp($op, Box::new($lhs), Box::new($rhs)) }
  }
  macro_rules! op {
    ($op:expr) => { Operation { op: Rc::new($op.to_string()) } }
  }

  #[test]
  fn test_parsing() {
    parse_test!("8.1", AST(vec![Expression(FloatLiteral(8.1))]));
    parse_test!("0b010", AST(vec![Expression(IntLiteral(2))]));
    parse_test!("3; 4; 4.3", AST(
      vec![Expression(IntLiteral(3)), Expression(IntLiteral(4)),
        Expression(FloatLiteral(4.3))]));

    parse_test!("1 + 2 * 3", AST(vec!
      [
        Expression(binexp!(op!("+"), IntLiteral(1), binexp!(op!("*"), IntLiteral(2), IntLiteral(3))))
      ]));

    parse_test!("1 * 2 + 3", AST(vec!
      [
        Expression(binexp!(op!("+"), binexp!(op!("*"), IntLiteral(1), IntLiteral(2)), IntLiteral(3)))
      ]));
  }
}
