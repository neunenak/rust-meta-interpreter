extern crate itertools;

use language::{TokenError, ParseError};
use std::rc::Rc;
use std::iter::{Enumerate, Peekable};
use self::itertools::Itertools;
use std::str::Chars;

#[allow(dead_code)]
#[derive(Debug)]
pub enum TokenType {
  Newline, Semicolon,

  LParen, RParen,
  LSquareBracket, RSquareBracket,
  LAngleBracket, RAngleBracket,
  LCurlyBrace, RCurlyBrace,

  Comma, Period, Colon, Underscore,

  Operator(Rc<String>),
  DigitGroup(Rc<String>), HexNumberSigil, BinNumberSigil,
  StrLiteral(Rc<String>),
  Identifier(Rc<String>),
  Keyword(Kw),

  Error(String),
}

#[derive(Debug)]
pub enum Kw {
  If,
  Else,
  Func,
  For,
  Loop,
}

#[derive(Debug)]
pub struct Token {
  token_type: TokenType,
  offset: usize,
}

fn is_digit(c: &char) -> bool {
  c.is_digit(10)
}

type CharIter<'a> = Peekable<Enumerate<Chars<'a>>>;

pub fn tokenize(input: &str) -> Result<Vec<Token>, TokenError> {
  use self::TokenType::*;

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
      c if char::is_whitespace(c) && c != '\n' => continue,
      '\n' => Newline, ';' => Semicolon,
      ':' => Colon, ',' => Comma, '.' => Period,
      '(' => LParen, ')' => RParen,
      '{' => LCurlyBrace, '}' => RCurlyBrace,
      '<' => LAngleBracket, '>' => RAngleBracket,
      '[' => LSquareBracket, ']' => RSquareBracket,
      '"' => handle_quote(&mut input),
      c if is_digit(&c) => handle_digit(c, &mut input),
      c @ '_' | c if c.is_alphabetic() => handle_alphabetic(c, &mut input),
      c => handle_operator(c, &mut input),
    };

    tokens.push(Token { token_type: cur_tok_type, offset: idx });
  }

  Ok(tokens)
}

fn handle_digit(c: char, input: &mut CharIter) -> TokenType {
  use self::TokenType::*;

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
  while let Some(c) = input.next().map(|(_, c)| { c }) {
    if c == '"' {
      break;
    } else if c == '\\' {
      let next = input.peek().map(|&(_, c)| { c });
      if next == Some('n') {
        input.next();
        buf.push('\n')
      } else if next == Some('"') {
        input.next();
        buf.push('"');
      }
      //TODO handle more escapes
    } else {
      buf.push(c);
    }
  }
  TokenType::StrLiteral(Rc::new(buf))
}

fn handle_alphabetic(c: char, input: &mut CharIter) -> TokenType {
  unimplemented!()
}

fn handle_operator(c: char, input: &mut CharIter) -> TokenType {
  unimplemented!()
}


/*
Schala EBNF grammar


 type alias <name> = <other type>
 
type <name> = struct { <field> : <type>,* }
type <name> = Variant1 | Variant2(type, type) | Variant3 struct { }


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

#[allow(dead_code)]
#[derive(Debug)]
pub struct AST { }

#[allow(dead_code)]
pub fn parse(_input: Vec<Token>) -> Result<AST, ParseError> {
  Ok(AST { })
}
