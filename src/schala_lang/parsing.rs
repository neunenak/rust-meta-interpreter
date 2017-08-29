use language::{TokenError, ParseError};
use std::rc::Rc;

#[derive(Debug)]
pub enum Token {
  Newline,
  Semicolon,
  LParen,
  RParen,
  LSquareBracket,
  RSquareBracket,
  LCurlyBrace,
  RCurlyBrace,
  Comma,
  Period,
  Colon,
  Digit(u8),
  StrLiteral(Rc<String>),
  Identifier(Rc<String>),
  Operator(Rc<String>),
}

pub fn tokenize(input: &str) -> Result<Vec<Token>, TokenError> {
  Ok(vec!())
}

/*
Schala grammar

program := (statement delimiter ?)*
delimiter := Newline | Semicolon
statement := declaration | expression
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


#[derive(Debug)]
pub struct AST { }

pub fn parse(input: Vec<Token>) -> Result<AST, ParseError> {
  Ok(AST { })
}
