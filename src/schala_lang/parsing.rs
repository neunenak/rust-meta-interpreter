use language::{TokenError, ParseError};
use std::rc::Rc;

#[allow(dead_code)]
#[derive(Debug)]
pub enum TokenType {
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
  Keyword(Kw),
  Operator(Rc<String>),
}

#[derive(Debug)]
pub enum Kw {
  If,
  Else,
  Func,
  Loop,
}

#[derive(Debug)]
pub struct Token {
  token_type: TokenType,
  line_number: u32,
  char_number: u32,
}

pub fn tokenize(_input: &str) -> Result<Vec<Token>, TokenError> {
  Ok(vec!())
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
