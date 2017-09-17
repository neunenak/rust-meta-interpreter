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

const OPERATOR_CHARS: [char; 19] = ['!', '$', '%', '&', '*', '+', '-', '.', '/', ':', '<', '>', '=', '?', '@', '^', '|', '~', '`'];
fn is_operator(c: &char) -> bool {
  OPERATOR_CHARS.iter().any(|x| x == c)
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
      ':' => Colon, ',' => Comma,
      '(' => LParen, ')' => RParen,
      '{' => LCurlyBrace, '}' => RCurlyBrace,
      '[' => LSquareBracket, ']' => RSquareBracket,
      '"' => handle_quote(&mut input),
      c if is_digit(&c) => handle_digit(c, &mut input),
      c if c.is_alphabetic() || c == '_' => handle_alphabetic(c, &mut input), //TODO I'll probably have to rewrite this if I care about types being uppercase, also type parameterization
      c if is_operator(&c) => handle_operator(c, &mut input),
      unknown => Error(format!("Unexpected character: {}", unknown)),
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
  match c {
    '<' | '>' | '|' | '.' => {
      let ref next = input.peek().map(|&(_, c)| { c });
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
    match input.peek().map(|&(_, c)| { c }) {
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


/* for reference, here is the scala EBNF for expressions:
 * see http://scala-lang.org/files/archive/spec/2.12/06-expressions.html

Expr ::= (Bindings | id | ‘_’) ‘=>’ Expr
| Expr1
Expr1 ::= ‘if’ ‘(’ Expr ‘)’ {nl} Expr [[semi] else Expr]
| ‘while’ ‘(’ Expr ‘)’ {nl} Expr
| ‘try’ ‘{’ Block ‘}’ [‘catch’ ‘{’ CaseClauses ‘}’]
[‘finally’ Expr]
| ‘do’ Expr [semi] ‘while’ ‘(’ Expr ’)’
| ‘for’ (‘(’ Enumerators ‘)’ | ‘{’ Enumerators ‘}’)
{nl} [‘yield’] Expr
| ‘throw’ Expr
| ‘return’ [Expr]
| [SimpleExpr ‘.’] id ‘=’ Expr
| SimpleExpr1 ArgumentExprs ‘=’ Expr
| PostfixExpr
| PostfixExpr Ascription
| PostfixExpr ‘match’ ‘{’ CaseClauses ‘}’

PrefixExpr ::= [‘-’ | ‘+’ | ‘~’ | ‘!’] SimpleExpr

*/

/* Schala EBNF Grammar */
/* Terminal productions are in 'single quotes' or UPPERCASE if they are a class
 * or not representable in ASCII

program := (statement delimiter)* EOF
delimiter := NEWLINE | ';'
statement := expression | declaration

declaration := type_alias | type_declaration | func_declaration

type_alias := 'alias' IDENTIFIER '=' IDENTIFIER
type_declaration := 'type' IDENTIFIER '=' type_body
type_body := variant_specifier ('|' variant_specifier)*
variant_specifier := '{' member_list '}'
member_list := (IDENTIFIER type_anno)*

func_declaration := 'fn' IDENTIFIER '(' param_list ')'
param_list := (IDENTIFIER type_anno+ ',')*

type_anno := ':' type

expression := precedence_expr
precedence_expr := primary
primary := literal | paren_expr | identifier_expr

paren_expr := LParen expression RParen
identifier_expr := call_expr | index_expr | IDENTIFIER
literal := 'true' | 'false' | number_literal | STR_LITERAL

call_expr := IDENTIFIER '(' expr_list ')' //TODO maybe make this optional? or no, have a bare identifier meant to be used as method taken care of in eval
index_expr := '[' (expression (',' (expression)* | ε) ']'
expr_list := expression (',' expression)* | ε

// a float_literal can still be assigned to an int in type-checking
number_literal := int_literal | float_literal
int_literal = ('0x' | '0b') digits
float_literal := digits ('.' digits)
digits := (DIGIT_GROUP underscore)+

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

#[derive(Debug)]
pub struct ParseRecord {
  production_name: String,
  next_token: String,
}

struct Parser {
  tokens: TokenIter,
  parse_record: Vec<ParseRecord>,
}

impl Parser {
  fn new(input: Vec<Token>) -> Parser {
    Parser { tokens: input.into_iter().peekable(), parse_record: vec![] }
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
  };
  ($self:expr, $token_type:pat if $cond:expr, $message:expr) => {
    match $self.peek() {
      $token_type if $cond => $self.next(),
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

type ParamName = Rc<String>;
type TypeName = Rc<String>;
type FormalParamList = Vec<(ParamName, Option<TypeName>)>;

#[derive(Debug, PartialEq)]
pub enum Declaration {
  FuncDecl {
    name: Rc<String>,
    params: FormalParamList,
  },
  TypeDecl(Rc<String>, TypeBody),
  TypeAlias(Rc<String>, Rc<String>)
}

#[derive(Debug, PartialEq)]
pub struct TypeBody(Vec<Variant>);

#[derive(Debug, PartialEq)]
pub enum Variant {
  Singleton(Rc<String>),
  //ArgumentConstructor,
  //Record
}

#[derive(Debug, PartialEq)]
pub enum Expression {
  IntLiteral(u64),
  FloatLiteral(f64),
  StringLiteral(Rc<String>),
  BoolLiteral(bool),
  BinExp(Operation, Box<Expression>, Box<Expression>),
  Variable(Rc<String>),
  Call {
    name: Rc<String>,
    params: Vec<Expression>,
  },
  Index {
    indexee: Box<Expression>,
    indexers: Vec<Expression>,
  }
}

#[derive(Debug, PartialEq)]
pub struct Operation {
  op: Rc<String>
}

impl Operation {
  fn min_precedence() -> i32 {
    i32::min_value()
  }

  fn get_precedence(op: &str) -> i32 {
    let c: char = op.chars().next().unwrap();
    match c {
      '+' | '-' => 10,
      '*' | '/' | '%' => 20,
      _ => 30,
    }
  }
}

macro_rules! parse_method {
  ($name:ident(&mut $self:ident) -> $type:ty $body:block) => {
    fn $name(&mut $self) -> $type {
      let next_token = $self.peek();
      let record = ParseRecord {
        production_name: stringify!($name).to_string(),
        next_token: format!("{:?}", next_token),
      };
      $self.parse_record.push(record);
      $body
    }
  };
}

impl Parser {
  parse_method!(program(&mut self) -> ParseResult<AST> {
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
  });

  parse_method!(statement(&mut self) -> ParseResult<Statement> {
    //TODO handle error recovery here
    match self.peek() {
      Keyword(Alias) => self.type_alias().map(|alias| { Statement::Declaration(alias) }),
      Keyword(Type) => self.type_declaration().map(|decl| { Statement::Declaration(decl) }),
      Keyword(Func)=> self.func_declaration().map(|func| { Statement::Declaration(func) }),
      _ => self.expression().map(|expr| { Statement::Expression(expr) } ),
    }
  });

  parse_method!(type_alias(&mut self) -> ParseResult<Declaration> {
    expect!(self, Keyword(Alias), "Expected 'alias'");
    let alias = self.identifier()?;
    expect!(self, Operator(ref c) if **c == "=", "Expected '='");
    let original = self.identifier()?;
    Ok(Declaration::TypeAlias(alias, original))
  });

  parse_method!(type_declaration(&mut self) -> ParseResult<Declaration> {
    expect!(self, Keyword(Type), "Expected 'type'");
    let name = self.identifier()?;
    expect!(self, Operator(ref c) if **c == "=", "Expected '='");
    let body = self.type_body()?;
    Ok(Declaration::TypeDecl(name, body))
  });

  parse_method!(type_body(&mut self) -> ParseResult<TypeBody> {
    let variant = Variant::Singleton(self.identifier()?);
    Ok(TypeBody(vec!(variant)))
  });

  parse_method!(func_declaration(&mut self) -> ParseResult<Declaration> {
    expect!(self, Keyword(Func), "Expected 'fn'");
    let name = self.identifier()?;
    expect!(self, LParen, "Expected '('");
    let params = self.param_list()?;
    expect!(self, RParen, "Expected ')'");
    let decl = Declaration::FuncDecl {
      name: name,
      params: params
    };
    Ok(decl)
  });

  parse_method!(param_list(&mut self) -> ParseResult<FormalParamList> {
    Ok(vec!())
  });

  parse_method!(expression(&mut self) -> ParseResult<Expression> {
    self.precedence_expr(Operation::min_precedence())
  });

  // this implements Pratt parsing, see http://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/
  fn precedence_expr(&mut self, precedence: i32) -> ParseResult<Expression> {
    use self::Expression::*;

    let next_token = self.peek();
    let record = ParseRecord {
      production_name: "precedence_expr".to_string(),
      next_token: format!("{:?}", next_token),
    };
    self.parse_record.push(record);

    //TODO clean this up
    let mut lhs = self.primary()?;
    loop {
      let new_precedence = match self.peek() {
        Operator(op) => Operation::get_precedence(&*op),
        Period => Operation::get_precedence("."),
        _ => break,
      };

      if precedence >= new_precedence {
        break;
      }
      let op_str = match self.next() {
        Operator(op) => op,
        Period => Rc::new(".".to_string()),
        _ => unreachable!(),
      };
      let rhs = self.precedence_expr(new_precedence)?;
      let operation = Operation { op: op_str };
      lhs = BinExp(operation, Box::new(lhs), Box::new(rhs));
    }
    Ok(lhs)
  }

  parse_method!(primary(&mut self) -> ParseResult<Expression> {
    match self.peek() {
      LParen => self.paren_expr(),
      Identifier(_) => self.identifier_expr(),
      _ => self.literal(),
    }
  });

  parse_method!(paren_expr(&mut self) -> ParseResult<Expression> {
    expect!(self, LParen, "Expected '('");
    let expr = self.expression()?;
    expect!(self, RParen, "Expected ')'");
    Ok(expr)
  });

  fn identifier_expr(&mut self) -> ParseResult<Expression> {
    let identifier = self.identifier()?;
    match self.peek() {
      LParen => {
        let call_params = self.call_expr()?;
        Ok(Expression::Call {
          name: identifier,
          params: call_params,
        })
      },
      LSquareBracket => {
        let indexers = self.index_expr()?;
        Ok(Expression::Index {
          indexee: Box::new(Expression::Variable(identifier)),
          indexers: indexers,
        })
      }
      _ => Ok(Expression::Variable(identifier))
    }
  }

  parse_method!(call_expr(&mut self) -> ParseResult<Vec<Expression>> {
    let mut exprs = Vec::new();
    expect!(self, LParen, "Expected '('");
    loop {
      if let RParen = self.peek() {
        break;
      }
      exprs.push(self.expression()?);
      match self.peek() {
        Comma => { self.next(); },
        _ => break,
      }
    }
    expect!(self, RParen, "Expected ')'");
    Ok(exprs)
  });

  parse_method!(index_expr(&mut self) -> ParseResult<Vec<Expression>> {
    expect!(self, LSquareBracket, "Expected '['");
    let mut exprs = Vec::new();
    loop {
      if let RSquareBracket = self.peek() {
        break;
      }
      exprs.push(self.expression()?);
      match self.peek() {
        Comma => { self.next(); }
        _ => break,
      };
    }
    expect!(self, RSquareBracket, "Expected ']'");
    Ok(exprs)
  });

  parse_method!(identifier(&mut self) -> ParseResult<Rc<String>> {
    match self.next() {
      Identifier(s) => Ok(s),
      p => ParseError::new(&format!("Expected an identifier, got {:?}", p)),
    }
  });

  parse_method!(literal(&mut self) -> ParseResult<Expression> {
    match self.peek() {
      DigitGroup(_) | HexNumberSigil | BinNumberSigil | Period => self.number_literal(),
      Keyword(Kw::True) => { self.next(); Ok(Expression::BoolLiteral(true)) },
      Keyword(Kw::False) => { self.next(); Ok(Expression::BoolLiteral(false)) },
      StrLiteral(s) => {
        self.next();
        Ok(Expression::StringLiteral(s))
      }
      e => ParseError::new(&format!("Expected a literal expression, got {:?}", e)),
    }
  });

  parse_method!(number_literal(&mut self) -> ParseResult<Expression> {
    match self.peek() {
      HexNumberSigil | BinNumberSigil => self.int_literal(),
      _ => self.float_literal(),
    }
  });

  parse_method!(int_literal(&mut self) -> ParseResult<Expression> {
    use self::Expression::*;
    match self.next() {
      BinNumberSigil => {
        let digits = self.digits()?;
        let n = parse_binary(digits)?;
        Ok(IntLiteral(n))
      },
      HexNumberSigil => {
        ParseError::new("Not implemented")
      },
      _ => return ParseError::new("Expected '0x' or '0b'"),
    }
  });

  parse_method!(float_literal(&mut self) -> ParseResult<Expression> {
    use self::Expression::*;
    let mut digits = self.digits()?;
    if let TokenType::Period = self.peek() {
      self.next();
      digits.push_str(".");
      digits.push_str(&self.digits()?);
      match digits.parse::<f64>() {
        Ok(f) => Ok(FloatLiteral(f)),
        Err(e) => ParseError::new(&format!("Float failed to parse with error: {}", e)),

      }
    } else {
      match digits.parse::<u64>() {
        Ok(d) => Ok(IntLiteral(d)),
        Err(e) => ParseError::new(&format!("Integer failed to parse with error: {}", e)),
      }
    }
  });

  parse_method!(digits(&mut self) -> ParseResult<String> {
    let mut ds = String::new();
    loop {
      match self.peek() {
        Underscore => { self.next(); continue; },
        DigitGroup(ref s) => { self.next(); ds.push_str(s)},
        _ => break,
      }
    }
    Ok(ds)
  });
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

pub fn parse(input: Vec<Token>) -> (Result<AST, ParseError>, Vec<String>) {
  let mut parser = Parser::new(input);
  let ast = parser.program();

  let trace = parser.parse_record.into_iter().map(|r| {
    format!("Production `{}`, token: {:?}", r.production_name, r.next_token)
  }).collect();
  (ast, trace)
}

#[cfg(test)]
mod parse_tests {
  use super::*;
  use super::Statement::*;
  use super::Declaration::*;
  use super::Expression::*;

  macro_rules! rc {
    ($string:tt) => { Rc::new(stringify!($string).to_string()) }
  }

  macro_rules! parse_test {
    ($string:expr, $correct:expr) => { assert_eq!(parse(tokenize($string)).0.unwrap(), $correct) }
  }

  macro_rules! binexp {
    ($op:expr, $lhs:expr, $rhs:expr) => { BinExp($op, Box::new($lhs), Box::new($rhs)) }
  }
  macro_rules! op {
    ($op:expr) => { Operation { op: Rc::new($op.to_string()) } }
  }
  macro_rules! var {
    ($var:expr) => { Variable(Rc::new($var.to_string())) }
  }

  #[test]
  fn parsing_number_literals_and_binexps() {
    parse_test!(".2", AST(vec![Expression(FloatLiteral(0.2))]));
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

    parse_test!("1 && 2", AST(vec![Expression(binexp!(op!("&&"), IntLiteral(1), IntLiteral(2)))]));

    parse_test!("1 + 2 * 3 + 4", AST(vec![Expression(
          binexp!(op!("+"),
                  binexp!(op!("+"), IntLiteral(1),
                          binexp!(op!("*"), IntLiteral(2), IntLiteral(3))
                         ),
                  IntLiteral(4)
                 )
          )]));

    parse_test!("(1 + 2) * 3", AST(vec!
      [
        Expression(binexp!(op!("*"), binexp!(op!("+"), IntLiteral(1), IntLiteral(2)), IntLiteral(3)))
      ]));

    parse_test!(".1 + .2", AST(vec![Expression(binexp!(op!("+"), FloatLiteral(0.1), FloatLiteral(0.2)))]));
  }

  #[test]
  fn parsing_identifiers() {
    parse_test!("a", AST(vec![Expression(var!("a"))]));
    parse_test!("a + b", AST(vec![Expression(binexp!(op!("+"), var!("a"), var!("b")))]));
    //parse_test!("a[b]", AST(vec![Expression(
    //parse_test!("a[]", <- TODO THIS NEEDS TO FAIL
    //parse_test!(damn()[a] ,<- TODO needs to succeed
    parse_test!("a[b,c]", AST(vec![Expression(Index { indexee: Box::new(var!("a")), indexers: vec![var!("b"), var!("c")]} )]));     
  }

  #[test]
  fn parse_complicated_operators() {
    parse_test!("a <- b", AST(vec![Expression(binexp!(op!("<-"), var!("a"), var!("b")))]));
    parse_test!("a || b", AST(vec![Expression(binexp!(op!("||"), var!("a"), var!("b")))]));
    parse_test!("a<>b", AST(vec![Expression(binexp!(op!("<>"), var!("a"), var!("b")))]));
    parse_test!("a.b.c.d", AST(vec![Expression(binexp!(op!("."),
                                                binexp!(op!("."),
                                                  binexp!(op!("."), var!("a"), var!("b")),
                                                var!("c")),
                                               var!("d")))]));
  }

  #[test]
  fn parsing_functions() {
    parse_test!("fn oi()",  AST(vec![Declaration(FuncDecl { name: rc!(oi), params: vec![] })]));
    parse_test!("oi()", AST(vec![Expression(Call { name: rc!(oi), params: vec![] })]));
    parse_test!("oi(a, 2 + 2)", AST(vec![Expression(Call
    { name: rc!(oi),
      params: vec![var!("a"), binexp!(op!("+"), IntLiteral(2), IntLiteral(2))]
    })]));
  }

  #[test]
  fn parse_bools() {
    parse_test!("false", AST(vec![Expression(BoolLiteral(false))]));
    parse_test!("true", AST(vec![Expression(BoolLiteral(true))]));
  }

  #[test]
  fn parsing_strings() {
    parse_test!(r#""hello""#, AST(vec![Expression(StringLiteral(rc!(hello)))]));
  }

  #[test]
  fn parsing_types() {
    parse_test!("type Yolo = Yolo", AST(vec![Declaration(TypeDecl(rc!(Yolo), TypeBody(vec![Variant::Singleton(rc!(Yolo))])))]));
    parse_test!("alias Sex = Drugs", AST(vec![Declaration(TypeAlias(rc!(Sex), rc!(Drugs)))]));
  }
}
