use std::rc::Rc;
use std::iter::Peekable;
use std::vec::IntoIter;

use tokenizing::*;
use tokenizing::Kw::*;
use tokenizing::TokenType::*;

use builtin::{BinOp, PrefixOp};

/* Schala EBNF Grammar */
/* Terminal productions are in 'single quotes' or UPPERCASE if they are a class
 * or not representable in ASCII

program := (statement delimiter)* EOF
delimiter := NEWLINE | ';'
statement := expression | declaration

declaration := type_declaration | func_declaration | binding_declaration | impl_declaration

type_declaration := 'type' type_declaration_body
type_declaration_body := 'alias' type_alias | type_singleton_name '=' type_body
type_alias := IDENTIFIER '=' type_name
type_body := variant_specifier ('|' variant_specifier)*

variant_specifier := IDENTIFIER | IDENTIFIER '{' typed_identifier_list '}' | IDENTIFIER '(' type_name* ')'
typed_identifier_list := typed_identifier*
typed_identifier := IDENTIFIER type_anno

func_declaration := func_signature func_body
func_body := ε | '{' (statement delimiter)* '}'
func_signature := 'fn' IDENTIFIER formal_param_list func_body
formal_param_list := '(' (formal_param ',')* ')'
formal_param := IDENTIFIER type_anno+

binding_declaration: 'var' IDENTIFIER '=' expression
                      | 'const' IDENTIFIER '=' expression

interface_declaration := 'interface' interface_name signature_block
impl_declaration := 'impl' IDENTIFIER decl_block | 'impl' interface_name 'for' IDENTIFIER decl_block

decl_block := '{' (func_declaration)* '}'
signature_block := '{' (func_signature)* '}'

interface_name := IDENTIFIER

type_anno := (':' type_name)+
type_name := type_singleton_name | '(' type_names ')'
type_names := ε | type_name (, type_name)*
type_singleton_name = IDENTIFIER (type_params)*
type_params := '<' type_name (, type_name)* '>'

expression := precedence_expr type_anno+
precedence_expr := prefix_expr
prefix_expr := prefix_op call_expr
prefix_op := '+' | '-' | '!' | '~'
call_expr := index_expr ( '(' expr_list ')' )*
index_expr := primary ( '[' (expression (',' (expression)* | ε) ']' )*
primary := literal | paren_expr | if_expr | match_expr | for_expr | identifier_expr | curly_brace_expr | list_expr
curly_brace_expr := lambda_expr | anonymous_struct //TODO
list_expr := '[' (expression, ',')* ']'
lambda_expr := '{' '|' (formal_param ',')* '|' (type_anno)* (statement)* '}'
paren_expr := LParen paren_inner RParen
paren_inner := (expression ',')*
identifier_expr := named_struct | IDENTIFIER

literal := 'true' | 'false' | number_literal | STR_LITERAL

named_struct := IDENTIFIER record_block
record_block :=  '{' (record_entry, ',')* | '}' //TODO support anonymus structs, update syntax
record_entry := IDENTIFIER ':' expression
anonymous_struct := TODO

if_expr := 'if' expression block else_clause
else_clause := ε | 'else' block

match_expr := 'match' expression match_body
match_body := '{' (match_arm)* '}'
match_arm := pattern '=>' expression
pattern := identifier //TODO NOT DONE

block := '{' (statement)* '}'

expr_list := expression (',' expression)* | ε

for_expr := 'for' ... ????

// a float_literal can still be assigned to an int in type-checking
number_literal := int_literal | float_literal
int_literal = ('0x' | '0b') digits
float_literal := digits ('.' digits)
digits := (DIGIT_GROUP underscore)+

*/

type TokenIter = Peekable<IntoIter<Token>>;

#[derive(Debug, PartialEq)]
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
  level: u32,
}

struct Parser {
  tokens: TokenIter,
  parse_record: Vec<ParseRecord>,
  parse_level: u32,
  restrictions: ParserRestrictions,
}

struct ParserRestrictions {
  no_struct_literal: bool
}

impl Parser {
  fn new(input: Vec<Token>) -> Parser {
    Parser {
      tokens: input.into_iter().peekable(),
      parse_record: vec![],
      parse_level: 0,
      restrictions: ParserRestrictions { no_struct_literal: false }
    }
  }

  fn peek(&mut self) -> TokenType {
    self.tokens.peek().map(|ref t| { t.token_type.clone() }).unwrap_or(TokenType::EOF)
  }
  fn peek_with_token_offset(&mut self) -> Token {
    self.tokens.peek().map(|t: &Token| { t.clone()}).unwrap_or(Token { token_type: TokenType::EOF, offset: (0,0)})
  }
  fn next(&mut self) -> TokenType {
    self.tokens.next().map(|ref t| { t.token_type.clone() }).unwrap_or(TokenType::EOF)
  }
}

macro_rules! expect {
  ($self:expr, $token_type:pat, $expected_item:expr) => { expect!($self, $token_type if true, $expected_item) };
  ($self:expr, $token_type:pat if $cond:expr, $expected_item:expr) => {
    match $self.peek() {
      $token_type if $cond => $self.next(),
      tok => {
        let msg = format!("Expected {}, got {:?}", $expected_item, tok);
        return Err(ParseError { msg })
      }
    }
  }
}

#[derive(Debug, PartialEq)]
pub struct AST(pub Vec<Statement>);

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
  ExpressionStatement(Expression),
  Declaration(Declaration),
}

type Block = Vec<Statement>;

type ParamName = Rc<String>;
type InterfaceName = Rc<String>; //should be a singleton I think??
type FormalParam = (ParamName, Option<TypeName>);

#[derive(Debug, PartialEq, Clone)]
pub enum Declaration {
  FuncSig(Signature),
  FuncDecl(Signature, Block),
  TypeDecl(TypeSingletonName, TypeBody), //should have TypeSingletonName in it
  TypeAlias(Rc<String>, Rc<String>), //should have TypeSingletonName in it, or maybe just String, not sure
  Binding {
    name: Rc<String>,
    constant: bool,
    expr: Expression,
  },
  Impl {
    type_name: TypeName,
    interface_name: Option<InterfaceName>,
    block: Vec<Declaration>,
  },
  Interface {
    name: Rc<String>,
    signatures: Vec<Signature>
  }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Signature {
  pub name: Rc<String>,
  pub params: Vec<FormalParam>,
  pub type_anno: Option<TypeName>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypeBody(pub Vec<Variant>);

#[derive(Debug, PartialEq, Clone)]
pub enum Variant {
  UnitStruct(Rc<String>),
  TupleStruct(Rc<String>, Vec<TypeName>),
  Record(Rc<String>, Vec<(Rc<String>, TypeName)>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Expression(pub ExpressionType, pub Option<TypeName>);

#[derive(Debug, PartialEq, Clone)]
pub enum TypeName {
  Tuple(Vec<TypeName>),
  Singleton(TypeSingletonName)
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypeSingletonName {
  pub name: Rc<String>,
  pub params: Vec<TypeName>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExpressionType {
  IntLiteral(u64),
  FloatLiteral(f64),
  StringLiteral(Rc<String>),
  BoolLiteral(bool),
  BinExp(BinOp, Box<Expression>, Box<Expression>),
  PrefixExp(PrefixOp, Box<Expression>),
  TupleLiteral(Vec<Expression>),
  Value(Rc<String>),
  NamedStruct {
    name: Rc<String>,
    fields: Vec<(Rc<String>, Expression)>,
  },
  Call {
    f: Box<Expression>,
    arguments: Vec<Expression>,
  },
  Index {
    indexee: Box<Expression>,
    indexers: Vec<Expression>,
  },
  IfExpression(Box<Expression>, Block, Option<Block>),
  MatchExpression(Box<Expression>, Vec<MatchArm>),
  ForExpression,
  Lambda {
    params: Vec<FormalParam>,
    body: Block,
  },
  ListLiteral(Vec<Expression>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct MatchArm {
  pat: Pattern,
  expr: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Pattern(Rc<String>);

macro_rules! parse_method {
  ($name:ident(&mut $self:ident) -> $type:ty $body:block) => {
    fn $name(&mut $self) -> $type {
      let next_token = $self.peek_with_token_offset();
      let record = ParseRecord {
        production_name: stringify!($name).to_string(),
        next_token: format!("{}", next_token.to_string_with_metadata()),
        level: $self.parse_level,
      };
      $self.parse_level += 1;
      $self.parse_record.push(record);
      let result = { $body };

      if $self.parse_level != 0 {
        $self.parse_level -= 1;
      }
      result
    }
  };
}

macro_rules! delimited {
  ($self:expr, $start:pat, $start_str:expr, $parse_fn:ident, $( $delim:pat )|+, $end:pat, $end_str:expr, nonstrict) => {
    delimited!($self, $start, $start_str, $parse_fn, $( $delim )|*, $end, $end_str, false)
  };
  ($self:expr, $start:pat, $start_str:expr, $parse_fn:ident, $( $delim:pat )|+, $end:pat, $end_str:expr) => {
    delimited!($self, $start, $start_str, $parse_fn, $( $delim )|*, $end, $end_str, true)
  };
  ($self:expr, $start:pat, $start_str:expr, $parse_fn:ident, $( $delim:pat )|+, $end:pat, $end_str:expr, $strictness:expr) => {
    {
      expect!($self, $start, $start_str);
      let mut acc = vec![];
      loop {
        let peek = $self.peek();
        match peek {
          $end | EOF => break,
          _ => (),
        }
        if !$strictness {
          match peek {
            $( $delim )|* => { $self.next(); continue },
            _ => ()
          }
        }
        acc.push($self.$parse_fn()?);
        match $self.peek() {
          $( $delim )|* => { $self.next(); continue },
          _ if $strictness => break,
          _ => continue,
        };
      }
      expect!($self, $end, $end_str);
      acc
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
      Keyword(Type) => self.type_declaration().map(|decl| { Statement::Declaration(decl) }),
      Keyword(Func)=> self.func_declaration().map(|func| { Statement::Declaration(func) }),
      Keyword(Var) | Keyword(Const) => self.binding_declaration().map(|decl| Statement::Declaration(decl)),
      Keyword(Interface) => self.interface_declaration().map(|decl| Statement::Declaration(decl)),
      Keyword(Impl) => self.impl_declaration().map(|decl| Statement::Declaration(decl)),
      _ => self.expression().map(|expr| { Statement::ExpressionStatement(expr) } ),
    }
  });

  parse_method!(type_declaration(&mut self) -> ParseResult<Declaration> {
    expect!(self, Keyword(Type), "'type'");
    self.type_declaration_body()
  });

  parse_method!(type_declaration_body(&mut self) -> ParseResult<Declaration> {
    if let Keyword(Alias) = self.peek() {
      self.type_alias()
    } else {
      let name = self.type_singleton_name()?;
      expect!(self, Operator(ref c) if **c == "=", "'='");
      let body = self.type_body()?;
      Ok(Declaration::TypeDecl(name, body))
    }
  });

  parse_method!(type_alias(&mut self) -> ParseResult<Declaration> {
    expect!(self, Keyword(Alias), "'alias'");
    let alias = self.identifier()?;
    expect!(self, Operator(ref c) if **c == "=", "'='");
    let original = self.identifier()?;
    Ok(Declaration::TypeAlias(alias, original))
  });

  parse_method!(type_body(&mut self) -> ParseResult<TypeBody> {
    let mut variants = Vec::new();
    variants.push(self.variant_specifier()?);
    loop {
      if let Pipe = self.peek() {
        self.next();
        variants.push(self.variant_specifier()?);
      } else {
        break;
      }
    }
    Ok(TypeBody(variants))
  });

  parse_method!(variant_specifier(&mut self) -> ParseResult<Variant> {
    use self::Variant::*;

    let name = self.identifier()?;
    match self.peek() {
      LParen => {
        let tuple_members = delimited!(self, LParen, '(', type_name, Comma, RParen, ')');
        Ok(TupleStruct(name, tuple_members))
      },
      LCurlyBrace => {
        let typed_identifier_list = delimited!(self, LCurlyBrace, '{', typed_identifier, Comma, RCurlyBrace, '}');
        Ok(Record(name, typed_identifier_list))
      },
      _ => Ok(UnitStruct(name))
    }
  });

  parse_method!(typed_identifier(&mut self) -> ParseResult<(Rc<String>, TypeName)> {
    let identifier = self.identifier()?;
    expect!(self, Colon, "':'");
    let type_name = self.type_name()?;
    Ok((identifier, type_name))
  });

  parse_method!(func_declaration(&mut self) -> ParseResult<Declaration> {
    let signature = self.signature()?;
    if let LCurlyBrace = self.peek() {
      let statements = delimited!(self, LCurlyBrace, '{', statement, Newline | Semicolon, RCurlyBrace, '}', nonstrict);
      Ok(Declaration::FuncDecl(signature, statements))
    } else {
      Ok(Declaration::FuncSig(signature))
    }
  });

  parse_method!(signature(&mut self) -> ParseResult<Signature> {
    expect!(self, Keyword(Func), "'fn'");
    let name = self.identifier()?;
    let params = delimited!(self, LParen, '(', formal_param, Comma, RParen, ')');
    let type_anno = match self.peek() {
      Colon => Some(self.type_anno()?),
      _ => None,
    };
    Ok(Signature { name, params, type_anno })
  });

  parse_method!(formal_param(&mut self) -> ParseResult<FormalParam> {
    let name = self.identifier()?;
    let ty = match self.peek() {
      Colon => Some(self.type_anno()?),
      _ => None
    };
    Ok((name, ty))
  });

  parse_method!(binding_declaration(&mut self) -> ParseResult<Declaration> {
    let constant = match self.next() {
      Keyword(Var) => false,
      Keyword(Const) => true,
      _ => return ParseError::new("Expected 'var' or 'const'"),
    };
    let name = self.identifier()?;
    expect!(self, Operator(ref o) if **o == "=", "'='");
    let expr = self.expression()?;

    Ok(Declaration::Binding { name, constant, expr })
  });

  parse_method!(interface_declaration(&mut self) -> ParseResult<Declaration> {
    expect!(self, Keyword(Interface), "'interface'");
    let name = self.identifier()?;
    let signatures = self.signature_block()?;
    Ok(Declaration::Interface { name, signatures })
  });

  parse_method!(signature_block(&mut self) -> ParseResult<Vec<Signature>> {
    Ok(delimited!(self, LCurlyBrace, '{', signature, Newline | Semicolon, RCurlyBrace, '}', nonstrict))
  });

  parse_method!(impl_declaration(&mut self) -> ParseResult<Declaration> {
    expect!(self, Keyword(Impl), "'impl'");
    let first = self.type_name()?;
    let second = if let Keyword(For) = self.peek() {
      self.next();
      Some(self.type_name()?)
    } else {
      None
    };

    let block = self.decl_block()?;

    let result = match (first, second) {
      (first, Some(second)) => {
        match first {
          TypeName::Singleton(TypeSingletonName { ref name, ref params }) if params.len() == 0 =>
            Declaration::Impl { type_name: second, interface_name: Some(name.clone()), block },
          _ => return ParseError::new(&format!("Invalid name for an interface")),
        }
      },
      (first, None) => Declaration::Impl { type_name: first, interface_name: None, block }
    };
    Ok(result)
  });

  parse_method!(decl_block(&mut self) -> ParseResult<Vec<Declaration>> {
    Ok(delimited!(self, LCurlyBrace, '{', func_declaration, Newline | Semicolon, RCurlyBrace, '}', nonstrict))
  });

  parse_method!(expression(&mut self) -> ParseResult<Expression> {
    let mut expr_body = self.precedence_expr(BinOp::min_precedence())?;
    let type_anno = match self.peek() {
      Colon => Some(self.type_anno()?),
      _ => None
    };
    if let Some(_) = expr_body.1 {
      return ParseError::new("Bad parse state");
    }
    expr_body.1 = type_anno;
    Ok(expr_body)
  });

  parse_method!(type_anno(&mut self) -> ParseResult<TypeName> {
    expect!(self, Colon, "':'");
    self.type_name()
  });

  parse_method!(type_name(&mut self) -> ParseResult<TypeName> {
    use self::TypeName::*;
    Ok(match self.peek() {
      LParen => Tuple(delimited!(self, LParen, '(', type_name, Comma, RParen, ')')),
      _ => Singleton(self.type_singleton_name()?),
    })
  });

  parse_method!(type_singleton_name(&mut self) -> ParseResult<TypeSingletonName> {
    Ok(TypeSingletonName {
      name: self.identifier()?,
      params: match self.peek() {
        LAngleBracket => delimited!(self, LAngleBracket, '<', type_name, Comma, RAngleBracket, '>'),
        _ => vec![],
      }
    })
  });

  // this implements Pratt parsing, see http://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/
  fn precedence_expr(&mut self, precedence: i32) -> ParseResult<Expression> {
    let record = ParseRecord {
      production_name: "precedence_expr".to_string(),
      next_token: format!("{}", self.peek_with_token_offset().to_string_with_metadata()),
      level: self.parse_level,
    };
    self.parse_level += 1;
    self.parse_record.push(record);

    let mut lhs = self.prefix_expr()?;
    loop {
      let new_precedence = match self.peek() {
        Operator(op) => BinOp::get_precedence(&*op),
        Period => BinOp::get_precedence("."),
        Pipe => BinOp::get_precedence("|"),
        Slash => BinOp::get_precedence("/"),
        _ => break,
      };

      if precedence >= new_precedence {
        break;
      }
      let sigil = match self.next() {
        Operator(op) => op,
        Period => Rc::new(".".to_string()),
        Pipe => Rc::new("|".to_string()),
        _ => unreachable!(),
      };
      let rhs = self.precedence_expr(new_precedence)?;
      let operation = BinOp::from_sigil(sigil.as_ref());
      lhs = Expression(ExpressionType::BinExp(operation, bx!(lhs), bx!(rhs)), None);
    }
    self.parse_level -= 1;
    Ok(lhs)
  }

  parse_method!(prefix_expr(&mut self) -> ParseResult<Expression> {
    match self.peek() {
      Operator(ref op) if PrefixOp::is_prefix(&*op) => {
        let sigil = match self.next() {
          Operator(op) => op,
          _ => unreachable!(),
        };
        let expr = self.primary()?;
        Ok(Expression(
            ExpressionType::PrefixExp(PrefixOp::from_sigil(sigil.as_str()), bx!(expr)),
            None))
      },
      _ => self.call_expr()
    }
  });

  parse_method!(call_expr(&mut self) -> ParseResult<Expression> {
    let index  = self.index_expr()?;
    Ok(if let LParen = self.peek() {
      let arguments = delimited!(self, LParen, ')', expression, Comma, RParen, '(');
      Expression(ExpressionType::Call { f: bx!(index), arguments }, None) //TODO fix this none
    } else {
      index
    })
  });

  parse_method!(index_expr(&mut self) -> ParseResult<Expression> {
    let primary = self.primary()?;
    Ok(if let LSquareBracket = self.peek() {
      let indexers = delimited!(self, LSquareBracket, '[', expression, Comma, RSquareBracket, ']');
      Expression(ExpressionType::Index {
        indexee: bx!(Expression(primary.0, None)),
        indexers,
      }, None)
    } else {
      primary
    })
  });

  parse_method!(primary(&mut self) -> ParseResult<Expression> {
    match self.peek() {
      LCurlyBrace => self.curly_brace_expr(),
      LParen => self.paren_expr(),
      LSquareBracket => self.list_expr(),
      Keyword(Kw::If) => self.if_expr(),
      Keyword(Kw::Match) => self.match_expr(),
      Keyword(Kw::For) => self.for_expr(),
      Identifier(_) => self.identifier_expr(),
      _ => self.literal(),
    }
  });

  parse_method!(list_expr(&mut self) -> ParseResult<Expression> {
    let exprs = delimited!(self, LSquareBracket, '[', expression, Comma, RSquareBracket, ']');
    Ok(Expression(ExpressionType::ListLiteral(exprs), None))
  });

  parse_method!(curly_brace_expr(&mut self) -> ParseResult<Expression> {
    self.lambda_expr()
  });

  parse_method!(lambda_expr(&mut self) -> ParseResult<Expression> {
    expect!(self, LCurlyBrace, "{");
    let params = delimited!(self, Pipe, '|', formal_param, Comma, Pipe, '|');
    let mut body = Vec::new();
    loop {
      match self.peek() {
        EOF | RCurlyBrace => break,
        Newline | Semicolon => {
          self.next();
          continue;
        },
        _ => body.push(self.statement()?),
      }
    }
    expect!(self, RCurlyBrace, "}");
    Ok(Expression(ExpressionType::Lambda { params, body }, None)) //TODO need to handle types somehow
  });

  parse_method!(paren_expr(&mut self) -> ParseResult<Expression> {
    use self::ExpressionType::*;
    let old_struct_value = self.restrictions.no_struct_literal;
    self.restrictions.no_struct_literal = false;
    let output = (|| {
      let mut inner = delimited!(self, LParen, '(', expression, Comma, RParen, ')');
      match inner.len() {
        0 => Ok(Expression(TupleLiteral(vec![]), None)),
        1 => Ok(inner.pop().unwrap()),
        _ => Ok(Expression(TupleLiteral(inner), None)),
      }
    })();
    self.restrictions.no_struct_literal = old_struct_value;
    output
  });

  parse_method!(identifier_expr(&mut self) -> ParseResult<Expression> {
    use self::ExpressionType::*;
    let identifier = self.identifier()?;
    Ok(match self.peek() {
      LCurlyBrace if !self.restrictions.no_struct_literal => {
        let fields = self.record_block()?;
        Expression(NamedStruct { name: identifier, fields }, None)
      },
      _ => Expression(Value(identifier), None)
    })
  });

  parse_method!(record_block(&mut self) -> ParseResult<Vec<(Rc<String>, Expression)>> {
    Ok(delimited!(self, LCurlyBrace, '{', record_entry, Comma, RCurlyBrace, '}'))
  });

  parse_method!(record_entry(&mut self) -> ParseResult<(Rc<String>, Expression)> {
    let field_name = self.identifier()?;
    expect!(self, Colon, ":");
    let value = self.expression()?;
    Ok((field_name, value))
  });

  parse_method!(if_expr(&mut self) -> ParseResult<Expression> {
    expect!(self, Keyword(Kw::If), "'if'");
    let condition = (|| {
      self.restrictions.no_struct_literal = true;
      let x = self.expression();
      self.restrictions.no_struct_literal = false;
      x
    })()?;
    let then_clause = self.block()?;
    let else_clause = self.else_clause()?;
    Ok(Expression(ExpressionType::IfExpression(bx!(condition), then_clause, else_clause), None))
  });

  parse_method!(else_clause(&mut self) -> ParseResult<Option<Block>> {
    Ok(if let Keyword(Kw::Else) = self.peek() {
      self.next();
      Some(self.block()?)
    } else {
      None
    })
  });

  parse_method!(block(&mut self) -> ParseResult<Block> {
    Ok(delimited!(self, LCurlyBrace, '{', statement, Newline | Semicolon, RCurlyBrace, '}', nonstrict))
  });

  parse_method!(match_expr(&mut self) -> ParseResult<Expression> {
    expect!(self, Keyword(Kw::Match), "'match'");
    let expr = self.expression()?;
    //TODO abstract these errors into the delimited macro
    //expect!(self, LCurlyBrace, "Expected '{'");
    let body = self.match_body()?;
    //expect!(self, RCurlyBrace, "Expected '}'");
    Ok(Expression(ExpressionType::MatchExpression(bx!(expr), body), None))
  });

  parse_method!(match_body(&mut self) -> ParseResult<Vec<MatchArm>> {
    Ok(delimited!(self, LCurlyBrace, '{', match_arm, Comma, RCurlyBrace, '}'))
  });

  parse_method!(match_arm(&mut self) -> ParseResult<MatchArm> {
    let pat = self.pattern()?;
    expect!(self, Operator(ref c) if **c == "=>", "'=>'");
    let expr = self.expression()?;
    Ok(MatchArm { pat, expr })
  });

  parse_method!(pattern(&mut self) -> ParseResult<Pattern> {
    let identifier = self.identifier()?;
    Ok(Pattern(identifier))
  });

  parse_method!(for_expr(&mut self) -> ParseResult<Expression> {
    expect!(self, Keyword(Kw::For), "'for'");
    Ok(Expression(ExpressionType::ForExpression, None))
  });

  parse_method!(identifier(&mut self) -> ParseResult<Rc<String>> {
    match self.next() {
      Identifier(s) => Ok(s),
      p => ParseError::new(&format!("Expected an identifier, got {:?}", p)),
    }
  });

  parse_method!(literal(&mut self) -> ParseResult<Expression> {
    use self::ExpressionType::*;
    match self.peek() {
      DigitGroup(_) | HexLiteral(_) | BinNumberSigil | Period => self.number_literal(),
      Keyword(Kw::True) => {
        self.next();
        Ok(Expression(BoolLiteral(true), None))
      },
      Keyword(Kw::False) => {
        self.next();
        Ok(Expression(BoolLiteral(false), None))
      },
      StrLiteral(s) => {
        self.next();
        Ok(Expression(StringLiteral(s), None))
      }
      e => ParseError::new(&format!("Expected a literal expression, got {:?}", e)),
    }
  });

  parse_method!(number_literal(&mut self) -> ParseResult<Expression> {
    match self.peek() {
      HexLiteral(_) | BinNumberSigil => self.int_literal(),
      _ => self.float_literal(),
    }
  });

  parse_method!(int_literal(&mut self) -> ParseResult<Expression> {
    use self::ExpressionType::*;
    match self.next() {
      BinNumberSigil => {
        let digits = self.digits()?;
        let n = parse_binary(digits)?;
        Ok(Expression(IntLiteral(n), None))
      },
      HexLiteral(text) => {
        let digits: String = text.chars().filter(|c| c.is_digit(16)).collect();
        let n = parse_hex(digits)?;
        Ok(Expression(IntLiteral(n), None))
      },
      _ => return ParseError::new("Expected '0x' or '0b'"),
    }
  });

  parse_method!(float_literal(&mut self) -> ParseResult<Expression> {
    use self::ExpressionType::*;
    let mut digits = self.digits()?;
    if let TokenType::Period = self.peek() {
      self.next();
      digits.push_str(".");
      digits.push_str(&self.digits()?);
      match digits.parse::<f64>() {
        Ok(f) => Ok(Expression(FloatLiteral(f), None)),
        Err(e) => ParseError::new(&format!("Float failed to parse with error: {}", e)),

      }
    } else {
      match digits.parse::<u64>() {
        Ok(d) => Ok(Expression(IntLiteral(d), None)),
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
    multiplier = match multiplier.checked_mul(2) {
      Some(m) => m,
      None => return ParseError::new("This binary expression will overflow")
    }
  }
  Ok(result)
}

fn parse_hex(digits: String) -> ParseResult<u64> {
  let mut result: u64 = 0;
  let mut multiplier: u64 = 1;
  for d in digits.chars().rev() {
    match d.to_digit(16) {
      Some(n) => result += n as u64 * multiplier,
      None => return ParseError::new("Encountered a non-hex digit in a hex literal"),
    }
    multiplier = match multiplier.checked_mul(16) {
      Some(m) => m,
      None => return ParseError::new("This hex expression will overflow")
    }
  }
  Ok(result)
}

pub fn parse(input: Vec<Token>) -> (Result<AST, ParseError>, Vec<String>) {
  let mut parser = Parser::new(input);
  let ast = parser.program();

  let trace = parser.parse_record.into_iter().map(|r| {
    let mut indent = String::new();
    for _ in 0..r.level {
      indent.push(' ');
    }
    format!("{}Production `{}`, token: {}", indent, r.production_name, r.next_token)
  }).collect();
  (ast, trace)
}

#[cfg(test)]
mod parse_tests {
  use ::std::rc::Rc;
  use super::{AST, Expression, Statement, PrefixOp, BinOp, TypeBody, Variant, parse, tokenize};
  use super::Statement::*;
  use super::Declaration::*;
  use super::Signature;
  use super::TypeName::*;
  use super::TypeSingletonName;
  use super::ExpressionType::*;
  use super::Variant::*;

  macro_rules! rc {
    ($string:tt) => { Rc::new(stringify!($string).to_string()) }
  }
  macro_rules! parse_test {
    ($string:expr, $correct:expr) => { assert_eq!(parse(tokenize($string)).0.unwrap(), $correct) }
  }
  macro_rules! parse_error {
    ($string:expr) => { assert!(parse(tokenize($string)).0.is_err()) }
  }
  macro_rules! val {
    ($var:expr) => { Value(Rc::new($var.to_string())) }
  }
  macro_rules! exprstatement {
    ($expr_type:expr) => { Statement::ExpressionStatement(Expression($expr_type, None)) };
    ($expr_type:expr, $type_anno:expr) => { Statement::ExpressionStatement(Expression($expr_type, Some($type_anno))) };
  }
  macro_rules! ty {
    ($name:expr) => { Singleton(tys!($name)) }
  }
  macro_rules! tys {
    ($name:expr) => { TypeSingletonName { name: Rc::new($name.to_string()), params: vec![] } };
  }


  /* new style of test macros */

  macro_rules! single_expr {
    ($exprtype:expr) => { AST(vec![Statement::ExpressionStatement(Expression($exprtype, None))]) };
    ($exprtype:expr, $type:expr) => { AST(vec![Statement::ExpressionStatement(Expression($exprtype, $type))]) }
  }
  macro_rules! ex {
    ($expr_type:expr) => { Expression($expr_type, None) }
  }
  macro_rules! binexp {
    ($op:expr, $lhs:expr, $rhs:expr) => { BinExp(BinOp::from_sigil($op), bx!(Expression($lhs, None)), bx!(Expression($rhs, None))) }
  }
  macro_rules! prefexp {
    ($op:expr, $lhs:expr) => { PrefixExp(PrefixOp::from_sigil($op), bx!(Expression($lhs, None))) }
  }
  macro_rules! exst {
    ($expr_type:expr) => { Statement::ExpressionStatement(Expression($expr_type, None)) };
    ($expr_type:expr, $type_anno:expr) => { Statement::ExpressionStatement(Expression($expr_type, Some($type_anno))) };
    ($op:expr, $lhs:expr, $rhs:expr) => { Statement::ExpressionStatement(ex!(binexp!($op, $lhs, $rhs))) };
  }

  #[test]
  fn parsing_number_literals_and_binexps() {
    parse_test! { ".2", single_expr!(FloatLiteral(0.2)) };
    parse_test! { "8.1", single_expr!(FloatLiteral(8.1)) };

    parse_test! { "0b010", single_expr!(IntLiteral(2)) };
    parse_test! { "0b0_1_0_", single_expr!(IntLiteral(2)) }

    parse_test! {"0xff", single_expr!(IntLiteral(255)) };
    parse_test! {"0xf_f_", single_expr!(IntLiteral(255)) };

    parse_test!("0xf_f_+1", AST(vec![exprstatement!(binexp!("+", IntLiteral(255), IntLiteral(1)))]));

    parse_test! {"3; 4; 4.3", AST(
      vec![exprstatement!(IntLiteral(3)), exprstatement!(IntLiteral(4)),
        exprstatement!(FloatLiteral(4.3))])
    };

    parse_test!("1 + 2 * 3", AST(vec!
      [
        exprstatement!(binexp!("+", IntLiteral(1), binexp!("*", IntLiteral(2), IntLiteral(3))))
      ]));

    parse_test!("1 * 2 + 3", AST(vec!
      [
        exprstatement!(binexp!("+", binexp!("*", IntLiteral(1), IntLiteral(2)), IntLiteral(3)))
      ]));

    parse_test!("1 && 2", AST(vec![exprstatement!(binexp!("&&", IntLiteral(1), IntLiteral(2)))]));

    parse_test!("1 + 2 * 3 + 4", AST(vec![exprstatement!(
      binexp!("+",
        binexp!("+", IntLiteral(1), binexp!("*", IntLiteral(2), IntLiteral(3))),
        IntLiteral(4)))]));

    parse_test!("(1 + 2) * 3", AST(vec!
      [exprstatement!(binexp!("*", binexp!("+", IntLiteral(1), IntLiteral(2)), IntLiteral(3)))]));

    parse_test!(".1 + .2", AST(vec![exprstatement!(binexp!("+", FloatLiteral(0.1), FloatLiteral(0.2)))]));
  }

  #[test]
  fn parsing_tuples() {
    parse_test!("()", AST(vec![exprstatement!(TupleLiteral(vec![]))]));
    parse_test!("(\"hella\", 34)", AST(vec![exprstatement!(
      TupleLiteral(
        vec![ex!(StringLiteral(rc!(hella))), ex!(IntLiteral(34))]
      )
    )]));
    parse_test!("((1+2), \"slough\")", AST(vec![exprstatement!(TupleLiteral(vec![
      ex!(binexp!("+", IntLiteral(1), IntLiteral(2))),
      ex!(StringLiteral(rc!(slough))),
    ]))]))
  }

  #[test]
  fn parsing_identifiers() {
    parse_test!("a", AST(vec![exprstatement!(val!("a"))]));
    parse_test!("a + b", AST(vec![exprstatement!(binexp!("+", val!("a"), val!("b")))]));
    //parse_test!("a[b]", AST(vec![Expression(
    //parse_test!("a[]", <- TODO THIS NEEDS TO FAIL
    //parse_test!(damn()[a] ,<- TODO needs to succeed
    parse_test!("a[b,c]", AST(vec![exprstatement!(Index { indexee: bx!(ex!(val!("a"))), indexers: vec![ex!(val!("b")), ex!(val!("c"))]} )]));     

    parse_test!("None", AST(vec![exprstatement!(val!("None"))]));
    parse_test!("Pandas {  a: x + y }", AST(vec![
      exprstatement!(NamedStruct { name: rc!(Pandas), fields: vec![(rc!(a), ex!(binexp!("+", val!("x"), val!("y"))))]})
    ]));
  }

  #[test]
  fn parsing_complicated_operators() {
    parse_test!("a <- b", AST(vec![exprstatement!(binexp!("<-", val!("a"), val!("b")))]));
    parse_test!("a || b", AST(vec![exprstatement!(binexp!("||", val!("a"), val!("b")))]));
    parse_test!("a<>b", AST(vec![exprstatement!(binexp!("<>", val!("a"), val!("b")))]));
    parse_test!("a.b.c.d", AST(vec![exprstatement!(binexp!(".",
                                                binexp!(".",
                                                  binexp!(".", val!("a"), val!("b")),
                                                val!("c")),
                                               val!("d")))]));
    parse_test!("-3", AST(vec![exprstatement!(prefexp!("-", IntLiteral(3)))]));
    parse_test!("-0.2", AST(vec![exprstatement!(prefexp!("-", FloatLiteral(0.2)))]));
    parse_test!("!3", AST(vec![exprstatement!(prefexp!("!", IntLiteral(3)))]));
    parse_test!("a <- -b", AST(vec![exprstatement!(binexp!("<-", val!("a"), prefexp!("-", val!("b"))))]));
    parse_test!("a <--b", AST(vec![exprstatement!(binexp!("<--", val!("a"), val!("b")))]));
  }

  #[test]
  fn parsing_functions() {
    parse_test!("fn oi()",  AST(vec![Declaration(FuncSig(Signature { name: rc!(oi), params: vec![], type_anno: None }))]));
    parse_test!("oi()", AST(vec![exprstatement!(Call { f: bx!(ex!(val!("oi"))), arguments: vec![] })]));
    parse_test!("oi(a, 2 + 2)", AST(vec![exprstatement!(Call
    { f: bx!(ex!(val!("oi"))),
      arguments: vec![ex!(val!("a")), ex!(binexp!("+", IntLiteral(2), IntLiteral(2)))]
    })]));
    parse_error!("a(b,,c)");

    parse_test!("fn a(b, c: Int): Int", AST(vec![Declaration(
      FuncSig(Signature { name: rc!(a), params: vec![
        (rc!(b), None), (rc!(c), Some(ty!("Int")))
      ], type_anno: Some(ty!("Int")) }))]));


    parse_test!("fn a(x) { x() }", AST(vec![Declaration(
      FuncDecl(Signature { name: rc!(a), params: vec![(rc!(x),None)], type_anno: None },
        vec![exprstatement!(Call { f: bx!(ex!(val!("x"))), arguments: vec![] })]))]));
    parse_test!("fn a(x) {\n x() }", AST(vec![Declaration(
      FuncDecl(Signature { name: rc!(a), params: vec![(rc!(x),None)], type_anno: None },
        vec![exprstatement!(Call { f: bx!(ex!(val!("x"))), arguments: vec![] })]))]));

    let multiline = r#"
fn a(x) {
  x()
}
"#;
    parse_test!(multiline, AST(vec![Declaration(
      FuncDecl(Signature { name: rc!(a), params: vec![(rc!(x),None)], type_anno: None },
        vec![exprstatement!(Call { f: bx!(ex!(val!("x"))), arguments: vec![] })]))]));
    let multiline2 = r#"
fn a(x) {

  x()

}
"#;
    parse_test!(multiline2, AST(vec![Declaration(
      FuncDecl(Signature { name: rc!(a), params: vec![(rc!(x),None)], type_anno: None },
        vec![exprstatement!(Call { f: bx!(ex!(val!("x"))), arguments: vec![] })]))]));

  }

  #[test]
  fn parsing_bools() {
    parse_test!("false", AST(vec![exprstatement!(BoolLiteral(false))]));
    parse_test!("true", AST(vec![exprstatement!(BoolLiteral(true))]));
  }

  #[test]
  fn parsing_strings() {
    parse_test!(r#""hello""#, AST(vec![exprstatement!(StringLiteral(rc!(hello)))]));
  }

  #[test]
  fn parsing_types() {
    parse_test!("type Yolo = Yolo", AST(vec![Declaration(TypeDecl(tys!("Yolo"), TypeBody(vec![UnitStruct(rc!(Yolo))])))]));
    parse_test!("type alias Sex = Drugs", AST(vec![Declaration(TypeAlias(rc!(Sex), rc!(Drugs)))]));
    parse_test!("type Sanchez = Miguel | Alejandro(Int, Option<a>) | Esperanza { a: Int, b: String }",
      AST(vec![Declaration(TypeDecl(tys!("Sanchez"), TypeBody(vec![
        UnitStruct(rc!(Miguel)),
        TupleStruct(rc!(Alejandro), vec![
          Singleton(TypeSingletonName { name: rc!(Int), params: vec![] }),
          Singleton(TypeSingletonName { name: rc!(Option), params: vec![Singleton(TypeSingletonName { name: rc!(a), params: vec![] })] }),
        ]),
        Record(rc!(Esperanza), vec![
          (rc!(a), Singleton(TypeSingletonName { name: rc!(Int), params: vec![] })),
          (rc!(b), Singleton(TypeSingletonName { name: rc!(String), params: vec![] })),
        ])])))]));

    parse_test!("type Jorge<a> = Diego | Kike(a)", AST(vec![
      Declaration(TypeDecl(
        TypeSingletonName { name: rc!(Jorge), params: vec![Singleton(TypeSingletonName { name: rc!(a), params: vec![] })] },
        TypeBody(vec![UnitStruct(rc!(Diego)), TupleStruct(rc!(Kike), vec![Singleton(TypeSingletonName { name: rc!(a), params: vec![] })])]))
    )]));
  }

  #[test]
  fn parsing_bindings() {
    parse_test!("var a = 10", AST(vec![Declaration(Binding { name: rc!(a), constant: false, expr: ex!(IntLiteral(10)) } )]));
    parse_test!("const a = 2 + 2", AST(vec![Declaration(Binding { name: rc!(a), constant: true, expr: ex!(binexp!("+", IntLiteral(2), IntLiteral(2))) }) ]));
  }

  #[test]
  fn parsing_block_expressions() {
    parse_test!("if a() { b(); c() }", AST(vec![exprstatement!(
        IfExpression(bx!(ex!(Call { f: bx!(ex!(val!("a"))), arguments: vec![]})),
          vec![exprstatement!(Call { f: bx!(ex!(val!("b"))), arguments: vec![]}), exprstatement!(Call { f: bx!(ex!(val!("c"))), arguments: vec![] })],
          None)
        )]));
    parse_test!(r#"
    if true {
      const a = 10
      b
    } else {
      c
    }"#,
    AST(vec![exprstatement!(IfExpression(bx!(ex!(BoolLiteral(true))),
      vec![Declaration(Binding { name: rc!(a), constant: true, expr: ex!(IntLiteral(10)) }),
           exprstatement!(val!(rc!(b)))],
      Some(vec![exprstatement!(val!(rc!(c)))])))])
    );

    parse_test!("if a { b } else { c }", AST(vec![exprstatement!(
          IfExpression(bx!(ex!(val!("a"))),
            vec![exprstatement!(val!("b"))],
            Some(vec![exprstatement!(val!("c"))])))]));

    parse_test!("if (A {a: 1}) { b } else { c }", AST(vec![exprstatement!(
          IfExpression(bx!(ex!(NamedStruct { name: rc!(A), fields: vec![(rc!(a), ex!(IntLiteral(1)))]})),
            vec![exprstatement!(val!("b"))],
            Some(vec![exprstatement!(val!("c"))])))]));

    parse_error!("if A {a: 1} { b } else { c }");
  }
  #[test]
  fn parsing_interfaces() {
    parse_test!("interface Unglueable { fn unglue(a: Glue); fn mar(): Glue }", AST(vec![
      Declaration(Interface {
        name: rc!(Unglueable),
        signatures: vec![
          Signature { name: rc!(unglue), params: vec![(rc!(a), Some(Singleton(TypeSingletonName { name: rc!(Glue), params: vec![] })))], type_anno: None },
          Signature { name: rc!(mar), params: vec![], type_anno: Some(Singleton(TypeSingletonName { name: rc!(Glue), params: vec![] })) },
        ]
      })
    ]));
  }

  #[test]
  fn parsing_impls() {
    parse_test!("impl Heh { fn yolo(); fn swagg(); }", AST(vec![
      Declaration(Impl {
        type_name: ty!("Heh"),
        interface_name: None,
        block: vec![
          FuncSig(Signature { name: rc!(yolo), params: vec![], type_anno: None }),
          FuncSig(Signature { name: rc!(swagg), params: vec![], type_anno: None })
        ] })]));

    parse_test!("impl Mondai for Lollerino { fn yolo(); fn swagg(); }", AST(vec![
      Declaration(Impl {
        type_name: ty!("Lollerino"),
        interface_name: Some(rc!(Mondai)),
        block: vec![
          FuncSig(Signature { name: rc!(yolo), params: vec![], type_anno: None}),
          FuncSig(Signature { name: rc!(swagg), params: vec![], type_anno: None })
        ] })]));
    parse_test!("impl Option<WTFMate> { fn oi() }", AST(vec![
      Declaration(Impl {
        type_name: Singleton(TypeSingletonName { name: rc!(Option), params: vec![ty!("WTFMate")]}),
        interface_name: None,
        block: vec![
          FuncSig(Signature { name: rc!(oi), params: vec![], type_anno: None }),
        ]
      })]));
  }

  #[test]
  fn parsing_type_annotations() {
    parse_test!("const a = b : Int", AST(vec![
      Declaration(Binding { name: rc!(a), constant: true, expr:
        Expression(val!("b"), Some(ty!("Int"))) })]));

    parse_test!("a : Int", AST(vec![
      exprstatement!(val!("a"), ty!("Int"))
    ]));

    parse_test!("a : Option<Int>", AST(vec![
      exprstatement!(val!("a"), Singleton(TypeSingletonName { name: rc!(Option), params: vec![ty!("Int")] }))
    ]));

    parse_test!("a : KoreanBBQSpecifier<Kimchi, Option<Bulgogi> >", AST(vec![
      exprstatement!(val!("a"), Singleton(TypeSingletonName { name: rc!(KoreanBBQSpecifier), params: vec![
        ty!("Kimchi"), Singleton(TypeSingletonName { name: rc!(Option), params: vec![ty!("Bulgogi")] })
      ] }))
    ]));

    parse_test!("a : (Int, Yolo<a>)", AST(vec![
      exprstatement!(val!("a"), Tuple(
        vec![ty!("Int"), Singleton(TypeSingletonName {
          name: rc!(Yolo), params: vec![ty!("a")]
        })]))]));
  }

  #[test]
  fn parsing_lambdas() {
    parse_test! { "{|x| x + 1}", single_expr!(
      Lambda { params: vec![(rc!(x), None)], body: vec![exst!("+", val!("x"), IntLiteral(1))] }
    ) }

    parse_test!("{      |x: Int, y| a;b;c;}", AST(vec![
      exprstatement!(Lambda {
        params: vec![(rc!(x), Some(ty!("Int"))), (rc!(y), None)],
        body: vec![exst!(val!("a")), exst!(val!("b")), exst!(val!("c"))]
      })
    ]));

    parse_test!("{|x| y}(1)", AST(vec![
      exprstatement!(Call { f: bx!(ex!(
        Lambda { params: vec![(rc!(x), None)], body: vec![exprstatement!(val!("y"))] })),
        arguments: vec![ex!(IntLiteral(1))] })]));
  }

  #[test]
   fn list_literals() {
     parse_test! {
       "[1,2]", AST(vec![
         exprstatement!(ListLiteral(vec![ex!(IntLiteral(1)), ex!(IntLiteral(2))]))])
     };
   }
}
