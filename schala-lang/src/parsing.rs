use std::rc::Rc;
use std::iter::Peekable;
use std::vec::IntoIter;

use tokenizing::*;
use tokenizing::Kw::*;
use tokenizing::TokenType::*;

use ast::*;

use builtin::{BinOp, PrefixOp};

/* Schala EBNF Grammar */
/* Terminal productions are in 'single quotes' or UPPERCASE if they are a class
 * or not representable in ASCII


/* Top-level Structure */

program := (statement delimiter)* EOF
delimiter := NEWLINE | ';'
statement := expression | declaration
block := '{' (statement delimiter)* '}'

declaration := type_declaration | func_declaration | binding_declaration | impl_declaration

/* Declarations - Types */

type_declaration := 'type' type_declaration_body
type_declaration_body := 'alias' type_alias | 'mut'? type_singleton_name '=' type_body
type_alias := IDENTIFIER '=' type_name
type_body := variant_specifier ('|' variant_specifier)*
variant_specifier := IDENTIFIER | IDENTIFIER '{' typed_identifier_list '}' | IDENTIFIER '(' type_name* ')'
typed_identifier_list := typed_identifier*
typed_identifier := IDENTIFIER type_anno

/* Declaration - Functions */

func_declaration := func_signature func_body
func_body := ε | '{' (statement delimiter)* '}'
func_signature := 'fn' IDENTIFIER formal_param_list func_body
formal_param_list := '(' (formal_param ',')* ')'
formal_param := IDENTIFIER type_anno+

/* Declaration - Variable bindings */
binding_declaration := 'let' 'mut'? IDENTIFIER '=' expresion

/* Declaration - Interface */

interface_declaration := 'interface' interface_name signature_block
impl_declaration := 'impl' IDENTIFIER decl_block | 'impl' interface_name 'for' IDENTIFIER decl_block
decl_block := '{' (func_declaration)* '}'
signature_block := '{' (func_signature)* '}'
interface_name := IDENTIFIER

/* Type annotations */

type_anno := (':' type_name)+
type_name := type_singleton_name | '(' type_names ')'
type_names := ε | type_name (, type_name)*
type_singleton_name = IDENTIFIER (type_params)*
type_params := '<' type_name (, type_name)* '>'


/* Expressions */

expression := precedence_expr type_anno+
precedence_expr := prefix_expr
prefix_expr := prefix_op call_expr
prefix_op := '+' | '-' | '!' | '~'
call_expr := index_expr ( '(' expr_list ')' )*
expr_list := expression (',' expression)* | ε
index_expr := primary ( '[' (expression (',' (expression)* | ε) ']' )*
primary := literal | paren_expr | if_expr | match_expr | for_expr | while_expr | identifier_expr | curly_brace_expr | list_expr

/* Primary Expressions */

curly_brace_expr := lambda_expr | anonymous_struct //TODO
list_expr := '[' (expression, ',')* ']'
lambda_expr := '{' '|' (formal_param ',')* '|' (type_anno)* (statement delimiter)* '}'
paren_expr := LParen paren_inner RParen
paren_inner := (expression ',')*
identifier_expr := named_struct | IDENTIFIER

/* Expression - Literals */

literal := 'true' | 'false' | number_literal | STR_LITERAL
named_struct := IDENTIFIER record_block
record_block :=  '{' (record_entry, ',')* | '}' //TODO support anonymus structs, update syntax
record_entry := IDENTIFIER ':' expression
anonymous_struct := TODO

// a float_literal can still be assigned to an int in type-checking
number_literal := int_literal | float_literal
int_literal = ('0x' | '0b') digits
float_literal := digits ('.' digits)
digits := (DIGIT_GROUP underscore)+

/* OLD OBSOLETE */

/* Expression - If */
if_expr := 'if' expression block else_clause
else_clause := ε | 'else' block

/* Expression - Match */
match_expr := 'match' expression match_body
match_body := '{' (match_arm)* '}'
match_arm := pattern '=>' expression

/* NEW GOOD */

/* Pattern syntax */
pattern := '(' (pattern, ',')* ')' | simple_pattern
simple_pattern := pattern_literal | record_pattern | tuple_struct_pattern
pattern_literal := 'true' | 'false' | number_literal | STR_LITERAL | IDENTIFIER
record_pattern := IDENTIFIER '{' (record_pattern_entry, ',')* '}'
record_pattern_entry := ???
tuple_struct_pattern := IDENTIFIER '(' (pattern, ',')* ')'

/* Expression - If */
if_expr := 'if' discriminator ('then' condititional | 'is' simple_pattern_match | guard_block)
discriminator := modified_precedence_expression
conditional := block else_clause
simple_pattern_match := pattern 'then' conditional
else_clause := ε | 'else' block

/* Expression - While */
while_expr := 'while' while_cond '{' (statement delimiter)* '}'
while_cond := ε | expression | expression 'is'  pattern //TODO maybe is-expresions should be primary

//TODO this implies there must be at least one enumerator, which the parser doesn't support right
//this second, and maybe should fail later anyway
/* Expression - For */
for_expr := 'for' (enumerator | '{' enumerators '}') for_expr_body
for_expr_body := 'return' expression |  '{' (statement delimiter)* '}
enumerators := enumerator (',' enumerators)*
enumerator :=  identifier '<-' expression | identifier '=' expression //TODO add guards, etc.
*/

type TokenIter = Peekable<IntoIter<Token>>;

#[derive(Debug)]
pub struct ParseError {
  pub msg: String,
  pub token: Option<Token>
}

impl ParseError {
  fn new<T>(msg: &str) -> ParseResult<T> {
    Err(ParseError { 
      msg: msg.to_string(),
      token: None
    })
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

macro_rules! print_token_pattern {
  ($tokenpattern:pat) => { stringify!($tokenpattern) }
}

macro_rules! expect {
  ($self:expr, $token_type:pat) => { expect!($self, $token_type if true) };
  ($self:expr, $token_type:pat if $cond:expr) => {
    match $self.peek() {
      $token_type if $cond => $self.next(),
      tok => {
        let msg = format!("Expected {}, got {:?}", print_token_pattern!($token_type), tok);
        return ParseError::new(&msg);
      }
    }
  }
}


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
      match result {
        Err(ParseError { token: None, msg }) =>
          Err(ParseError { token: Some(next_token), msg }),
        _ => result
      }
    }
  };
}

macro_rules! delimited {
  ($self:expr, $start:pat, $parse_fn:ident, $( $delim:pat )|+, $end:pat, nonstrict) => {
    delimited!($self, $start, $parse_fn, $( $delim )|*, $end, false)
  };
  ($self:expr, $start:pat, $parse_fn:ident, $( $delim:pat )|+, $end:pat) => {
    delimited!($self, $start, $parse_fn, $( $delim )|*, $end, true)
  };
  ($self:expr, $start:pat, $parse_fn:ident, $( $delim:pat )|+, $end:pat, $strictness:expr) => {
    {
      expect!($self, $start);
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
      expect!($self, $end);
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
      Keyword(Let) => self.binding_declaration().map(|decl| Statement::Declaration(decl)),
      Keyword(Interface) => self.interface_declaration().map(|decl| Statement::Declaration(decl)),
      Keyword(Impl) => self.impl_declaration().map(|decl| Statement::Declaration(decl)),
      _ => self.expression().map(|expr| { Statement::ExpressionStatement(expr) } ),
    }
  });

  parse_method!(type_declaration(&mut self) -> ParseResult<Declaration> {
    expect!(self, Keyword(Type));
    self.type_declaration_body()
  });

  parse_method!(type_declaration_body(&mut self) -> ParseResult<Declaration> {
    if let Keyword(Alias) = self.peek() {
      self.type_alias()
    } else {
      let mutable = if let Keyword(Mut) = self.peek() {
        self.next();
        true
      } else {
        false
      };
      let name = self.type_singleton_name()?;
      expect!(self, Operator(ref c) if **c == "=");
      let body = self.type_body()?;
      Ok(Declaration::TypeDecl { name, body, mutable})
    }
  });

  parse_method!(type_alias(&mut self) -> ParseResult<Declaration> {
    expect!(self, Keyword(Alias));
    let alias = self.identifier()?;
    expect!(self, Operator(ref c) if **c == "=");
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
        let tuple_members = delimited!(self, LParen, type_name, Comma, RParen);
        Ok(TupleStruct(name, tuple_members))
      },
      LCurlyBrace => {
        let typed_identifier_list = delimited!(self, LCurlyBrace, typed_identifier, Comma, RCurlyBrace);
        Ok(Record(name, typed_identifier_list))
      },
      _ => Ok(UnitStruct(name))
    }
  });

  parse_method!(typed_identifier(&mut self) -> ParseResult<(Rc<String>, TypeName)> {
    let identifier = self.identifier()?;
    expect!(self, Colon);
    let type_name = self.type_name()?;
    Ok((identifier, type_name))
  });

  parse_method!(func_declaration(&mut self) -> ParseResult<Declaration> {
    let signature = self.signature()?;
    if let LCurlyBrace = self.peek() {
      let statements = delimited!(self, LCurlyBrace, statement, Newline | Semicolon, RCurlyBrace, nonstrict);
      Ok(Declaration::FuncDecl(signature, statements))
    } else {
      Ok(Declaration::FuncSig(signature))
    }
  });

  parse_method!(signature(&mut self) -> ParseResult<Signature> {
    expect!(self, Keyword(Func));
    let name = self.identifier()?;
    let params = delimited!(self, LParen, formal_param, Comma, RParen);
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

    expect!(self, Keyword(Kw::Let));
    let constant = match self.peek() {
      Keyword(Kw::Mut) => {
        self.next();
        false 
      }
      _ => true
    };
    let name = self.identifier()?;
    expect!(self, Operator(ref o) if **o == "=");
    let expr = self.expression()?;

    Ok(Declaration::Binding { name, constant, expr })
  });

  parse_method!(interface_declaration(&mut self) -> ParseResult<Declaration> {
    expect!(self, Keyword(Interface));
    let name = self.identifier()?;
    let signatures = self.signature_block()?;
    Ok(Declaration::Interface { name, signatures })
  });

  parse_method!(signature_block(&mut self) -> ParseResult<Vec<Signature>> {
    Ok(delimited!(self, LCurlyBrace, signature, Newline | Semicolon, RCurlyBrace, nonstrict))
  });

  parse_method!(impl_declaration(&mut self) -> ParseResult<Declaration> {
    expect!(self, Keyword(Impl));
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
    Ok(delimited!(self, LCurlyBrace, func_declaration, Newline | Semicolon, RCurlyBrace, nonstrict))
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
    expect!(self, Colon);
    self.type_name()
  });

  parse_method!(type_name(&mut self) -> ParseResult<TypeName> {
    use self::TypeName::*;
    Ok(match self.peek() {
      LParen => Tuple(delimited!(self, LParen, type_name, Comma, RParen)),
      _ => Singleton(self.type_singleton_name()?),
    })
  });

  parse_method!(type_singleton_name(&mut self) -> ParseResult<TypeSingletonName> {
    Ok(TypeSingletonName {
      name: self.identifier()?,
      params: match self.peek() {
        LAngleBracket => delimited!(self, LAngleBracket, type_name, Comma, RAngleBracket),
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
        Slash => Rc::new("/".to_string()),
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
      let arguments = delimited!(self, LParen, expression, Comma, RParen);
      Expression(ExpressionType::Call { f: bx!(index), arguments }, None) //TODO fix this none
    } else {
      index
    })
  });

  parse_method!(index_expr(&mut self) -> ParseResult<Expression> {
    let primary = self.primary()?;
    Ok(if let LSquareBracket = self.peek() {
      let indexers = delimited!(self, LSquareBracket, expression, Comma, RSquareBracket);
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
      //Keyword(Kw::Match) => self.match_expr(),
      Keyword(Kw::For) => self.for_expr(),
      Keyword(Kw::While) => self.while_expr(),
      Identifier(_) => self.identifier_expr(),
      _ => self.literal(),
    }
  });

  parse_method!(list_expr(&mut self) -> ParseResult<Expression> {
    let exprs = delimited!(self, LSquareBracket, expression, Comma, RSquareBracket);
    Ok(Expression(ExpressionType::ListLiteral(exprs), None))
  });

  parse_method!(curly_brace_expr(&mut self) -> ParseResult<Expression> {
    self.lambda_expr()
  });

  parse_method!(lambda_expr(&mut self) -> ParseResult<Expression> {
    expect!(self, LCurlyBrace);
    let params = delimited!(self, Pipe, formal_param, Comma, Pipe);
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
    expect!(self, RCurlyBrace);
    Ok(Expression(ExpressionType::Lambda { params, body }, None)) //TODO need to handle types somehow
  });

  parse_method!(paren_expr(&mut self) -> ParseResult<Expression> {
    use self::ExpressionType::*;
    let old_struct_value = self.restrictions.no_struct_literal;
    self.restrictions.no_struct_literal = false;
    let output = {
      let mut inner = delimited!(self, LParen, expression, Comma, RParen);
      match inner.len() {
        0 => Ok(Expression(TupleLiteral(vec![]), None)),
        1 => Ok(inner.pop().unwrap()),
        _ => Ok(Expression(TupleLiteral(inner), None)),
      }
    };
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
    Ok(delimited!(self, LCurlyBrace, record_entry, Comma, RCurlyBrace))
  });

  parse_method!(record_entry(&mut self) -> ParseResult<(Rc<String>, Expression)> {
    let field_name = self.identifier()?;
    expect!(self, Colon);
    let value = self.expression()?;
    Ok((field_name, value))
  });

  parse_method!(if_expr(&mut self) -> ParseResult<Expression> {
    expect!(self, Keyword(Kw::If));
    let discriminator = Box::new({
      self.restrictions.no_struct_literal = true;
      let x = self.discriminator();
      self.restrictions.no_struct_literal = false;
      x?
    });

    let body = Box::new(match self.peek() {
      Keyword(Kw::Then) => self.conditional()?,
      Keyword(Kw::Is) => self.simple_pattern_match()? ,
      _ => self.guard_block()?
    });

    Ok(Expression(ExpressionType::IfExpression { discriminator, body }, None))
  });

  parse_method!(discriminator(&mut self) -> ParseResult<Discriminator> {
    Ok(Discriminator::Simple(self.expression()?)) //TODO make proper
  });

  parse_method!(conditional(&mut self) -> ParseResult<IfExpressionBody> {
    expect!(self, Keyword(Kw::Then));
    let then_clause = self.block()?; //TODO should be block_or_expr
    let else_clause = self.else_clause()?;
    Ok(IfExpressionBody::SimpleConditional(then_clause, else_clause))
  });

  parse_method!(simple_pattern_match(&mut self) -> ParseResult<IfExpressionBody> {
    expect!(self, Keyword(Kw::Is));
    let pat = self.pattern()?;
    expect!(self, Keyword(Kw::Then));
    let then_clause = self.block()?; //TODO should be block_or_expr
    let else_clause = self.else_clause()?;
    Ok(IfExpressionBody::SimplePatternMatch(pat, then_clause, else_clause))
  });

  parse_method!(guard_block(&mut self) -> ParseResult<IfExpressionBody> {
    ParseError::new("Rest of if not done")
  });

  parse_method!(else_clause(&mut self) -> ParseResult<Option<Block>> {
    Ok(if let Keyword(Kw::Else) = self.peek() {
      self.next();
      Some(self.block()?)
    } else {
      None
    })
  });

  parse_method!(pattern(&mut self) -> ParseResult<Pattern> {
    if let LParen = self.peek() {
      let tuple_pattern_variants = delimited!(self, LParen, pattern, Comma, RParen);
      Ok(Pattern::TuplePattern(tuple_pattern_variants))
    } else {
      self.simple_pattern()
    }
  });

  parse_method!(simple_pattern(&mut self) -> ParseResult<Pattern> {
    Ok(match self.peek() {
      Identifier(_) => {
        let id = self.identifier()?;
        match self.peek() {
          LCurlyBrace => { 
            let members = delimited!(self, LCurlyBrace, record_pattern_entry, Comma, RCurlyBrace);
            Pattern::Record(id, members)
          },
          LParen => {
            let members = delimited!(self, LParen, pattern, Comma, RParen);
            Pattern::TupleStruct(id, members)
          },
          _ => Pattern::Literal(PatternLiteral::VarPattern(id))
        }
      },
      Keyword(Kw::True) => Pattern::Literal(PatternLiteral::BoolPattern(true)),
      Keyword(Kw::False) => Pattern::Literal(PatternLiteral::BoolPattern(false)),
      StrLiteral(s) =>  Pattern::Literal(PatternLiteral::StringPattern(s)),
      DigitGroup(_) | HexLiteral(_) | BinNumberSigil | Period => {
        //TODO handle negatives
        let Expression(expr_type, _) = self.number_literal()?;
        Pattern::Literal(PatternLiteral::NumPattern(expr_type))
      },
      Underscore => {
        self.next();
        Pattern::Ignored
      },
      other => return ParseError::new(&format!("{:?} is not a valid Pattern", other))
    })
  });

  parse_method!(record_pattern_entry(&mut self) -> ParseResult<(Rc<String>, Pattern)> {
    unimplemented!()
  });

  parse_method!(block(&mut self) -> ParseResult<Block> {
    Ok(delimited!(self, LCurlyBrace, statement, Newline | Semicolon, RCurlyBrace, nonstrict))
  });

  /*
  parse_method!(match_expr(&mut self) -> ParseResult<Expression> {
    expect!(self, Keyword(Kw::Match));
    let expr = {
      self.restrictions.no_struct_literal = true;
      let x = self.expression();
      self.restrictions.no_struct_literal = false;
      x?
    };
    let body = self.match_body()?;
    Ok(Expression(ExpressionType::MatchExpression(bx!(expr), body), None))
  });

  parse_method!(match_body(&mut self) -> ParseResult<Vec<MatchArm>> {
    Ok(delimited!(self, LCurlyBrace, match_arm, Comma, RCurlyBrace))
  });

  parse_method!(match_arm(&mut self) -> ParseResult<MatchArm> {
    let pat = self.pattern()?;
    expect!(self, Operator(ref c) if **c == "=>");
    let expr = self.expression()?;
    Ok(MatchArm { pat, expr })
  });

  */

  parse_method!(while_expr(&mut self) -> ParseResult<Expression> {
    use self::ExpressionType::*;
    expect!(self, Keyword(Kw::While));
    let condition =  {
      self.restrictions.no_struct_literal = true;
      let x = self.while_cond();
      self.restrictions.no_struct_literal = false;
      x?.map(|expr| bx!(expr))
    };
    let body = self.block()?;
    Ok(Expression(WhileExpression {condition, body}, None))
  });

  parse_method!(while_cond(&mut self) -> ParseResult<Option<Expression>> {
    Ok(match self.peek() {
      LCurlyBrace => None,
      _ => Some(self.expression()?),
    })
  });

  parse_method!(for_expr(&mut self) -> ParseResult<Expression> {
    expect!(self, Keyword(Kw::For));
    let enumerators = if let LCurlyBrace = self.peek() {
      delimited!(self, LCurlyBrace, enumerator, Comma | Newline, RCurlyBrace)
    } else {
      let single_enum = {
        self.restrictions.no_struct_literal = true;
        let s = self.enumerator();
        self.restrictions.no_struct_literal = false;
        s?
      };
      vec![single_enum]
    };
    let body = Box::new(self.for_expr_body()?);
    Ok(Expression(ExpressionType::ForExpression { enumerators, body }, None))
  });

  parse_method!(enumerator(&mut self) -> ParseResult<Enumerator> {
    let id = self.identifier()?;
    expect!(self, Operator(ref c) if **c == "<-");
    let generator = self.expression()?;
    Ok(Enumerator { id, generator })
  });

  parse_method!(for_expr_body(&mut self) -> ParseResult<ForBody> {
    use self::ForBody::*;
    Ok(match self.peek() {
      LCurlyBrace => {
        let statements = delimited!(self, LCurlyBrace, statement, Newline | Semicolon, RCurlyBrace, nonstrict);
        StatementBlock(statements)
      },
      Keyword(Kw::Return) => {
        self.next();
        MonadicReturn(self.expression()?)
      },
      _ => return ParseError::new("for expressions must end in a block or 'return'"),
    })
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
        Ok(Expression(NatLiteral(n), None))
      },
      HexLiteral(text) => {
        let digits: String = text.chars().filter(|c| c.is_digit(16)).collect();
        let n = parse_hex(digits)?;
        Ok(Expression(NatLiteral(n), None))
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
        Ok(d) => Ok(Expression(NatLiteral(d), None)),
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
  use super::{parse, tokenize};
  use builtin::{PrefixOp, BinOp};
  use ast::{AST, Expression, Statement, IfExpressionBody, Discriminator, Pattern, TypeBody, Variant, Enumerator, ForBody};
  use super::Statement::*;
  use super::Declaration::*;
  use super::Signature;
  use super::TypeName::*;
  use super::TypeSingletonName;
  use super::ExpressionType::*;
  use super::Variant::*;
  use super::ForBody::*;

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

    parse_test! { "0b010", single_expr!(NatLiteral(2)) };
    parse_test! { "0b0_1_0_", single_expr!(NatLiteral(2)) }

    parse_test! {"0xff", single_expr!(NatLiteral(255)) };
    parse_test! {"0xf_f_", single_expr!(NatLiteral(255)) };

    parse_test!("0xf_f_+1", AST(vec![exprstatement!(binexp!("+", NatLiteral(255), NatLiteral(1)))]));

    parse_test! {"3; 4; 4.3", AST(
      vec![exprstatement!(NatLiteral(3)), exprstatement!(NatLiteral(4)),
        exprstatement!(FloatLiteral(4.3))])
    };

    parse_test!("1 + 2 * 3", AST(vec!
      [
        exprstatement!(binexp!("+", NatLiteral(1), binexp!("*", NatLiteral(2), NatLiteral(3))))
      ]));

    parse_test!("1 * 2 + 3", AST(vec!
      [
        exprstatement!(binexp!("+", binexp!("*", NatLiteral(1), NatLiteral(2)), NatLiteral(3)))
      ]));

    parse_test!("1 && 2", AST(vec![exprstatement!(binexp!("&&", NatLiteral(1), NatLiteral(2)))]));

    parse_test!("1 + 2 * 3 + 4", AST(vec![exprstatement!(
      binexp!("+",
        binexp!("+", NatLiteral(1), binexp!("*", NatLiteral(2), NatLiteral(3))),
        NatLiteral(4)))]));

    parse_test!("(1 + 2) * 3", AST(vec!
      [exprstatement!(binexp!("*", binexp!("+", NatLiteral(1), NatLiteral(2)), NatLiteral(3)))]));

    parse_test!(".1 + .2", AST(vec![exprstatement!(binexp!("+", FloatLiteral(0.1), FloatLiteral(0.2)))]));
    parse_test!("1 / 2", AST(vec![exprstatement!(binexp!("/", NatLiteral(1), NatLiteral(2)))]));
  }

  #[test]
  fn parsing_tuples() {
    parse_test!("()", AST(vec![exprstatement!(TupleLiteral(vec![]))]));
    parse_test!("(\"hella\", 34)", AST(vec![exprstatement!(
      TupleLiteral(
        vec![ex!(StringLiteral(rc!(hella))), ex!(NatLiteral(34))]
      )
    )]));
    parse_test!("((1+2), \"slough\")", AST(vec![exprstatement!(TupleLiteral(vec![
      ex!(binexp!("+", NatLiteral(1), NatLiteral(2))),
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
    parse_test!("-3", AST(vec![exprstatement!(prefexp!("-", NatLiteral(3)))]));
    parse_test!("-0.2", AST(vec![exprstatement!(prefexp!("-", FloatLiteral(0.2)))]));
    parse_test!("!3", AST(vec![exprstatement!(prefexp!("!", NatLiteral(3)))]));
    parse_test!("a <- -b", AST(vec![exprstatement!(binexp!("<-", val!("a"), prefexp!("-", val!("b"))))]));
    parse_test!("a <--b", AST(vec![exprstatement!(binexp!("<--", val!("a"), val!("b")))]));
  }

  #[test]
  fn parsing_functions() {
    parse_test!("fn oi()",  AST(vec![Declaration(FuncSig(Signature { name: rc!(oi), params: vec![], type_anno: None }))]));
    parse_test!("oi()", AST(vec![exprstatement!(Call { f: bx!(ex!(val!("oi"))), arguments: vec![] })]));
    parse_test!("oi(a, 2 + 2)", AST(vec![exprstatement!(Call
    { f: bx!(ex!(val!("oi"))),
      arguments: vec![ex!(val!("a")), ex!(binexp!("+", NatLiteral(2), NatLiteral(2)))]
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
    parse_test!("type Yolo = Yolo", AST(vec![Declaration(TypeDecl { name: tys!("Yolo"), body: TypeBody(vec![UnitStruct(rc!(Yolo))]), mutable: false} )]));
    parse_test!("type mut Yolo = Yolo", AST(vec![Declaration(TypeDecl { name: tys!("Yolo"), body: TypeBody(vec![UnitStruct(rc!(Yolo))]), mutable: true} )]));
    parse_test!("type alias Sex = Drugs", AST(vec![Declaration(TypeAlias(rc!(Sex), rc!(Drugs)))]));
    parse_test!("type Sanchez = Miguel | Alejandro(Int, Option<a>) | Esperanza { a: Int, b: String }",
    AST(vec![Declaration(TypeDecl{
      name: tys!("Sanchez"),
      body: TypeBody(vec![
        UnitStruct(rc!(Miguel)),
        TupleStruct(rc!(Alejandro), vec![
          Singleton(TypeSingletonName { name: rc!(Int), params: vec![] }),
          Singleton(TypeSingletonName { name: rc!(Option), params: vec![Singleton(TypeSingletonName { name: rc!(a), params: vec![] })] }),
        ]),
        Record(rc!(Esperanza), vec![
              (rc!(a), Singleton(TypeSingletonName { name: rc!(Int), params: vec![] })),
              (rc!(b), Singleton(TypeSingletonName { name: rc!(String), params: vec![] })),
        ])
      ]),
      mutable: false
    })]));

    parse_test!("type Jorge<a> = Diego | Kike(a)", AST(vec![
      Declaration(TypeDecl{
        name: TypeSingletonName { name: rc!(Jorge), params: vec![Singleton(TypeSingletonName { name: rc!(a), params: vec![] })] },
        body: TypeBody(vec![UnitStruct(rc!(Diego)), TupleStruct(rc!(Kike), vec![Singleton(TypeSingletonName { name: rc!(a), params: vec![] })])]),
        mutable: false
      }
    )]));
  }

  #[test]
  fn parsing_bindings() {
    parse_test!("let mut a = 10", AST(vec![Declaration(Binding { name: rc!(a), constant: false, expr: ex!(NatLiteral(10)) } )]));
    parse_test!("let a = 2 + 2", AST(vec![Declaration(Binding { name: rc!(a), constant: true, expr: ex!(binexp!("+", NatLiteral(2), NatLiteral(2))) }) ]));
  }

  #[test]
  fn parsing_block_expressions() {
    parse_test! {
      "if a() then { b(); c() }", AST(vec![exprstatement!(
        IfExpression {
          discriminator: bx! {
            Discriminator::Simple(ex!(Call { f: bx!(ex!(val!("a"))), arguments: vec![]}))
          },
          body: bx! {
            IfExpressionBody::SimpleConditional(
              vec![exprstatement!(Call { f: bx!(ex!(val!("b"))), arguments: vec![]}), exprstatement!(Call { f: bx!(ex!(val!("c"))), arguments: vec![] })],
              None
            )
          }
        }
      )])
    };

    parse_test! {
      "if a() then { b(); c() } else { q }", AST(vec![exprstatement!(
        IfExpression {
          discriminator: bx! {
            Discriminator::Simple(ex!(Call { f: bx!(ex!(val!("a"))), arguments: vec![]}))
          },
          body: bx! {
            IfExpressionBody::SimpleConditional(
              vec![exprstatement!(Call { f: bx!(ex!(val!("b"))), arguments: vec![]}), exprstatement!(Call { f: bx!(ex!(val!("c"))), arguments: vec![] })],
              Some(
                vec![exprstatement!(val!("q"))],
              )
            )
          }
        }
      )])
    };

    /*
    parse_test!("if a() then { b(); c() }", AST(vec![exprstatement!(
        IfExpression(bx!(ex!(Call { f: bx!(ex!(val!("a"))), arguments: vec![]})),
          vec![exprstatement!(Call { f: bx!(ex!(val!("b"))), arguments: vec![]}), exprstatement!(Call { f: bx!(ex!(val!("c"))), arguments: vec![] })],
          None)
        )]));
    parse_test!(r#"
    if true then {
      const a = 10
      b
    } else {
      c
    }"#,
    AST(vec![exprstatement!(IfExpression(bx!(ex!(BoolLiteral(true))),
      vec![Declaration(Binding { name: rc!(a), constant: true, expr: ex!(NatLiteral(10)) }),
           exprstatement!(val!(rc!(b)))],
      Some(vec![exprstatement!(val!(rc!(c)))])))])
    );

    parse_test!("if a { b } else { c }", AST(vec![exprstatement!(
          IfExpression(bx!(ex!(val!("a"))),
            vec![exprstatement!(val!("b"))],
            Some(vec![exprstatement!(val!("c"))])))]));

    parse_test!("if (A {a: 1}) { b } else { c }", AST(vec![exprstatement!(
          IfExpression(bx!(ex!(NamedStruct { name: rc!(A), fields: vec![(rc!(a), ex!(NatLiteral(1)))]})),
            vec![exprstatement!(val!("b"))],
            Some(vec![exprstatement!(val!("c"))])))]));

    parse_error!("if A {a: 1} { b } else { c }");
    */
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
    parse_test!("let a = b : Int", AST(vec![
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
      Lambda { params: vec![(rc!(x), None)], body: vec![exst!("+", val!("x"), NatLiteral(1))] }
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
        arguments: vec![ex!(NatLiteral(1))] })]));
  }

  #[test]
   fn list_literals() {
     parse_test! {
       "[1,2]", AST(vec![
         exprstatement!(ListLiteral(vec![ex!(NatLiteral(1)), ex!(NatLiteral(2))]))])
     };
   }

  #[test]
  fn while_expr() {
    parse_test! {
      "while { }", AST(vec![
      exprstatement!(WhileExpression { condition: None, body: vec![] })])
    }

    parse_test! {
      "while a == b { }", AST(vec![
      exprstatement!(WhileExpression { condition: Some(bx![ex![binexp!("==", val!("a"), val!("b"))]]), body: vec![] })])
    }
  }

  #[test]
  fn for_expr() {
    parse_test! {
      "for { a <- maybeValue } return 1", AST(vec![
      exprstatement!(ForExpression {
        enumerators: vec![Enumerator { id: rc!(a), generator: ex!(val!("maybeValue")) }],
        body: bx!(MonadicReturn(ex!(NatLiteral(1))))
      })])
    }

    parse_test! {
      "for n <- someRange { f(n); }", AST(vec![
      exprstatement!(ForExpression { enumerators: vec![Enumerator { id: rc!(n), generator: ex!(val!("someRange"))}],
        body: bx!(ForBody::StatementBlock(vec![exprstatement!(Call { f: bx![ex!(val!("f"))], arguments: vec![ex!(val!("n"))] })]))
      })])
    }
  }

  #[test]
  fn patterns() {
    parse_test! {
      "if x is Some(a) then { 4 } else { 9 }", AST(vec![
        exprstatement!(
          IfExpression {
            discriminator: Discriminator::Simple(ex!(Value("x"))),
            body: SimplePatternMatch(TupleStruct("Some", [Literal(VarPattern("a"))]), [ExpressionStatement(Expression(NatLiteral(4), None))], Some([ExpressionStatement(Expression(NatLiteral(9), None))])) }
          )
      ])
    }
  }
}
