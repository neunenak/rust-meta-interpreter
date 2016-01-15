use tokenizer::Token;
use tokenizer::Kw;

/* Grammar
   program := (statement delimiter ?)*
   delimiter := Newline | Semicolon
   statement := declaration | expression
   declaraion :=  Fn prototype (statement)* End
   prototype := identifier LParen identlist RParen
   identlist := Ident (Comma Ident)* | e

   expression := primary_expression (op primary_expression)*
   primary_expression :=  Variable | Number | String | call_expr | paren_expr
   paren_expr := LParen expression RParen
   call_expr := identifier LParen identlist RParen
   op := '+', '-', etc.
 */

#[derive(Debug, Clone)]
pub enum ASTNode {
    ExprNode(Expression),
    FuncNode(Function),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub prototype: Prototype,
    pub body: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct Prototype {
    pub name: String,
    pub args: Vec<String>
}

#[derive(Debug, Clone)]
pub enum Expression {
    StringLiteral(String),
    Number(f64),
    Variable(String),
    BinExp(String, Box<Expression>, Box<Expression>),
    Call(String, Vec<Expression>),
}

pub type AST = Vec<ASTNode>;

//TODO make this support incomplete parses
pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug)]
pub struct ParseError {
    pub msg: String
}

impl ParseError {
    fn result_from_str<T>(msg: &str) -> ParseResult<T> {
        Err(ParseError { msg: msg.to_string() })
    }
}

struct Parser {
    tokens: Vec<Token>,
}

impl Parser {
    fn initialize(tokens: &[Token]) -> Parser {
        let mut tokens = tokens.to_vec();
        tokens.reverse();
        Parser { tokens: tokens }
    }

    fn peek(&mut self) -> Option<Token> {
        self.tokens.last().map(|x| x.clone())
    }

    fn next(&mut self) -> Option<Token>{
        self.tokens.pop()
    }
}

macro_rules! expect {
    ($self_:expr, $token:pat, $error:expr) => {
        match $self_.peek() {
            Some($token) => {$self_.next();},
            _ => return ParseError::result_from_str($error)
        }
    }
}

fn is_delimiter(token: &Token) -> bool {
    use tokenizer::Token::*;
    match *token {
        Newline | Semicolon => true,
        _ => false
    }
}

impl Parser {
    fn program(&mut self) -> ParseResult<AST> {
        use tokenizer::Token::*;
        let mut ast = Vec::new(); //TODO have this come from previously-parsed tree
        loop {
            let cur_tok = match self.peek() {
                Some(t) => t.clone(),
                None => break
            };

            let result: ParseResult<ASTNode> = match cur_tok {
                ref t if is_delimiter(&t) => { self.next(); continue},
                _ => self.statement()
            };

            match result {
                Ok(node) => ast.push(node),
                Err(err) => return Err(err)
            }
        }

        Ok(ast)
    }

    fn statement(&mut self) -> ParseResult<ASTNode> {
        use tokenizer::Token::*;
        let cur_tok: Token = self.peek().unwrap().clone();
        let node: ASTNode = match cur_tok {
            Keyword(Kw::Fn) => try!(self.declaration()),
            _ => ASTNode::ExprNode(try!(self.expression())),
        };

        Ok(node)
    }

    fn declaration(&mut self) -> ParseResult<ASTNode> {
        use tokenizer::Token::*;
        expect!(self, Fn, "Expected 'fn'");
        let prototype = try!(self.prototype());
        let body: Vec<Expression> = try!(self.body());
        expect!(self, Keyword(Kw::End), "Expected 'end'");
        Ok(ASTNode::FuncNode(Function { prototype: prototype, body: body } ))
    }

    fn prototype(&mut self) -> ParseResult<Prototype> {
        use tokenizer::Token::*;
        let name: String = match self.peek() {
            Some(Identifier(name)) => {self.next(); name},
            _ => return ParseError::result_from_str("Expected identifier")
        };
        expect!(self, LParen, "Expected '('");
        let mut args: Vec<String> = try!(self.identlist());
        expect!(self, RParen, "Expected ')'");
        Ok(Prototype {name: name, args: args})
    }

    fn identlist(&mut self) -> ParseResult<Vec<String>> {
        use tokenizer::Token::*;
        let mut args: Vec<String> = Vec::new();
        loop {
            match self.peek() {
                Some(Identifier(name)) => {
                    args.push(name);
                    self.next();
                    if let Some(Comma) = self.peek() {
                        self.next();
                    } else {
                        break;
                    }
                },

                _ => break
            }
        }

        Ok(args)
    }

    fn body(&mut self) -> ParseResult<Vec<Expression>> {
        use tokenizer::Token::*;
        let mut exprs = Vec::new();
        loop {
            match self.peek() {
                Some(ref t) if is_delimiter(t) => { self.next(); continue},
                Some(Keyword(Kw::End)) => break,
                _ => {
                    let expr = try!(self.expression());
                    exprs.push(expr);
                }
            }
        }
        Ok(exprs)
    }

    fn expression(&mut self) -> ParseResult<Expression> {
        use tokenizer::Token::*;
        let mut lhr: Expression = try!(self.primary_expression());
        Ok(lhr)
    }

    fn primary_expression(&mut self) -> ParseResult<Expression> {
        use tokenizer::Token::*;
        let expr = match self.peek() {
            Some(NumLiteral(n)) => { self.next(); Expression::Number(n) },
            Some(StrLiteral(s)) => { self.next(); Expression::StringLiteral(s) },
            Some(Identifier(var)) => {
                self.next();
                match self.peek() {
                    Some(LParen) => try!(self.call_expr()),
                    _ => Expression::Variable(var)
                }
            },
            Some(LParen) => { try!(self.paren_expr()) }
            Some(x) => return ParseError::result_from_str("Expected primary expression"),
            None => return ParseError::result_from_str("Expected primary expression received EoI")
        };

        Ok(expr)
    }

    fn call_expr(&mut self) -> ParseResult<Expression> {
        unimplemented!()
    }

    fn paren_expr(&mut self) -> ParseResult<Expression> {

        unimplemented!()
    }
}

pub fn parse(tokens: &[Token], _parsed_tree: &[ASTNode]) -> ParseResult<AST> {
    let mut parser = Parser::initialize(tokens);
    parser.program()
}
