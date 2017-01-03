use std::fmt;
use tokenizer::{Token, Kw, Op};
use std::collections::VecDeque;

// Grammar
// program := (statement delimiter ?)*
// delimiter := Newline | Semicolon
// statement := declaration | expression
// declaraion :=  Fn prototype (statement)* End
// prototype := identifier LParen identlist RParen
// identlist := Ident (Comma Ident)* | e
// exprlist  := Expression (Comma Expression)* | e
//
// expression := primary_expression (op primary_expression)*
// primary_expression :=  Number | String | identifier_expr | paren_expr | conditional_expr
// identifier_expr := call_expression | Variable
// paren_expr := LParen expression RParen
// call_expr := Identifier LParen exprlist RParen
// conditional_expr := IF expression THEN (expression delimiter?)* ELSE (expresion delimiter?)* END
// op := '+', '-', etc.
//

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

#[derive(Debug, Clone, PartialEq)]
pub struct Prototype {
    pub name: String,
    pub parameters: Vec<String>,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Null,
    StringLiteral(String),
    Number(f64),
    Variable(String),
    BinExp(String, Box<Expression>, Box<Expression>),
    Call(String, Vec<Expression>),
    Conditional(Box<Expression>, Box<Expression>, Option<Box<Expression>>),
    Block(VecDeque<Expression>),
}

impl fmt::Display for ASTNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::ASTNode::*;
        match self {
            &ExprNode(ref expr) => write!(f, "{}", expr),
            &FuncNode(_) => write!(f, "UNIMPLEMENTED"),
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Expression::*;
        match self {
            &Null => write!(f, "null"),
            &StringLiteral(ref s) => write!(f, "\"{}\"", s),
            &Number(n) => write!(f, "{}", n),
            _ => write!(f, "UNIMPLEMENTED"),
        }
    }
}

pub type AST = Vec<ASTNode>;

type Precedence = u8;

// TODO make this support incomplete parses
pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug)]
pub struct ParseError {
    pub msg: String,
    pub remaining_tokens: Vec<Token>,
}

impl ParseError {
    fn result_from_str<T>(msg: &str) -> ParseResult<T> {
        Err(ParseError {
            msg: msg.to_string(),
            remaining_tokens: vec![],
        })
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

    fn peek(&self) -> Option<Token> {
        self.tokens.last().map(|x| x.clone())
    }

    fn next(&mut self) -> Option<Token> {
        self.tokens.pop()
    }

    fn get_precedence(&self, op: &Op) -> Precedence {
        match &op.0[..] {
            "+" => 10,
            "-" => 10,
            "*" => 20,
            "/" => 20,
            "%" => 20,
            "=" => 1,
            _ => 255,
        }
    }
}

macro_rules! expect {
    ($self_:expr, $token:pat) => {
        match $self_.peek() {
            Some($token) => {$self_.next();},
            Some(x) => {
                let err = format!("Expected `{:?}` but got `{:?}`", stringify!($token), x); //TODO implement Display for token
                return ParseError::result_from_str(&err)
            },
            None => {
                let err = format!("Expected `{:?}` but got end of input", stringify!($token));
                return ParseError::result_from_str(&err) //TODO make this not require 2 stringifications
            }

        }
    }
}

macro_rules! expect_identifier {
    ($self_:expr) => {
        match $self_.peek() {
            Some(Identifier(s)) => {$self_.next(); s},
            _ => return ParseError::result_from_str("Expected Identifier")
        }
    }
}

fn is_delimiter(token: &Token) -> bool {
    use tokenizer::Token::*;
    match *token {
        Newline | Semicolon => true,
        _ => false,
    }
}

impl Parser {
    fn program(&mut self) -> ParseResult<AST> {
        let mut ast = Vec::new(); //TODO have this come from previously-parsed tree
        loop {
            let result: ParseResult<ASTNode> = match self.peek() {
                Some(ref t) if is_delimiter(&t) => {
                    self.next();
                    continue;
                }
                Some(_) => self.statement(),
                None => break,
            };

            match result {
                Ok(node) => ast.push(node),
                Err(mut err) => {
                    err.remaining_tokens = self.tokens.clone();
                    err.remaining_tokens.reverse();
                    return Err(err);
                }
            }
        }

        Ok(ast)
    }

    fn statement(&mut self) -> ParseResult<ASTNode> {
        use tokenizer::Token::*;
        let node: ASTNode = match self.peek() {
            Some(Keyword(Kw::Fn)) => try!(self.declaration()),
            Some(_) => ASTNode::ExprNode(try!(self.expression())),
            None => panic!("unexpected end of tokens"),
        };

        Ok(node)
    }

    fn declaration(&mut self) -> ParseResult<ASTNode> {
        use tokenizer::Token::*;
        expect!(self, Keyword(Kw::Fn));
        let prototype = try!(self.prototype());
        let body: Vec<Expression> = try!(self.body());
        expect!(self, Keyword(Kw::End));
        Ok(ASTNode::FuncNode(Function {
            prototype: prototype,
            body: body,
        }))
    }

    fn prototype(&mut self) -> ParseResult<Prototype> {
        use tokenizer::Token::*;
        let name: String = expect_identifier!(self);
        expect!(self, LParen);
        let parameters: Vec<String> = try!(self.identlist());
        expect!(self, RParen);
        Ok(Prototype {
            name: name,
            parameters: parameters,
        })
    }

    fn identlist(&mut self) -> ParseResult<Vec<String>> {
        use tokenizer::Token::*;
        let mut args: Vec<String> = Vec::new();
        while let Some(Identifier(name)) = self.peek() {
            args.push(name);
            self.next();
            if let Some(Comma) = self.peek() {
                self.next();
            } else {
                break;
            }
        }
        Ok(args)
    }

    fn exprlist(&mut self) -> ParseResult<Vec<Expression>> {
        use tokenizer::Token::*;
        let mut args: Vec<Expression> = Vec::new();
        loop {
            if let Some(RParen) = self.peek() {
                break;
            }
            let exp = try!(self.expression());
            args.push(exp);
            if let Some(Comma) = self.peek() {
                self.next();
            } else {
                break;
            }
        }
        Ok(args)
    }

    fn body(&mut self) -> ParseResult<Vec<Expression>> {
        use tokenizer::Token::*;
        let mut exprs = Vec::new();
        loop {
            match self.peek() {
                Some(ref t) if is_delimiter(t) => {
                    self.next();
                    continue;
                }
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
        let lhs: Expression = try!(self.primary_expression());
        self.precedence_expr(lhs, 0)
    }

    fn precedence_expr(&mut self,
                       mut lhs: Expression,
                       min_precedence: u8)
                       -> ParseResult<Expression> {
        use tokenizer::Token::*;
        while let Some(Operator(op)) = self.peek() {
            let precedence = self.get_precedence(&op);
            if precedence < min_precedence {
                break;
            }
            self.next();
            let mut rhs = try!(self.primary_expression());
            while let Some(Operator(ref op)) = self.peek() {
                if self.get_precedence(op) > precedence {
                    let new_prec = self.get_precedence(op);
                    rhs = try!(self.precedence_expr(rhs, new_prec));

                } else {
                    break;
                }
            }

            lhs = Expression::BinExp(op.0, Box::new(lhs), Box::new(rhs));
        }

        Ok(lhs)
    }

    fn primary_expression(&mut self) -> ParseResult<Expression> {
        use tokenizer::Token::*;
        Ok(match self.peek() {
            Some(Keyword(Kw::Null)) => {
                self.next();
                Expression::Null
            }
            Some(NumLiteral(n)) => {
                self.next();
                Expression::Number(n)
            }
            Some(StrLiteral(s)) => {
                self.next();
                Expression::StringLiteral(s)
            }
            Some(Keyword(Kw::If)) => try!(self.conditional_expr()),
            Some(Identifier(_)) => try!(self.identifier_expr()),
            Some(Token::LParen) => try!(self.paren_expr()),
            Some(e) => {
                return ParseError::result_from_str(&format!("Expected primary expression, got \
                                                             {:?}",
                                                            e));
            }
            None => return ParseError::result_from_str("Expected primary expression received EoI"),
        })
    }

    fn conditional_expr(&mut self) -> ParseResult<Expression> {
        use tokenizer::Token::*;
        use self::Expression::*;
        expect!(self, Keyword(Kw::If));
        let test = try!(self.expression());
        expect!(self, Keyword(Kw::Then));
        let mut then_block = VecDeque::new();
        loop {
            match self.peek() {
                None |
                Some(Keyword(Kw::Else)) |
                Some(Keyword(Kw::End)) => break,
                Some(Semicolon) | Some(Newline) => {
                    self.next();
                    continue;
                }
                _ => {
                    let exp = try!(self.expression());
                    then_block.push_back(exp);
                }
            }
        }
        let else_block = if let Some(Keyword(Kw::Else)) = self.peek() {
            self.next();
            let mut else_exprs = VecDeque::new();
            loop {
                match self.peek() {
                    None |
                    Some(Keyword(Kw::End)) => break,
                    Some(Semicolon) | Some(Newline) => {
                        self.next();
                        continue;
                    }
                    _ => {
                        let exp = try!(self.expression());
                        else_exprs.push_back(exp);
                    }
                }
            }
            Some(else_exprs)
        } else {
            None
        };

        expect!(self, Keyword(Kw::End));
        Ok(Conditional(Box::new(test),
                       Box::new(Block(then_block)),
                       else_block.map(|list| Box::new(Block(list)))))
    }

    fn identifier_expr(&mut self) -> ParseResult<Expression> {
        use tokenizer::Token::*;
        let name = expect_identifier!(self);
        let expr = match self.peek() {
            Some(LParen) => {
                let args = try!(self.call_expr());
                Expression::Call(name, args)
            }
            __ => Expression::Variable(name),
        };

        Ok(expr)
    }

    fn call_expr(&mut self) -> ParseResult<Vec<Expression>> {
        use tokenizer::Token::*;
        expect!(self, LParen);
        let args: Vec<Expression> = try!(self.exprlist());
        expect!(self, RParen);
        Ok(args)
    }

    fn paren_expr(&mut self) -> ParseResult<Expression> {
        expect!(self, Token::LParen);
        let expr = try!(self.expression());
        expect!(self, Token::RParen);
        Ok(expr)
    }
}

pub fn parse(tokens: &[Token], _parsed_tree: &[ASTNode]) -> ParseResult<AST> {
    let mut parser = Parser::initialize(tokens);
    parser.program()
}

#[cfg(test)]
mod tests {
    use tokenizer;
    use super::*;

    macro_rules! parsetest {
        ($input:expr, $output:pat, $ifexpr:expr) => {
            {
            let tokens = tokenizer::tokenize($input).unwrap();
            let ast = parse(&tokens, &[]).unwrap();
            match &ast[..] {
                $output if $ifexpr => (),
                x => panic!("Error in parse test, got {:?} instead", x)
            }
            }
        }
    }

    #[test]
    fn call_parse_test() {
        use super::ASTNode::*;
        use super::Expression::*;
        use super::Function;

        parsetest!(
        "fn a() 1 + 2 end",
        &[FuncNode(Function {prototype: Prototype { ref name, ref parameters }, ref body})],
        match &body[..] { &[BinExp(_, box Number(1.0), box Number(2.0))] => true, _ => false }
            && name == "a" && match &parameters[..] { &[] => true, _ => false }
        );

        parsetest!(
        "fn a(x,y) 1 + 2 end",
        &[FuncNode(Function {prototype: Prototype { ref name, ref parameters }, ref body})],
        match &body[..] { &[BinExp(_, box Number(1.0), box Number(2.0))] => true, _ => false }
            && name == "a" && *parameters == ["x","y"]
        );
    }

    #[test]
    fn expression_parse_test() {
        use super::ASTNode::*;
        use super::Expression::*;
        parsetest!("a", &[ExprNode(Variable(ref s))], s == "a");
        parsetest!("a + b",
            &[ExprNode(BinExp(ref plus, box Variable(ref a), box Variable(ref b)))],
            plus == "+" && a == "a" && b == "b");
        parsetest!("a + b * c",
            &[ExprNode(BinExp(ref plus, box Variable(ref a), box BinExp(ref mul, box Variable(ref b), box Variable(ref c))))],
            plus == "+" && mul == "*" && a == "a" && b == "b" && c == "c");
        parsetest!("a * b + c",
            &[ExprNode(BinExp(ref plus, box BinExp(ref mul, box Variable(ref a), box Variable(ref b)), box Variable(ref c)))],
            plus == "+" && mul == "*" && a == "a" && b == "b" && c == "c");
        parsetest!("(a + b) * c",
            &[ExprNode(BinExp(ref mul, box BinExp(ref plus, box Variable(ref a), box Variable(ref b)), box Variable(ref c)))],
            plus == "+" && mul == "*" && a == "a" && b == "b" && c == "c");
    }

    #[test]
    fn conditional_parse_test() {
        use tokenizer;
        use super::ASTNode::*;
        use super::Expression::*;

        let t1 = "if null then 20 else 40 end";
        let tokens = tokenizer::tokenize(t1).unwrap();
        match parse(&tokens, &[]).unwrap()[..] {
            [ExprNode(Conditional(box Null, box Block(_), Some(box Block(_))))] => (),
            _ => panic!(),
        }
    }
}
