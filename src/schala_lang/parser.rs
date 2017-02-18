use schala_lang::tokenizer::{Token, Kw, OpTok};
use schala_lang::tokenizer::Token::*;

use std::fmt;
use std::collections::VecDeque;
use std::rc::Rc;
use std::convert::From;

// Grammar
// program := (statement delimiter ?)*
// delimiter := Newline | Semicolon
// statement := declaration | expression
// declaration :=  FN prototype LCurlyBrace (statement)* RCurlyBrace
// prototype := identifier LParen identlist RParen
// identlist := Ident (Comma Ident)* | e
// exprlist  := Expression (Comma Expression)* | e
//
// expression := postop_expression (op postop_expression)*
// postop_expression := primary_expression postop
// primary_expression :=  number_expr | String | identifier_expr | paren_expr | conditional_expr | while_expr | lambda_expr | list_expr
// number_expr := (PLUS | MINUS ) number_expr | Number
// identifier_expr := call_expression | Variable
// list_expr := LSquareBracket exprlist RSquareBracket
// call_expression := Identifier LParen exprlist RParen
// while_expr := WHILE primary_expression LCurlyBrace (expression delimiter)* RCurlyBrace
// paren_expr := LParen expression RParen
// conditional_expr := IF expression LCurlyBrace (expression delimiter)* RCurlyBrace (LCurlyBrace (expresion delimiter)* RCurlyBrace)?
// lambda_expr := FN LParen identlist RParen LCurlyBrace (expression delimiter)* RCurlyBrace
// lambda_call :=  | LParen exprlist RParen
// postop := ε | LParen exprlist RParen | LBracket expression RBracket
// op := '+', '-', etc.
//

pub type AST = Vec<Statement>;

#[derive(Debug, Clone)]
pub enum Statement {
    ExprNode(Expression),
    FuncDefNode(Function),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Statement::*;
        match *self {
            ExprNode(ref expr) => write!(f, "{}", expr),
            FuncDefNode(_) => write!(f, "UNIMPLEMENTED"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub prototype: Prototype,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Prototype {
    pub name: Rc<String>,
    pub parameters: Vec<Rc<String>>,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Null,
    StringLiteral(Rc<String>),
    Number(f64),
    Variable(Rc<String>),
    BinExp(BinOp, Box<Expression>, Box<Expression>),
    Call(Callable, Vec<Expression>),
    Conditional(Box<Expression>, Box<Expression>, Option<Box<Expression>>),
    Lambda(Function),
    Block(VecDeque<Expression>),
    While(Box<Expression>, Vec<Expression>),
    Index(Box<Expression>, Box<Expression>),
    ListLiteral(VecDeque<Expression>),
}

#[derive(Clone, Debug)]
pub enum Callable {
    NamedFunction(Rc<String>),
    Lambda(Function),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Expression::*;
        match *self {
            Null => write!(f, "null"),
            StringLiteral(ref s) => write!(f, "\"{}\"", s),
            Number(n) => write!(f, "{}", n),
            Lambda(Function { prototype: Prototype { ref name, ref parameters, .. }, .. }) => {
                write!(f, "«function: {}, {} arg(s)»", name, parameters.len())
            }
            ListLiteral(ref items) => {
                write!(f, "[");
                for item in items {
                    write!(f, ", {}", item);
                }
                write!(f, "]")
            }
            _ => write!(f, "UNIMPLEMENTED"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum BinOp {
    Add,
    AddAssign,
    Sub,
    SubAssign,
    Mul,
    MulAssign,
    Div,
    DivAssign,
    Mod,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    Equal,
    Assign,
    Custom(String),
}

impl From<OpTok> for BinOp {
    fn from(token: OpTok) -> BinOp {
        use self::BinOp::*;
        match &token.0[..] {
            "+" => Add,
            "+=" => AddAssign,
            "-" => Sub,
            "-=" => SubAssign,
            "*" => Mul,
            "*=" => MulAssign,
            "/" => Div,
            "/=" => DivAssign,
            "%" => Mod,
            "<" => Less,
            "<=" => LessEq,
            ">" => Greater,
            ">=" => GreaterEq,
            "==" => Equal,
            "=" => Assign,
            op => Custom(op.to_string()),
        }
    }
}

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

    fn get_precedence(&self, op: &OpTok) -> Precedence {
        match &op.0[..] {
            "+" => 10,
            "-" => 10,
            "*" => 20,
            "/" => 20,
            "%" => 20,
            "==" => 40,
            "=" | "+=" | "-=" | "*=" | "/=" => 1,
            ">" | ">=" | "<" | "<=" => 30,
            _ => 255,
        }
    }
}

macro_rules! expect {
    ($self_:expr, $token:pat) => {
        match $self_.peek() {
            Some($token) => {$self_.next();},
            Some(x) => {
                let err = format!("Expected `{:?}` but got `{:?}`", stringify!($token), x);
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
            Some(x) => return ParseError::result_from_str(&format!("Expected identifier, but got {:?}", x)),
            None => return ParseError::result_from_str("Expected identifier, but got end of input"),
        }
    }
}

macro_rules! skip_whitespace {
    ($_self: expr) => {
        loop {
            match $_self.peek() {
                Some(ref t) if is_delimiter(t) => {
                    $_self.next();
                    continue;
                }
                _ => break,
            }
        }
    }
}

macro_rules! delimiter_block {
    ($_self: expr, $try_parse: ident, $($break_pattern: pat)|+) => {
        {
        let mut acc = Vec::new();
        loop {
            match $_self.peek() {
                None => break,
                Some(ref t) if is_delimiter(t) => { $_self.next(); continue; },
                $($break_pattern)|+ => break,
                _ => {
                    let a = try!($_self.$try_parse());
                    acc.push(a);
                }
            }
        }
        acc
        }
    }
}

fn is_delimiter(token: &Token) -> bool {
    match *token {
        Newline | Semicolon => true,
        _ => false,
    }
}

impl Parser {
    fn program(&mut self) -> ParseResult<AST> {
        let mut ast = Vec::new(); //TODO have this come from previously-parsed tree
        loop {
            let result: ParseResult<Statement> = match self.peek() {
                Some(ref t) if is_delimiter(t) => {
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

    fn statement(&mut self) -> ParseResult<Statement> {
        let node: Statement = match self.peek() {
            Some(Keyword(Kw::Fn)) => self.declaration()?,
            Some(_) => Statement::ExprNode(self.expression()?),
            None => panic!("Unexpected end of tokens"),
        };
        Ok(node)
    }

    fn declaration(&mut self) -> ParseResult<Statement> {
        expect!(self, Keyword(Kw::Fn));
        let prototype = self.prototype()?;
        expect!(self, LCurlyBrace);
        let body = self.body()?;
        expect!(self, RCurlyBrace);
        Ok(Statement::FuncDefNode(Function {
            prototype: prototype,
            body: body,
        }))
    }

    fn prototype(&mut self) -> ParseResult<Prototype> {
        let name = expect_identifier!(self);
        expect!(self, LParen);
        let parameters = self.identlist()?;
        expect!(self, RParen);
        Ok(Prototype {
            name: name,
            parameters: parameters,
        })
    }

    fn identlist(&mut self) -> ParseResult<Vec<Rc<String>>> {
        let mut args = Vec::new();
        while let Some(Identifier(name)) = self.peek() {
            args.push(name.clone());
            self.next();
            match self.peek() {
                Some(Comma) => {self.next();},
                _ => break,
            }
        }
        Ok(args)
    }

    fn exprlist(&mut self) -> ParseResult<Vec<Expression>> {
        let mut exprs = Vec::new();
        loop {
            if let Some(RParen) = self.peek() {
                break;
            }
            let exp = self.expression()?;
            exprs.push(exp);
            match self.peek() {
                Some(Comma) => {self.next();},
                _ => break,
            }
        }
        Ok(exprs)
    }

    fn body(&mut self) -> ParseResult<Vec<Statement>> {
        let statements = delimiter_block!(
            self,
            statement,
            Some(RCurlyBrace)
        );
        Ok(statements)
    }

    fn expression(&mut self) -> ParseResult<Expression> {
        let lhs: Expression = self.postop_expression()?;
        self.precedence_expr(lhs, 0)
    }

    fn precedence_expr(&mut self,
                       mut lhs: Expression,
                       min_precedence: u8)
                       -> ParseResult<Expression> {
        while let Some(Operator(op)) = self.peek() {
            let precedence = self.get_precedence(&op);
            if precedence < min_precedence {
                break;
            }
            self.next();
            let mut rhs = self.postop_expression()?;
            while let Some(Operator(ref op)) = self.peek() {
                if self.get_precedence(op) > precedence {
                    let new_prec = self.get_precedence(op);
                    rhs = self.precedence_expr(rhs, new_prec)?;

                } else {
                    break;
                }
            }

            lhs = Expression::BinExp(op.into(), Box::new(lhs), Box::new(rhs));
        }
        Ok(lhs)
    }

    fn postop_expression(&mut self) -> ParseResult<Expression> {
        use self::Expression::*;
        let expr = self.primary_expression()?;
        let ret = match self.peek() {
            Some(LParen) => {
                let args = self.call_expression()?;
                match expr {
                    Lambda(f) => Call(Callable::Lambda(f), args),
                    e => {
                        let err = format!("Expected lambda expression before a call, got {:?}", e);
                        return ParseError::result_from_str(&err);
                    },
                }
            },
            Some(LSquareBracket) => {
                expect!(self, LSquareBracket);
                let index_expr = self.expression()?;
                expect!(self, RSquareBracket);
                Index(Box::new(expr), Box::new(index_expr))
            },
            _ => {
                expr
            }
        };
        Ok(ret)
    }

    fn primary_expression(&mut self) -> ParseResult<Expression> {
        Ok(match self.peek() {
            Some(Keyword(Kw::Null)) => {
                self.next();
                Expression::Null
            }
            Some(NumLiteral(_)) => self.number_expression()?,
            Some(Operator(OpTok(ref a))) if **a == "+" || **a == "-" => self.number_expression()?,
            Some(StrLiteral(s)) => {
                self.next();
                Expression::StringLiteral(s)
            }
            Some(Keyword(Kw::If)) => self.conditional_expr()?,
            Some(Keyword(Kw::While)) => self.while_expr()?,
            Some(Identifier(_)) => self.identifier_expr()?,
            Some(Token::LParen) => self.paren_expr()?,
            Some(Keyword(Kw::Fn)) => self.lambda_expr()?,
            Some(Token::LSquareBracket) => self.list_expr()?,
            Some(e) => {
                return ParseError::result_from_str(&format!("Expected primary expression, got \
                                                             {:?}",
                                                            e));
            }
            None => return ParseError::result_from_str("Expected primary expression received EoI"),
        })
    }

    fn list_expr(&mut self) -> ParseResult<Expression> {
        expect!(self, LSquareBracket);
        let exprlist: Vec<Expression> = self.exprlist()?;
        expect!(self, RSquareBracket);

        Ok(Expression::ListLiteral(VecDeque::from(exprlist)))
    }

    fn number_expression(&mut self) -> ParseResult<Expression> {
        let mut multiplier = 1;
        loop {
            match self.peek() {
                Some(NumLiteral(n)) => {
                    self.next();
                    return Ok(Expression::Number(n * multiplier as f64));
                }
                Some(Operator(OpTok(ref a))) if **a == "+" => {
                    self.next();
                }
                Some(Operator(OpTok(ref a))) if **a == "-" => {
                    multiplier *= -1;
                    self.next();
                }
                Some(e) => {
                    return ParseError::result_from_str(
                            &format!("Expected +, - or number, got {:?}", e));
                }
                None => {
                    return ParseError::result_from_str(
                            &format!("Expected +, - or number, got EoI"));
                }
            }
        }
    }

    fn lambda_expr(&mut self) -> ParseResult<Expression> {
        use self::Expression::*;
        expect!(self, Keyword(Kw::Fn));
        skip_whitespace!(self);
        expect!(self, LParen);
        let parameters = self.identlist()?;
        expect!(self, RParen);
        skip_whitespace!(self);
        expect!(self, LCurlyBrace);
        let body = self.body()?;
        expect!(self, RCurlyBrace);

        let prototype = Prototype {
            name: Rc::new("a lambda yo!".to_string()),
            parameters: parameters,
        };

        let function = Function {
            prototype: prototype,
            body: body,
        };

        Ok(Lambda(function))
    }

    fn while_expr(&mut self) -> ParseResult<Expression> {
        use self::Expression::*;
        expect!(self, Keyword(Kw::While));
        let test = self.expression()?;
        expect!(self, LCurlyBrace);
        let body = delimiter_block!(
            self,
            expression,
            Some(RCurlyBrace)
        );
        expect!(self, RCurlyBrace);
        Ok(While(Box::new(test), body))
    }

    fn conditional_expr(&mut self) -> ParseResult<Expression> {
        use self::Expression::*;
        expect!(self, Keyword(Kw::If));
        let test = self.expression()?;
        skip_whitespace!(self);
        expect!(self, LCurlyBrace);
        skip_whitespace!(self);
        let then_block = delimiter_block!(
            self,
            expression,
            Some(RCurlyBrace)
        );
        expect!(self, RCurlyBrace);
        skip_whitespace!(self);
        let else_block = if let Some(Keyword(Kw::Else)) = self.peek() {
            self.next();
            skip_whitespace!(self);
            expect!(self, LCurlyBrace);
            let else_exprs  = delimiter_block!(
                self,
                expression,
                Some(RCurlyBrace)
            );
            Some(else_exprs)
        } else {
            None
        };
        expect!(self, RCurlyBrace);
        Ok(Conditional(Box::new(test),
                       Box::new(Block(VecDeque::from(then_block))),
                       else_block.map(|list| Box::new(Block(VecDeque::from(list))))))
    }

    fn identifier_expr(&mut self) -> ParseResult<Expression> {
        let name = expect_identifier!(self);
        let expr = match self.peek() {
            Some(LParen) => {
                let args = self.call_expression()?;
                Expression::Call(Callable::NamedFunction(name), args)
            }
            __ => Expression::Variable(name),
        };
        Ok(expr)
    }

    fn call_expression(&mut self) -> ParseResult<Vec<Expression>> {
        expect!(self, LParen);
        let args: Vec<Expression> = self.exprlist()?;
        expect!(self, RParen);
        Ok(args)
    }

    fn paren_expr(&mut self) -> ParseResult<Expression> {
        expect!(self, Token::LParen);
        let expr = self.expression()?;
        expect!(self, Token::RParen);
        Ok(expr)
    }
}

pub fn parse(tokens: &[Token], _parsed_tree: &[Statement]) -> ParseResult<AST> {
    let mut parser = Parser::initialize(tokens);
    parser.program()
}

#[cfg(test)]
mod tests {
    use schala_lang::tokenizer;
    use super::*;
    use super::Statement::*;
    use super::Expression::*;

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
    fn function_parse_test() {
        use super::Function;
        parsetest!(
        "fn a() { 1 + 2 }",
        &[FuncDefNode(Function {prototype: Prototype { ref name, ref parameters }, ref body})],
        match &body[..] { &[ExprNode(BinExp(_, box Number(1.0), box Number(2.0)))] => true, _ => false }
            && **name == "a" && match &parameters[..] { &[] => true, _ => false }
        );

        parsetest!(
        "fn a(x,y){ 1 + 2 }",
        &[FuncDefNode(Function {prototype: Prototype { ref name, ref parameters }, ref body})],
        match &body[..] { &[ExprNode(BinExp(_, box Number(1.0), box Number(2.0)))] => true, _ => false }
            && **name == "a" && *parameters[0] == "x" && *parameters[1] == "y" && parameters.len() == 2
        );

        let t3 = "fn (x) { x + 2 }";
        let tokens3 = tokenizer::tokenize(t3).unwrap();
        assert!(parse(&tokens3, &[]).is_err());
    }

    #[test]
    fn expression_parse_test() {
        parsetest!("a", &[ExprNode(Variable(ref s))], **s == "a");
        parsetest!("a + b",
            &[ExprNode(BinExp(BinOp::Add, box Variable(ref a), box Variable(ref b)))],
            **a == "a" && **b == "b");
        parsetest!("a + b * c",
            &[ExprNode(BinExp(BinOp::Add, box Variable(ref a), box BinExp(BinOp::Mul, box Variable(ref b), box Variable(ref c))))],
            **a == "a" && **b == "b" && **c == "c");
        parsetest!("a * b + c",
            &[ExprNode(BinExp(BinOp::Add, box BinExp(BinOp::Mul, box Variable(ref a), box Variable(ref b)), box Variable(ref c)))],
            **a == "a" && **b == "b" && **c == "c");
        parsetest!("(a + b) * c",
            &[ExprNode(BinExp(BinOp::Mul, box BinExp(BinOp::Add, box Variable(ref a), box Variable(ref b)), box Variable(ref c)))],
            **a == "a" && **b == "b" && **c == "c");
    }

    #[test]
    fn lambda_parse_test() {
        use schala_lang::tokenizer;
        let t1 = "(fn(x) { x + 2 })";
        let tokens1 = tokenizer::tokenize(t1).unwrap();
        match parse(&tokens1, &[]).unwrap()[..] {
            _ => (),
        }

        let t2 = "fn(x) { x + 2 }";
        let tokens2 = tokenizer::tokenize(t2).unwrap();
        assert!(parse(&tokens2, &[]).is_err());

        let t3 = "(fn(x) { x + 10 })(20)";
        let tokens3 = tokenizer::tokenize(t3).unwrap();
        match parse(&tokens3, &[]).unwrap() {
            _ => (),
        };
    }

    #[test]
    fn conditional_parse_test() {
        use schala_lang::tokenizer;
        let t1 = "if null { 20 } else { 40 }";
        let tokens = tokenizer::tokenize(t1).unwrap();
        match parse(&tokens, &[]).unwrap()[..] {
            [ExprNode(Conditional(box Null, box Block(_), Some(box Block(_))))] => (),
            _ => panic!(),
        }

        let t2 = r"
        if null {
            20
        } else {
            40
        }
        ";
        let tokens2 = tokenizer::tokenize(t2).unwrap();
        match parse(&tokens2, &[]).unwrap()[..] {
            [ExprNode(Conditional(box Null, box Block(_), Some(box Block(_))))] => (),
            _ => panic!(),
        }

        let t2 = r"
        if null {
            20 } else
        {
            40
        }
        ";
        let tokens3 = tokenizer::tokenize(t2).unwrap();
        match parse(&tokens3, &[]).unwrap()[..] {
            [ExprNode(Conditional(box Null, box Block(_), Some(box Block(_))))] => (),
            _ => panic!(),
        }
    }
}
