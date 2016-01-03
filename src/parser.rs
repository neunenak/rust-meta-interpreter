use std::iter::Peekable;
use std::vec::IntoIter;
use std::fmt;

use tokenizer::Token;

#[derive(Debug)]
pub enum AST {
    BinOp(Box<AST>, Box<AST>, Box<AST>),
    Number(f64),
    Name(String),
    Block(Vec<AST>),
    Definition(String, Box<AST>),
}

impl fmt::Display for AST {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &AST::Number(ref n) => write!(f, "{}", n),
            &AST::Name(ref s) => write!(f, "{}", s),
            astnode => write!(f, "UNEXPANDED AST NODE: {:?}", astnode)
        }
    }
}

#[derive(Debug)]
pub struct ParseError {
    err: String
}

pub type ParseResult<T> = Result<T, ParseError>;

/* grammar

   program : block EOF
   block : (statement sep)+
   sep : NEWLINE | SEMICOLON
   statement: expr | definition
   definition: 'let' NAME '=' expr
   expr : term ((PLUS|MIMUS) term)*
   term : factor ((MUL | DIV) factor)*
   factor  : NUM | LPAREN expr RPAREN

*/

struct Parser {
    tokens: Peekable<IntoIter<Token>>
}

macro_rules! parse_error {
    ($($text:tt)*) => {
        Err(ParseError { err: format!($($text)*) })
    }
}

impl Parser {
    fn next(&mut self) -> Option<Token> {
        self.tokens.next()
    }

    //TODO see if I can get rid of the need for this move
    fn lookahead(&mut self) -> Option<Token> {
        self.tokens.peek().map(|x| x.clone())
    }
}

impl Parser {

    fn expect(&mut self, expected: Token) -> ParseResult<()> {
        match self.next() {
            Some(ref next) if *next == expected => Ok(()),
            Some(next) => {
                return parse_error!("Expected {:?} but got {:?}", expected, next);
            },
            None => {
                return parse_error!("Expected {:?} but got end of input", expected);
            }
        }
    }


    fn expect_identifier(&mut self) -> ParseResult<String> {
        use tokenizer::Token::*;
        match self.next() {
            Some(Identifier(ref s)) => Ok(s.to_string()),
            Some(next) => {
                return parse_error!("Expected identifier but got {:?}", next);
            }
            None => {
                return parse_error!("Expected identifier but got end of input");
            }
        }
    }

    fn expect_num_literal(&mut self) -> ParseResult<f64> {
        use tokenizer::Token::*;
        match self.next() {
            Some(NumLiteral(f)) => Ok(f),
            Some(t) => {
                return parse_error!("Expected NumLiteral, but got {:?}", t);
            },
            None => {
                return parse_error!("Expected NumLiteral but got end of input");
            }
        }
    }

    fn parse(&mut self) -> ParseResult<AST> {
        let r = self.block();
        try!(self.expect(Token::EOF));
        r
    }

    fn block(&mut self) -> ParseResult<AST> {
        use tokenizer::Token::*;
        let mut block_nodes: Vec<AST> = Vec::new();
        loop {
            let s: AST = try!(self.statement());
            block_nodes.push(s);
            match self.lookahead() {
                Some(Semicolon) | Some(Newline) => {
                    self.next();
                    if let Some(EOF) = self.lookahead() {
                        break
                    }
                },
                _ => break
            }
        }

        Ok(AST::Block(block_nodes))
    }

    fn statement(&mut self) -> ParseResult<AST> {
        use tokenizer::Token::*;
        use tokenizer::Kw;
        let r = match self.lookahead() {
            Some(Keyword(Kw::Let)) => try!(self.definition()),
            _ => try!(self.expr())
        };
        Ok(r)
    }

    fn definition(&mut self) -> ParseResult<AST> {
        use tokenizer::Token::*;
        use tokenizer::Kw;
        try!(self.expect(Keyword(Kw::Let)));
        let name = try!(self.expect_identifier());
        match self.lookahead() {
            Some(Identifier(ref s)) if s == "=" => { self.next(); },
            _ => return parse_error!("Expected `=`"),
        }

        let expr = try!(self.expr());

        Ok(AST::Definition(name, Box::new(expr)))
    }

    fn expr(&mut self) -> ParseResult<AST> {
        use tokenizer::Token::*;
        let mut lhs = try!(self.term());
        loop {
            match self.lookahead() {
                Some(Identifier(ref s)) if s == "+" || s == "-" => {
                    let op_token = self.next().unwrap();
                    let op = AST::Name(match op_token { Identifier(s) => s, _ => panic!("lol") });
                    let rhs = try!(self.term());
                    lhs = AST::BinOp(
                        Box::new(lhs),
                        Box::new(op),
                        Box::new(rhs));
                },
                _ => break
            }
        }
        Ok(lhs)
    }

    fn term(&mut self) -> ParseResult<AST> {
        use tokenizer::Token::*;
        let mut lhs = try!(self.factor());
        loop {
            match self.lookahead() {
                Some(Identifier(ref s)) if s == "*" || s == "/" => {
                    let op_token = self.next().unwrap();
                    let op = AST::Name(match op_token { Identifier(s) => s, _ => panic!("lol") });
                    let rhs = try!(self.factor());
                    lhs = AST::BinOp(
                        Box::new(lhs),
                        Box::new(op),
                        Box::new(rhs));
                },
                _ => break
            }
        }
        Ok(lhs)
    }

    fn factor(&mut self) -> ParseResult<AST> {
        use tokenizer::Token::*;
        match self.lookahead() {
            Some(LParen) => {
                self.next();
                let expr = try!(self.expr());
                try!(self.expect(RParen));
                Ok(expr)
            },
            Some(NumLiteral(n)) => {
                self.next();
                Ok(AST::Number(n))
            },
            x => parse_error!("Expected LParen or NumLiteral, got {:?}", x )
        }
    }
}

pub fn parse(input: Vec<Token>) -> ParseResult<AST> {
    let iter = input.into_iter().peekable();
    let mut parser = Parser { tokens: iter };
    return parser.parse();
}

