use std::iter::Peekable;
use std::vec::IntoIter;

use tokenizer::Token;

#[derive(Debug)]
pub enum AST {
    BinOp(Box<AST>, Box<AST>, Box<AST>),
    Number(f64),
    Name(String),
}

#[derive(Debug)]
pub struct ParseError {
    err: String
}

pub type ParseResult<T> = Result<T, ParseError>;

/* grammar

   expr : term ((PLUS|MIMUS) term)*
   term : factor ((MUL | DIV) factor)*
   factor  : NUM | LPAREN expr RPAREN

*/

struct Parser {
    tokens: Peekable<IntoIter<Token>>
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
                let err = format!("Expected {:?} but got {:?}", expected, next);
                return Err(ParseError { err: err });
            },
            None => {
                let err = format!("Expected {:?} but got end of input", expected);
                return Err(ParseError { err: err });
            }
        }
    }

    fn expect_identifier(&mut self, identifier_str: &str) -> ParseResult<()> {
        use tokenizer::Token::*;
        match self.next() {
            Some(Identifier(ref s)) if s == identifier_str => Ok(()),
            Some(next) => {
                let err = format!("Expected identifier `{}` but got {:?}", identifier_str, next);
                Err(ParseError { err: err })
            }
            None => {
                let err = format!("Expected identifier `{}` but got end of input", identifier_str);
                Err(ParseError { err: err })
            }
        }
    }

    fn expect_num_literal(&mut self) -> ParseResult<f64> {
        use tokenizer::Token::*;
        match self.next() {
            Some(NumLiteral(f)) => Ok(f),
            Some(t) => {
                let err = format!("Expected NumLiteral, but got {:?}", t);
                Err(ParseError { err: err })
            },
            None => {
                let err = format!("Expected NumLiteral but got end of input");
                Err(ParseError { err: err })
            }
        }
    }

    fn parse(&mut self) -> ParseResult<AST> {
        let r = self.expr();
        try!(self.expect(Token::Separator));
        try!(self.expect(Token::EOF));
        r
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
        let n = try!(self.expect_num_literal());
        Ok(AST::Number(n))
    }
}

pub fn parse(input: Vec<Token>) -> ParseResult<AST> {
    let iter = input.into_iter().peekable();
    let mut parser = Parser { tokens: iter };
    return parser.parse();
}

