use std::iter::Peekable;
use std::slice::Iter;

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

struct Parser<'a> {
    tokens: Peekable<Iter<'a, Token>>
}

impl<'a> Parser<'a> {

    fn expect(&mut self, expected: Token) -> ParseResult<()> {
        match self.tokens.next() {
            Some(next) if *next == expected => Ok(()),
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

    fn parse(&mut self) -> ParseResult<AST> {
        let r = self.expr();
        try!(self.expect(Token::Separator));
        try!(self.expect(Token::EOF));
        r
    }

    fn expr(&mut self) -> ParseResult<AST> {
        self.tokens.next();
        return Ok(AST::Number(5.0));
    }
}

pub fn parse(input: Vec<Token>) -> ParseResult<AST> {
    let mut iter = input.iter().peekable();
    let mut parser = Parser { tokens: iter };
    return parser.parse();
}

