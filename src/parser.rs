use std::iter::Peekable;
use std::slice::Iter;

use tokenizer::Token;

#[derive(Debug)]
enum AST {
    BinOp(Box<AST>, Box<AST>, Box<AST>),
    Number(f64),
    Name(String),
}

#[derive(Debug)]
pub struct ParseError {
    err: String
}

pub type ParseResult = Result<AST, ParseError>;

/* grammar

   expr : term ((PLUS|MIMUS) term)*
   term : factor ((MUL | DIV) factor)*
   factor  : NUM | LPAREN expr RPAREN

*/

struct Parser<'a> {
    tokens: Peekable<Iter<'a, Token>>
}

macro_rules! expect {
    ($tok:expr, $self_:ident) => {
        match $self_.tokens.next() {
            Some(next) if *next == $tok => (),
            Some(next) => {
                let err = format!("Expected {:?} but got {:?}", $tok, next);
                return Err(ParseError { err: err });
            },
            None => {
                let err = format!("Expected {:?} but got end of input", $tok);
                return Err(ParseError { err: err });
            }
        }
    }
}

impl<'a> Parser<'a> {

    fn parse(&mut self) -> ParseResult {
        let r = self.expr();
        expect!(Token::Separator, self);
        expect!(Token::EOF, self);
        r
    }

    fn expr(&mut self) -> ParseResult {
        self.tokens.next();
        return Ok(AST::Number(5.0));
    }
}

pub fn parse(input: Vec<Token>) -> ParseResult {
    let mut iter = input.iter().peekable();
    let mut parser = Parser { tokens: iter };
    return parser.parse();
}

