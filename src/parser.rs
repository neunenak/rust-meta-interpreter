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

impl<'a> Parser<'a> {

    fn parse(&mut self) -> ParseResult {
        let r = self.expr();
        match self.expect(Token::Separator) {
            None => (),
            Some(err) => return Err(err)
        }
        match self.expect(Token::EOF) {
            None => (),
            Some(err) => return Err(err)
        }
        r
    }

    fn expect(&mut self, expected: Token) -> Option<ParseError> {
        match self.tokens.next() {
            Some(next) if *next == expected => None,
            Some(next) => {
                let err = format!("Expected {:?} but got {:?}", expected, next);
                Some(ParseError { err: err })
            }
            None => {
                let err = format!("Expected {:?} but got end of input", expected);
                Some(ParseError { err: err })
            }
        }
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

