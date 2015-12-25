use tokenizer::Token;

/* grammar

   expr : term ((PLUS|MIMUS) term)*
   term : factor ((MUL | DIV) factor)*
   factor  : NUM | LPAREN expr RPAREN

*/

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

pub fn parse(input: Vec<Token>) -> ParseResult {
    let mut current_token: Token;

    return Err(ParseError { err: "error!".to_string() });
}

