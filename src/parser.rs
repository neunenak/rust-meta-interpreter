use tokenizer::Token;

#[derive(Debug)]
pub struct ParseResult {
    msg: i32
}

pub fn parse(tokens: &[Token]) -> ParseResult {

    ParseResult { msg: 0 }
}
