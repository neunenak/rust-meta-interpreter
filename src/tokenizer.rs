extern crate itertools;

use std::iter::Peekable;
use std::str::Chars;
use self::itertools::Itertools;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Newline,
    Semicolon,
    LParen,
    RParen,
    Comma,
    Period,
    Colon,
    NumLiteral(f64),
    StrLiteral(String),
    Identifier(String),
    Operator(Op),
    Keyword(Kw),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Op(pub String);

#[derive(Debug, Clone, PartialEq)]
pub enum Kw {
    If,
    Then,
    Else,
    While,
    End,
    Let,
    Fn,
    Null,
}

pub type TokenizeResult = Result<Vec<Token>, TokenizeError>;

#[derive(Debug)]
pub struct TokenizeError {
    pub msg: String,
}

impl TokenizeError {
    fn new(msg: &str) -> TokenizeError {
        TokenizeError { msg: msg.to_string() }
    }
}

fn is_digit(c: &char) -> bool {
    c.is_digit(10)
}

pub fn tokenize(input: &str) -> TokenizeResult {
    use self::Token::*;
    let mut tokens = Vec::new();
    let mut iter: Peekable<Chars> = input.chars().peekable();
    while let Some(c) = iter.next() {
        if c == '#' {
            while let Some(c) = iter.next() {
                if c == '\n' {
                    break;
                }
            }
        }
        let cur_tok = match c {
            c if char::is_whitespace(c) && c != '\n' => continue,
            '\n' => Newline,
            ';' => Semicolon,
            '(' => LParen,
            ')' => RParen,
            ':' => Colon,
            ',' => Comma,
            '"' => try!(tokenize_str(&mut iter)),
            c if !char::is_alphanumeric(c) => try!(tokenize_operator(c, &mut iter)),
            c @ '.' | c if is_digit(&c) => try!(tokenize_number_or_period(c, &mut iter)),
            c => try!(tokenize_identifier(c, &mut iter)),
        };
        tokens.push(cur_tok);
    }
    Ok(tokens)
}

fn tokenize_str(iter: &mut Peekable<Chars>) -> Result<Token, TokenizeError> {
    let mut buffer = String::new();
    loop {
        // TODO handle string escapes, interpolation
        match iter.next() {
            Some(x) if x == '"' => break,
            Some(x) => buffer.push(x),
            None => return Err(TokenizeError::new("Unclosed quote")),
        }
    }
    Ok(Token::StrLiteral(buffer))
}

fn tokenize_operator(c: char, iter: &mut Peekable<Chars>) -> Result<Token, TokenizeError> {
    let mut buffer = String::new();
    buffer.push(c);
    buffer.extend(iter.peeking_take_while(|x| !char::is_alphanumeric(*x) && !char::is_whitespace(*x))); 
    Ok(Token::Operator(Op(buffer)))
}

fn tokenize_number_or_period(c: char, iter: &mut Peekable<Chars>) -> Result<Token, TokenizeError> {
    if c == '.' && !iter.peek().map_or(false, is_digit) {
        return Ok(Token::Period);
    }

    let mut buffer = String::new();
    buffer.push(c);
    buffer.extend(iter.peeking_take_while(|x| is_digit(x) || *x == '.'));

    match buffer.parse::<f64>() {
        Ok(f) => Ok(Token::NumLiteral(f)),
        Err(_) => Err(TokenizeError::new("Failed to parse digit")),
    }
}

fn tokenize_identifier(c: char, iter: &mut Peekable<Chars>) -> Result<Token, TokenizeError> {
    fn ends_identifier(c: &char) -> bool {
        let c = *c;
        char::is_whitespace(c) || is_digit(&c) || c == ';' || c == '(' || c == ')' ||
        c == ',' || c == '.' || c == ',' || c == ':'
    }

    use self::Token::*;
    let mut buffer = String::new();
    buffer.push(c);
    buffer.extend(iter.peeking_take_while(|x| !ends_identifier(x)));

    Ok(match &buffer[..] {
        "if" => Keyword(Kw::If),
        "then" => Keyword(Kw::Then),
        "else" => Keyword(Kw::Else),
        "while" => Keyword(Kw::While),
        "end" => Keyword(Kw::End),
        "let" => Keyword(Kw::Let),
        "fn" => Keyword(Kw::Fn),
        "null" => Keyword(Kw::Null),
        b => Identifier(b.to_string()),
    })
}

#[cfg(test)]
mod tests {

    macro_rules! tokentest {
        ($input:expr, $output:expr) => {
            {
            let tokens = tokenize($input).unwrap();
            assert_eq!(format!("{:?}", tokens), $output);
            }
        }
    }

    use super::*;

    #[test]
    fn tokeniziation_tests() {
        tokentest!("let a = 3\n",
                   "[Keyword(Let), Identifier(\"a\"), Operator(Op(\"=\")), \
                    NumLiteral(3), Newline]");

        tokentest!("2+1",
                   "[NumLiteral(2), Operator(Op(\"+\")), NumLiteral(1)]");

        tokentest!("2 + 1",
                   "[NumLiteral(2), Operator(Op(\"+\")), NumLiteral(1)]");

        tokentest!("2.3*49.2",
                   "[NumLiteral(2.3), Operator(Op(\"*\")), NumLiteral(49.2)]");

        assert!(tokenize("2.4.5").is_err());
    }

    #[test]
    #[ignore]
    fn more_tokenization() {
        // it would be nice to support complicated operators in a nice, haskell-ish way
        tokentest!("a *> b",
                   "[Identifier(\"a\"), Identifier(\"*>\"), Identifier(\"b\"), EOF]");

    }
}
