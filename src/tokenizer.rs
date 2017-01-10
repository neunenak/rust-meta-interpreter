extern crate itertools;

use std::iter::Peekable;
use std::str::Chars;
use self::itertools::Itertools;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Newline,
    Semicolon,
    LParen,
    RParen,
    LSquareBracket,
    RSquareBracket,
    LCurlyBrace,
    RCurlyBrace,
    Comma,
    Period,
    Colon,
    NumLiteral(f64),
    StrLiteral(Rc<String>),
    Identifier(Rc<String>),
    Operator(OpTok),
    Keyword(Kw),
}

#[derive(Debug, Clone, PartialEq)]
pub struct OpTok(pub Rc<String>);

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
            '{' => LCurlyBrace,
            '}' => RCurlyBrace,
            '[' => LSquareBracket,
            ']' => RSquareBracket,
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
    Ok(Token::StrLiteral(Rc::new(buffer)))
}

fn tokenize_operator(c: char, iter: &mut Peekable<Chars>) -> Result<Token, TokenizeError> {
    let mut buffer = String::new();
    buffer.push(c);
    buffer.extend(iter.peeking_take_while(|x| !char::is_alphanumeric(*x) && !char::is_whitespace(*x))); 
    Ok(Token::Operator(OpTok(Rc::new(buffer))))
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
        b => Identifier(Rc::new(b.to_string())),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::Token::*;

    macro_rules! token_test {
        ($input: expr, $output: pat, $ifexpr: expr) => {
            let tokens = tokenize($input).unwrap();
            match tokens[..] {
                $output if $ifexpr => (),
                _ => panic!("Actual output: {:?}", tokens),
            }
        }
    }

    #[test]
    fn basic_tokeniziation_tests() {
        token_test!("let a = 3\n",
                    [Keyword(Kw::Let), Identifier(ref a), Operator(OpTok(ref b)), NumLiteral(3.0), Newline],
                    **a == "a" && **b == "=");

        token_test!("2+1",
                    [NumLiteral(2.0), Operator(OpTok(ref a)), NumLiteral(1.0)],
                    **a == "+");

        token_test!("2 + 1",
                    [NumLiteral(2.0), Operator(OpTok(ref a)), NumLiteral(1.0)],
                    **a == "+");

        token_test!("2.3*49.2",
                   [NumLiteral(2.3), Operator(OpTok(ref a)), NumLiteral(49.2)],
                   **a == "*");

        assert!(tokenize("2.4.5").is_err());

        token_test!("fn my_func(a) { a ? 3[1] }",
                    [Keyword(Kw::Fn), Identifier(ref a), LParen, Identifier(ref b), RParen, LCurlyBrace, Identifier(ref c),
                     Operator(OpTok(ref d)), NumLiteral(3.0), LSquareBracket, NumLiteral(1.0), RSquareBracket, RCurlyBrace],
                     **a == "my_func" && **b == "a" && **c == "a" && **d == "?");
    }

    #[test]
    fn string_test() {
        token_test!("null + \"a string\"",
                    [Keyword(Kw::Null), Operator(OpTok(ref a)), StrLiteral(ref b)],
                    **a == "+" && **b == "a string");

        token_test!("\"{?'q@?\"",
                    [StrLiteral(ref a)],
                    **a == "{?'q@?");
    }

    #[test]
    fn operator_test() {
        token_test!("a *> b",
                   [Identifier(ref a), Operator(OpTok(ref b)), Identifier(ref c)],
                   **a == "a" && **b == "*>" && **c == "b");


    }
}
