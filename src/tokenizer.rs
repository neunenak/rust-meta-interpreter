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

fn ends_identifier(c: &char) -> bool {
    let c = *c;
    char::is_whitespace(c) || is_digit(&c) || c == ';' || c == '(' || c == ')' || c == ',' ||
    c == '.' || c == ',' || c == ':'
}

pub fn tokenize(input: &str) -> TokenizeResult {
    use self::Token::*;
    let mut tokens = Vec::new();
    let mut iter = input.chars().peekable();

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
            '"' => {
                let mut buffer = String::with_capacity(20);
                loop {
                    // TODO handle string escapes, interpolation
                    match iter.next() {
                        Some(x) if x == '"' => break,
                        Some(x) => buffer.push(x),
                        None => return Err(TokenizeError::new("Unclosed quote")),
                    }
                }
                StrLiteral(buffer)
            }
            c if !char::is_alphanumeric(c) => {
                let mut buffer = String::with_capacity(20);
                buffer.push(c);
                loop {
                    if iter.peek().map_or(false,
                                          |x| !char::is_alphanumeric(*x) && !char::is_whitespace(*x)) {
                        let n = iter.next().unwrap();
                        buffer.push(n);
                    } else {
                        break;
                    }
                }
                Operator(Op(buffer))
            }
            c => {
                if c == '.' && !iter.peek().map_or(false, |x| is_digit(x)) {
                    Period
                } else if is_digit(&c) || c == '.' {
                    let mut buffer = String::with_capacity(20);
                    buffer.push(c);
                    loop {
                        if iter.peek().map_or(false, |x| is_digit(x) || *x == '.') {
                            let n = iter.next().unwrap();
                            buffer.push(n);
                        } else {
                            break;
                        }
                    }
                    match buffer.parse::<f64>() {
                        Ok(f) => NumLiteral(f),
                        Err(_) => return Err(TokenizeError::new("Failed to pase digit")),
                    }
                } else {
                    let mut buffer = String::with_capacity(20);
                    buffer.push(c);
                    loop {
                        if iter.peek().map_or(true, |x| ends_identifier(x)) {
                            break;
                        } else {
                            buffer.push(iter.next().unwrap());
                        }
                    }

                    match &buffer[..] {
                        "if" => Keyword(Kw::If),
                        "then" => Keyword(Kw::Then),
                        "else" => Keyword(Kw::Else),
                        "while" => Keyword(Kw::While),
                        "end" => Keyword(Kw::End),
                        "let" => Keyword(Kw::Let),
                        "fn" => Keyword(Kw::Fn),
                        "null" => Keyword(Kw::Null),
                        b => Identifier(b.to_string()),
                    }
                }
            }
        };

        tokens.push(cur_tok);
    }

    Ok(tokens)
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
                   "[Keyword(Let), Identifier(\"a\"), Operator(Op { repr: \"=\" }), \
                    NumLiteral(3), Newline]");

        tokentest!("2+1",
                   "[NumLiteral(2), Operator(Op { repr: \"+\" }), NumLiteral(1)]");

        tokentest!("2 + 1",
                   "[NumLiteral(2), Operator(Op { repr: \"+\" }), NumLiteral(1)]");

        tokentest!("2.3*49.2",
                   "[NumLiteral(2.3), Operator(Op { repr: \"*\" }), NumLiteral(49.2)]");

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
