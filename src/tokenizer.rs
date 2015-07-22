#[derive(Debug, Clone)]
pub enum Token {
    EOF,
    Separator,
    LParen,
    RParen,
    Comma,
    NumLiteral(f64),
    StrLiteral(String),
    Identifier(String)
    /* Keyword(Keyword) */ //implement in future
}

pub fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut iterator = input.chars().peekable();

    fn ends_identifier(c: char) -> bool {
        match c {
            c if char::is_whitespace(c) => true,
            ',' => true,
            ';' => true,
            '(' => true,
            ')' => true,
            _ => false
        }
    }

    while let Some(c) = iterator.next() {
        if char::is_whitespace(c) {
            continue;
        } else if c == '"' {

            let mut buffer = String::with_capacity(20);
            while let Some(x) = iterator.next() {
                if x == '"' {
                    break;
                }
                buffer.push(x);
            }
            tokens.push(Token::StrLiteral(buffer));

        } else if c == '#' {
            while let Some(x) = iterator.next() {
                if x == '\n' {
                    break;
                }
            }
        } else if c == ';' || c == '\n' {
            tokens.push(Token::Separator);
        } else if c == '(' {
            tokens.push(Token::LParen);
        } else if c == ')' {
            tokens.push(Token::RParen);
        } else if c == ',' {
            tokens.push(Token::Comma);
        } else {
            let mut buffer = String::with_capacity(20);
            buffer.push(c);

            while let Some(x) = iterator.peek().cloned() {
                if ends_identifier(x) {
                    break;
                }
                buffer.push(iterator.next().unwrap());
            }

            match buffer.parse::<f64>() {
                Ok(f) => tokens.push(Token::NumLiteral(f)),
                _ => tokens.push(Token::Identifier(buffer))
            }
        }
    }
    tokens.push(Token::EOF);
    tokens
}
