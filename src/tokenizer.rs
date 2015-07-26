#[derive(Debug, Clone)]
pub enum Token {
    EOF,
    Separator,
    LParen,
    RParen,
    Comma,
    Period,
    NumLiteral(f64),
    StrLiteral(String),
    Identifier(String),
    Keyword(Kw)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Kw {
    If,
    Then,
    Else,
    While,
    End,
    Let,
    Fn,
    Null
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
            '.' => true,
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
            if let Some(&Token::Separator) = tokens.last() {
            } else {
                tokens.push(Token::Separator);
            }
        } else if c == '(' {
            tokens.push(Token::LParen);
        } else if c == ')' {
            tokens.push(Token::RParen);
        } else if c == ',' {
            tokens.push(Token::Comma);
        } else if c == '.' {
            tokens.push(Token::Period);
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
                _ => tokens.push(handle_identifier(buffer))
            }
        }
    }
    tokens.push(Token::EOF);
    tokens
}

fn handle_identifier(identifier: String) -> Token {

    let keyword = match &identifier[..] {
        "let" => Kw::Let,
        "if"  => Kw::If,
        "then" => Kw::Then,
        "else" => Kw::Else,
        "while" => Kw::While,
        "end" => Kw::End,
        "fn" => Kw::Fn,
        "null" => Kw::Null,
        _ => return Token::Identifier(identifier)
    };

    return Token::Keyword(keyword);
}
