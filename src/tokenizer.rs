#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    EOF,
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
    Null,
    Assign
}

fn is_digit(c: &char) -> bool {
    c.is_digit(10)
}

fn ends_identifier(c: &char) -> bool {
    let c = *c;
    char::is_whitespace(c) ||
    is_digit(&c) ||
    c == ';' ||
    c == '(' ||
    c == ')' ||
    c == ',' ||
    c == '.' ||
    c == ':'
}

pub fn tokenize(input: &str) -> Option<Vec<Token>> {
    use self::Token::*;
    let mut tokens = Vec::new();
    let mut iter = input.chars().peekable();

    while let Some(c) = iter.next() {
        if char::is_whitespace(c) && c != '\n' {
            continue;
        } else if c == '#' {
            while let Some(c) = iter.next() {
                if c == '\n' { break; }
            }
        }

        let cur_tok =
        if c == '\n' {
            Newline
        } else if c == ';' {
            Semicolon
        } else if c == '(' {
            LParen
        } else if c == ')' {
            RParen
        } else if c == ':' {
            Colon
        } else if c == '"' {
            let mut buffer = String::with_capacity(20);
            loop {
                //TODO handle string escapes, interpolation
                match iter.next() {
                    Some(x) if x == '"' => break,
                    Some(x) => buffer.push(x),
                    None => return None,
                }
            }
            StrLiteral(buffer)
        } else if c == '.' && !iter.peek().map_or(false, |x| is_digit(x)) {
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
                Err(_) => return None
            }
        } else {
            let mut buffer = String::with_capacity(20);
            buffer.push(c);
            loop {
                if iter.peek().map_or(false, |x| ends_identifier(x)) {
                    break;
                } else {
                    buffer.push(iter.next().unwrap());
                }
            }

            match &buffer[..] {
                "if" => Keyword(Kw::If),
                "then" => Keyword(Kw::Then),
                b => Identifier(b.to_string())
            }
        };

        tokens.push(cur_tok);
    }

    Some(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokeniziation_tests() {
        let t1 = "let a = 3\n";
        assert_eq!(format!("{:?}", tokenize(t1)),
            "[Keyword(Let), Identifier(\"a\"), Keyword(Assign), NumLiteral(3), Newline, EOF]");

        // this is intentional
        let t2 = "a + b*c\n";
        assert_eq!(format!("{:?}", tokenize(t2)),
            "[Identifier(\"a\"), Identifier(\"+\"), Identifier(\"b*c\"), Newline, EOF]");

    }
}
