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

pub fn tokenize(input: &str) -> Vec<Token> {
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
                    None => return tokens,
                }
            }
            StrLiteral(buffer)
        } else {
            StrLiteral("DUMMY".to_string())
        };

        tokens.push(cur_tok);
    }

    tokens
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
