#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    EOF,
    Newline,
    Semicolon,
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
    Null,
    Assign
}

pub fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
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
