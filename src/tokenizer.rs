
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
