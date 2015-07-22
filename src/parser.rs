use std::slice::Iter;
use std::iter::Peekable;

use tokenizer::{Token, Kw};
use tokenizer::Token::*;

#[derive(Debug)]
pub enum AST {
    Name(String),
    LangString(String),
    Number(f64),
    BinOp(Box<AST>, Box<AST>, Box<AST>),
    Binding(String, Box<AST>)
}

pub enum ParseResult {
    Ok(AST),
    Err(String)
}

type Tokens<'a> = Peekable<Iter<'a,Token>>;

/* expect calls .next() and thus advances the token list */
macro_rules! expect {
    ($tok:expr, $tokens:expr) => ( if !expect_token($tok, $tokens) {
        return ParseResult::Err(format!("Expected {:?}", $tok));
    })
}

fn expect_token(tok: Token, tokens: &mut Tokens) -> bool {
    if let Some(n) = tokens.next() {
        let next = (*n).clone();
        return match (tok, next) {
            (EOF, EOF) => true,
            (Separator, Separator) => true,
            (LParen, LParen) => true,
            (RParen, RParen) => true,
            (Comma, Comma) => true,
            (NumLiteral(_), NumLiteral(_)) => true,
            (StrLiteral(_), StrLiteral(_)) => true,
            (Identifier(ref i1), Identifier(ref i2)) => i1 == i2,
            (Keyword(k1), Keyword(k2)) => k1 == k2,
            _ => false
        }
    }

    false
}

pub fn parse(input: Vec<Token>) -> ParseResult {

    let mut tokens: Tokens = input.iter().peekable();

    if let ParseResult::Ok(ast) = let_expression(&mut tokens) {
        expect!(EOF, &mut tokens);
        return ParseResult::Ok(ast);
    }

    return ParseResult::Err("Bad parse".to_string());
}


fn let_expression(input: &mut Tokens) -> ParseResult {
    expect!(Keyword(Kw::Let), input);
    if let Some(&Identifier(ref name)) = input.next() {
        if let Some(&Identifier(ref s)) = input.next() {
            if s == "=" {
                let next = input.next();
                if let Some(&Identifier(ref value)) = next {
                    let ast = AST::Binding(name.clone(), Box::new(AST::Name(value.clone())));
                    return ParseResult::Ok(ast);
                }

                if let Some(&StrLiteral(ref value)) = next {
                    let ast = AST::Binding(name.clone(), Box::new(AST::LangString(value.clone())));
                    return ParseResult::Ok(ast);
                }

                if let Some(&NumLiteral(n)) = next {
                    let ast = AST::Binding(name.clone(), Box::new(AST::Number(n)));
                    return ParseResult::Ok(ast);
                }
            }
        }
    }

    return ParseResult::Err("Bad parse".to_string());
}
