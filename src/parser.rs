use std::slice::Iter;

use tokenizer::{Token};
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

pub fn parse(input: Vec<Token>) -> ParseResult {

    let mut tokens = input.iter();

    if let ParseResult::Ok(ast) = let_expression(&mut tokens) {
        if expect(EOF, &mut tokens) {
            return ParseResult::Ok(ast);
        }
    }

    return ParseResult::Err("Bad parse".to_string());
}

fn expect(tok: Token, tokens: &mut Iter<Token>) -> bool {
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
            _ => false
        }
    }

    return false;
}

fn let_expression<'a>(input: &mut Iter<Token>) -> ParseResult {
    if expect(Identifier("let".to_string()), input) {
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
    }

    return ParseResult::Err("Bad parse".to_string());
}
