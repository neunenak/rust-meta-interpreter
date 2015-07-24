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
    Binding(String, Box<AST>),
    Statements(Vec<AST>)
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

    if let Some(&&EOF) = tokens.peek() {
        return ParseResult::Ok(AST::Statements(vec!()));
    }

    match statements(&mut tokens) {
        ok@ParseResult::Ok(_) => {
            expect!(EOF, &mut tokens);
            ok
        },
        err@ParseResult::Err(_) => err
    }
}

fn statements(input: &mut Tokens) -> ParseResult {

    let mut statements = Vec::new();

    let initial_statement = statement(input);
    match initial_statement {
        ParseResult::Ok(ast) => {
            statements.push(ast);
            loop {
                let lookahead = input.peek().map(|i| i.clone());
                if let Some(&Separator) = lookahead {
                    input.next();
                    if let ParseResult::Ok(ast_next) = statement(input) {
                        statements.push(ast_next);
                    } else {
                        return ParseResult::Err("bad thing happened".to_string());
                    }
                } else {
                    break;
                }
            }
        },
        err@ParseResult::Err(_) => {
            return err;
        }
    }

    return ParseResult::Ok(AST::Statements(statements));
}

fn statement(input: &mut Tokens) -> ParseResult {
    let_expression(input)
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

    return ParseResult::Err("Bad parse in let_expression()".to_string());
}
