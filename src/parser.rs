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
    Statements(Vec<AST>),
    IfStatement(Box<AST>, Box<AST>, Option<Box<AST>>)
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
    match input.peek().map(|i| i.clone()) {
        Some(&Keyword(Kw::Let)) => let_expression(input),
        _ => expression(input)
    }
}

fn let_expression(input: &mut Tokens) -> ParseResult {
    expect!(Keyword(Kw::Let), input);
    if let Some(&Identifier(ref name)) = input.next() {
        if let Some(&Identifier(ref s)) = input.next() {
            if s == "=" {
                if let ParseResult::Ok(rhs) = rhs(input) {
                    return ParseResult::Ok(
                        AST::Binding(name.clone(),
                                Box::new(rhs)));
                }
            }
        }
    }

    return ParseResult::Err("Bad parse in let_expression()".to_string());
}

fn expression(input: &mut Tokens) -> ParseResult {
    let lookahead = input.peek().map(|i| i.clone());
    match lookahead {
        Some(&Keyword(Kw::If)) => {
            if_expression(input)
        },
        _ => rhs(input)
    }
}

fn if_expression(input: &mut Tokens) -> ParseResult {

    expect!(Keyword(Kw::If), input);
    let if_clause = match expression(input) {
        err@ParseResult::Err(_) => return err,
        ParseResult::Ok(ast) => ast
    };

    expect!(Keyword(Kw::Then), input);

    let then_clause = match expression(input) {
        err@ParseResult::Err(_) => return err,
        ParseResult::Ok(ast) => ast
    };

    let else_clause = match input.peek().map(|i| i.clone()) {
        Some(&Keyword(Kw::Else)) => {
            input.next();
            match expression(input) {
                err@ParseResult::Err(_) => return err,
                ParseResult::Ok(ast) => Some(ast)
            }
        },
        _ => None
    };

    expect!(Keyword(Kw::End), input);

    ParseResult::Ok( AST::IfStatement(
            Box::new(if_clause),
            Box::new(then_clause),
            else_clause.map(|ast| Box::new(ast))
            ))
}

fn rhs(input: &mut Tokens) -> ParseResult {
    let next = input.next();
    if let Some(&Identifier(ref value)) = next {
        return ParseResult::Ok(AST::Name(value.clone()));
    }

    if let Some(&StrLiteral(ref value)) = next {
        return ParseResult::Ok(AST::LangString(value.clone()));
    }

    if let Some(&NumLiteral(n)) = next {
        return ParseResult::Ok(AST::Number(n));
    }

    return ParseResult::Err("Bad parse in rhs()".to_string());
}
