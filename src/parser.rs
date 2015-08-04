use std::slice::Iter;
use std::iter::Peekable;

use tokenizer::{Token, Kw};
use tokenizer::Token::*;

#[derive(Debug)]
pub enum AST {
    Null,
    Name(String),
    LangString(String),
    Number(f64),
    BinOp(Box<AST>, Box<AST>, Box<AST>),
    Binding(String, Box<AST>),
    Statements(Vec<AST>),
    IfStatement(Box<AST>, Box<AST>, Option<Box<AST>>),
    WhileStatement(Box<AST>, Box<AST>),
    DoNothing
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

fn statements(tokens: &mut Tokens) -> ParseResult {

    let mut statements = Vec::new();

    let initial_statement = match statement(tokens) {
        err@ParseResult::Err(_) => return err,
        ParseResult::Ok(ast) => ast
    };

    statements.push(initial_statement);

    loop {
        let lookahead = tokens.peek().map(|i| i.clone());
        match lookahead {
            Some(&Separator) => {
                tokens.next();
                match statement(tokens) {
                    ParseResult::Ok(ast_next) => {
                        statements.push(ast_next);
                    },
                    err@ParseResult::Err(_) => return err
                };
            },
            _ => break
        }
    }

    return ParseResult::Ok(AST::Statements(statements));
}

fn statement(tokens: &mut Tokens) -> ParseResult {
    match tokens.peek().map(|i| i.clone()) {
        Some(&Keyword(Kw::Let)) => let_expression(tokens),
        _ => expression(tokens)
    }
}

fn let_expression(tokens: &mut Tokens) -> ParseResult {
    expect!(Keyword(Kw::Let), tokens);
    if let Some(&Identifier(ref name)) = tokens.next() {
        if let Some(&Identifier(ref s)) = tokens.next() {
            if s == "=" {
                if let ParseResult::Ok(expr) = expression(tokens) {
                    return ParseResult::Ok(
                        AST::Binding(name.clone(),
                                Box::new(expr)));
                }
            }
        }
    }

    return ParseResult::Err("Bad parse in let_expression()".to_string());
}

fn expression(tokens: &mut Tokens) -> ParseResult {
    let lookahead = tokens.peek().map(|i| i.clone());
    match lookahead {
        Some(&Keyword(Kw::If)) => {
            if_expression(tokens)
        },
        Some(&Keyword(Kw::While)) => {
            while_expression(tokens)
        },
        Some(&LParen) => {
            tokens.next();
            let expr = expression(tokens);
            expect!(RParen, tokens);
            expr
        },
        _ => binop_expression(0, tokens)
    }
}

fn if_expression(tokens: &mut Tokens) -> ParseResult {

    expect!(Keyword(Kw::If), tokens);
    let if_clause = match expression(tokens) {
        err@ParseResult::Err(_) => return err,
        ParseResult::Ok(ast) => ast
    };

    expect!(Keyword(Kw::Then), tokens);

    let then_clause = match expression(tokens) {
        err@ParseResult::Err(_) => return err,
        ParseResult::Ok(ast) => ast
    };

    let else_clause = match tokens.peek().map(|i| i.clone()) {
        Some(&Keyword(Kw::Else)) => {
            tokens.next();
            match expression(tokens) {
                err@ParseResult::Err(_) => return err,
                ParseResult::Ok(ast) => Some(ast)
            }
        },
        _ => None
    };

    expect!(Keyword(Kw::End), tokens);

    ParseResult::Ok( AST::IfStatement(
            Box::new(if_clause),
            Box::new(then_clause),
            else_clause.map(|ast| Box::new(ast))
            ))
}

fn while_expression(tokens: &mut Tokens) -> ParseResult {
    expect!(Keyword(Kw::While), tokens);
    let while_expression = match expression(tokens) {
        err@ParseResult::Err(_) => return err,
        ParseResult::Ok(ast) => ast
    };

    expect!(Separator, tokens);
    let statements = match statements(tokens) {
        err@ParseResult::Err(_) => return err,
        ParseResult::Ok(ast) => ast
    };

    expect!(Keyword(Kw::End), tokens);

    ParseResult::Ok(AST::WhileStatement(
            Box::new(while_expression),
            Box::new(statements),
            ))
}

fn binop_expression(precedence: i32, tokens: &mut Tokens) -> ParseResult {

    //TODO left needs to match on an identifiers vs. a prefix operator and return *that* AST
    let left: AST = match simple_expression(tokens) {
        err@ParseResult::Err(_) => return err,
        ParseResult::Ok(ast) => ast
    };

    let lookahead: Option<&Token> = tokens.peek().map(|i| i.clone());
    let precedence = lookahead.and_then(|t| get_binop_precedence(t));

    if let None = precedence {
        return ParseResult::Ok(left);
    }

    binop_rhs(left, tokens)
}

fn binop_rhs(lhs: AST, tokens: &mut Tokens) -> ParseResult {

    let op: AST = match simple_expression(tokens) {
        err@ParseResult::Err(_) => return err,
        ParseResult::Ok(ast) => ast
    };

    let rhs: AST = match binop_expression(0, tokens) {
        err@ParseResult::Err(_) => return err,
        ParseResult::Ok(ast) => ast
    };

    ParseResult::Ok(AST::BinOp(
            Box::new(op),
            Box::new(lhs),
            Box::new(rhs)
            ))
}

fn get_binop_precedence(token: &Token) -> Option<i32> {
    let identifier_str = match token {
        &Identifier(ref s) => s,
        _ => return None
    };

    match &identifier_str[..] {
        "+" => Some(20),
        "-" => Some(20),
        "*" => Some(20),
        "/" => Some(20),
        _ => None
    }
}

fn simple_expression(tokens: &mut Tokens) -> ParseResult {
    let next = tokens.next();
    if let Some(&Identifier(ref value)) = next {
        return ParseResult::Ok(AST::Name(value.clone()));
    }

    if let Some(&StrLiteral(ref value)) = next {
        return ParseResult::Ok(AST::LangString(value.clone()));
    }

    if let Some(&NumLiteral(n)) = next {
        return ParseResult::Ok(AST::Number(n));
    }

    return ParseResult::Err("Bad parse in simple_expression()".to_string());
}
