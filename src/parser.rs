use std::slice::Iter;
use std::iter::Peekable;

use tokenizer::{Token, Kw};
use tokenizer::Token::*;

#[derive(Debug, Clone)]
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
    Function(String, Box<AST>, Box<AST>),
    ArgList(Vec<String>),
    DoNothing
}

pub type ParseResult = Result<AST, String>;

type Tokens<'a> = Peekable<Iter<'a,Token>>;

/* expect calls .next() and thus advances the token list */
macro_rules! expect {
    ($tok:expr, $tokens:expr) => ( if !expect_token($tok, $tokens) {
        let tokens_left: Vec<&Token> = $tokens.collect();
        let err_string = format!("Expected {:?}\ntokens: {:?}", $tok, tokens_left);
        return Err(err_string);
    })
}

macro_rules! expect_parse {
    ($parse_fn:ident, $tokens:ident) => (
        match $parse_fn($tokens) {
            err@Err(_) => return err,
            Ok(ast) => ast
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
        return Ok(AST::Statements(vec!()));
    }

    match statements(&mut tokens) {
        ok@Ok(_) => {
            expect!(EOF, &mut tokens);
            ok
        },
        err@Err(_) => err
    }
}

fn statements(tokens: &mut Tokens) -> ParseResult {

    let mut statements = Vec::new();

    let initial_statement = expect_parse!(statement, tokens);
    statements.push(initial_statement);

    loop {
        let lookahead = tokens.peek().map(|i| i.clone());
        match lookahead {
            Some(&Separator) => {
                tokens.next();
                match statement(tokens) {
                    Ok(ast_next) => {
                        statements.push(ast_next);
                    },
                    err@Err(_) => return err
                };
            },
            _ => break
        }
    }

    return Ok(AST::Statements(statements));
}

fn statement(tokens: &mut Tokens) -> ParseResult {
    match tokens.peek().map(|i| i.clone()) {
        Some(&Keyword(Kw::Let)) => let_expression(tokens),
        Some(&Keyword(Kw::Fn)) => function_block(tokens),
        _ => expression(tokens)
    }
}

fn function_block(tokens: &mut Tokens) -> ParseResult {
    expect!(Keyword(Kw::Fn), tokens);

    let name: String = match tokens.next() {
        Some(&Identifier(ref s)) => s.clone(),
        _ => return Err("bad parse in function_block()".to_string())
    };

    expect!(LParen, tokens);

    let arguments = try!(argument_list(tokens));

    expect!(RParen, tokens);

    let body = try!(statements(tokens));

    expect!(Keyword(Kw::End), tokens);

    Ok(AST::Function(
            name,
            Box::new(arguments),
            Box::new(body)
            ))
}

fn argument_list(tokens: &mut Tokens) -> ParseResult {

    let mut args: Vec<String> = Vec::new();

    loop {
        let lookahead = tokens.peek().map(|i| i.clone());
        match lookahead {
            Some(&Identifier(ref s)) => {
                args.push(s.clone());
                tokens.next();
                if let Some(&Comma) = tokens.peek().map(|i| i.clone()) {
                    tokens.next();
                } else {
                    break;
                }
            },
            _ => break
        }
    }

    Ok(AST::ArgList(args))
}

fn let_expression(tokens: &mut Tokens) -> ParseResult {
    expect!(Keyword(Kw::Let), tokens);
    if let Some(&Identifier(ref name)) = tokens.next() {
        if let Some(&Keyword(Kw::Assign)) = tokens.next() {
            if let Ok(expr) = expression(tokens) {
                return Ok(
                    AST::Binding(name.clone(),
                    Box::new(expr)));
            }
        }
    }

    return Err("Bad parse in let_expression()".to_string());
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
        _ => binop_expression(0, tokens)
    }
}

fn if_expression(tokens: &mut Tokens) -> ParseResult {

    expect!(Keyword(Kw::If), tokens);
    let if_clause = expect_parse!(expression, tokens);

    expect!(Keyword(Kw::Then), tokens);
    let then_clause = expect_parse!(expression, tokens);

    let else_clause = match tokens.peek().map(|i| i.clone()) {
        Some(&Keyword(Kw::Else)) => {
            tokens.next();
            match expression(tokens) {
                err@Err(_) => return err,
                Ok(ast) => Some(ast)
            }
        },
        _ => None
    };

    expect!(Keyword(Kw::End), tokens);

    Ok(AST::IfStatement(
            Box::new(if_clause),
            Box::new(then_clause),
            else_clause.map(|ast| Box::new(ast))
            ))
}

fn while_expression(tokens: &mut Tokens) -> ParseResult {
    expect!(Keyword(Kw::While), tokens);
    let while_expression = expect_parse!(expression, tokens);

    expect!(Separator, tokens);

    let statements = expect_parse!(statements, tokens);

    expect!(Keyword(Kw::End), tokens);

    Ok(AST::WhileStatement(
            Box::new(while_expression),
            Box::new(statements),
            ))
}

fn binop_expression(precedence: i32, tokens: &mut Tokens) -> ParseResult {

    //TODO left needs to match on an identifiers vs. a prefix operator and return *that* AST
    let mut left: AST = expect_parse!(simple_expression, tokens);

    loop {
        let lookahead: Option<&Token> = tokens.peek().map(|i| i.clone());
        let next_precedence = lookahead.and_then(|t| get_binop_precedence(t));

        match next_precedence {
            Some(next) if precedence < next => {
                left = match binop_rhs(next, left, tokens) {
                    err@Err(_) => return err,
                    Ok(ast) => ast
                };
            },

            _ => return Ok(left),
        }
    }
}

fn binop_rhs(precedence: i32, lhs: AST, tokens: &mut Tokens) -> ParseResult {

    let op: AST = match simple_expression(tokens) {
        err@Err(_) => return err,
        Ok(ast) => ast
    };

    let rhs: AST = match binop_expression(precedence, tokens) {
        err@Err(_) => return err,
        Ok(ast) => ast
    };

    Ok(AST::BinOp(
            Box::new(op),
            Box::new(lhs),
            Box::new(rhs)
            ))
}

fn get_binop_precedence(token: &Token) -> Option<i32> {
    let identifier_str: &String = match token {
        &Identifier(ref s) => s,
        _ => return None
    };

    let output =
    ::BINOP_TABLE.with(|hm| {
        let prec_table = hm.borrow();
        let val: Option<i32> = prec_table.get(&identifier_str[..]).map(|i| *i);
        val
    });

    output
}

fn simple_expression(tokens: &mut Tokens) -> ParseResult {
    let next = tokens.next();

    match next {
        Some(&Keyword(Kw::Null)) =>
            Ok(AST::Name("null".to_string())),

        Some(&Identifier(ref value)) =>
            Ok(AST::Name(value.clone())),

        Some(&StrLiteral(ref value)) =>
            Ok(AST::LangString(value.clone())),

        Some(&NumLiteral(n)) =>
            Ok(AST::Number(n)),

        Some(&LParen) => {
            let within_paren = expression(tokens);
            expect!(RParen, tokens);
            within_paren
        },
        _ => Err("Bad parse in simple_expression()".to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tokenizer::tokenize;

    #[test]
    fn parse_tests() {
        ::init_binop_table();

        match parse(tokenize("a + b * c")) {
            Ok(ast) =>
                assert_eq!(format!("{:?}", ast), "Statements([BinOp(Name(\"+\"), Name(\"a\"), BinOp(Name(\"*\"), Name(\"b\"), Name(\"c\")))])"),
            Err(err) => panic!("err: {:?}", err)
        }

        match parse(tokenize("(a + b) * c")) {
            Ok(ast) =>
                assert_eq!(format!("{:?}", ast), "Statements([BinOp(Name(\"*\"), BinOp(Name(\"+\"), Name(\"a\"), Name(\"b\")), Name(\"c\"))])"),
            Err(err) => panic!("err: {:?}", err)
        }
    }
}
