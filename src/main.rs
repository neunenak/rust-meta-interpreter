use std::io;
use std::io::Write;
use std::io::BufRead;
use std::char;
use std::slice::Iter;

use Token::*;


fn main() {
    println!("Unnamed language 0.01");
    repl();
}

#[derive(Debug, Clone)]
enum Token {
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

#[derive(Debug)]
enum ASTNode {
    Name(String),
    Number(f64),
    BinOp(Box<ASTNode>, Box<ASTNode>, Box<ASTNode>),
    Binding(String, Box<ASTNode>)
}

enum ParseResult {
    Ok(ASTNode),
    Err(String)
}

fn repl() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut buf = String::with_capacity(20);
    loop {
        print!(">> ");
        stdout.flush().ok();
        let line = stdin.lock().read_line(&mut buf);
        match line {
            Ok(_) => {
                if buf.is_empty() {
                    break;
                }
                let tokens = tokenize(&buf);
                buf.clear();
                println!("Tokens: {:?}", tokens);

                match parse(tokens) {
                    ParseResult::Ok(ast) => println!("AST: {:?}", ast),
                    ParseResult::Err(err) => println!("Error: {}", err)
                }

                /*
                let eval = evaluate(&ast);
                println!("{}", eval);
                */
            },
            Err(err) => {
                println!("Error: {}", err);
            }
        }
    }
}


fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut iterator = input.chars().peekable();

    fn ends_identifier(c: char) -> bool {
        match c {
            c if char::is_whitespace(c) => true,
            ',' => true,
            ';' => true,
            '(' => true,
            ')' => true,
            _ => false
        }
    }

    while let Some(c) = iterator.next() {
        if char::is_whitespace(c) {
            continue;
        } else if c == '"' {

            let mut buffer = String::with_capacity(20);
            while let Some(x) = iterator.next() {
                if x == '"' {
                    break;
                }
                buffer.push(x);
            }
            tokens.push(Token::StrLiteral(buffer));

        } else if c == '#' {
            while let Some(x) = iterator.next() {
                if x == '\n' {
                    break;
                }
            }
        } else if c == ';' || c == '\n' {
            tokens.push(Token::Separator);
        } else if c == '(' {
            tokens.push(Token::LParen);
        } else if c == ')' {
            tokens.push(Token::RParen);
        } else if c == ',' {
            tokens.push(Token::Comma);
        } else {
            let mut buffer = String::with_capacity(20);
            buffer.push(c);

            while let Some(x) = iterator.peek().cloned() {
                if ends_identifier(x) {
                    break;
                }
                buffer.push(iterator.next().unwrap());
            }

            match buffer.parse::<f64>() {
                Ok(f) => tokens.push(Token::NumLiteral(f)),
                _ => tokens.push(Token::Identifier(buffer))
            }
        }
    }
    tokens.push(Token::EOF);
    tokens
}

fn parse(input: Vec<Token>) -> ParseResult {

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
                        let ast = ASTNode::Binding(name.clone(), Box::new(ASTNode::Name(value.clone())));
                        return ParseResult::Ok(ast);
                    }

                    if let Some(&StrLiteral(ref value)) = next {
                        let ast = ASTNode::Binding(name.clone(), Box::new(ASTNode::Name(value.clone())));
                        return ParseResult::Ok(ast);
                    }

                    if let Some(&NumLiteral(n)) = next {
                        let ast = ASTNode::Binding(name.clone(), Box::new(ASTNode::Number(n)));
                        return ParseResult::Ok(ast);
                    }
                }
            }
        }
    }

    return ParseResult::Err("Bad parse".to_string());
}

