use std::io;
use std::io::Write;
use std::io::BufRead;
use std::char;
use std::slice::Iter;

use tokenizer::Token;
use tokenizer::Token::*;

mod tokenizer;


fn main() {
    println!("Unnamed language 0.01");
    repl();
}


#[derive(Debug)]
enum AST {
    Name(String),
    LangString(String),
    Number(f64),
    BinOp(Box<AST>, Box<AST>, Box<AST>),
    Binding(String, Box<AST>)
}

enum ParseResult {
    Ok(AST),
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

