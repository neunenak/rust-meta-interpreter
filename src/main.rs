use std::io;
use std::io::Write;
use std::io::BufRead;
use std::char;

fn main() {
    println!("Unnamed language 0.01");
    repl();
}

#[derive(Debug)]
enum Token {
    EOF,
    NumLiteral(i32),
    StrLiteral(String),
    Identifier(String)
    /* Keyword(Keyword) */ //implement in future

}

#[derive(Debug)]
enum ASTNode {
    GenericNode
}


fn repl() {
    let mut stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut buf = String::with_capacity(20);
    loop {
        print!(">> ");
        stdout.flush().ok();
        let line = stdin.lock().read_line(&mut buf);
        match line {
            Ok(n) => {
                let tokens = tokenize(&buf);
                buf.clear();
                println!("Tokens: {:?}", tokens);

                let ast = parse(tokens);
                println!("AST: {:?}", ast);

                let eval = evaluate(&ast);
                println!("{}", eval);
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
        } else {
            let mut buffer = String::with_capacity(20);
            buffer.push(c);
            while let Some(x) = iterator.next() {
                if char::is_whitespace(x) {
                    break;
                }
                buffer.push(x);
            }
            tokens.push(Token::Identifier(buffer));
        }
    }

    tokens.push(Token::EOF);
    tokens
}

fn parse(_input: Vec<Token>) -> ASTNode {
    ASTNode::GenericNode
}

fn evaluate(input: &ASTNode) -> String {

    return match eval_ast(input) {
        ASTNode::GenericNode => "Not implemented".to_string()
    };

    fn eval_ast(_input: &ASTNode) -> ASTNode {
        return ASTNode::GenericNode;
    }
}
