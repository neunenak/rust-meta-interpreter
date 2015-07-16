use std::io;
use std::io::Write;
use std::io::BufRead;

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
                let result = tokenize(&buf);
                println!("Tokens: {:?}", result);
            },
            Err(err) => {
                println!("Error: {}", err);
            }
        }
    }
}


fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut buffer = String::with_capacity(20);
    let mut iterator = input.chars();
    loop {

        break;
    }

    tokens.push(Token::EOF);
    tokens
}

fn parse(_input: Vec<Token>) -> ASTNode {
    ASTNode::GenericNode
}
