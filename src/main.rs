use std::io;
use std::io::Write;
use std::io::BufRead;
use std::process;
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::Path;
use std::fs::File;
use std::io::Read;

use tokenizer::tokenize;
use parser::{parse};
use evaluate::{evaluate, Environment};

mod tokenizer;
mod parser;
mod evaluate;

struct REPLOptions {
    show_tokenization: bool,
    show_ast: bool
}

impl REPLOptions {
    fn new() -> REPLOptions {
        REPLOptions {
            show_tokenization: false,
            show_ast: false
        }
    }
}

type BinopTable = HashMap<&'static str, i32>;

thread_local!(static BINOP_TABLE: RefCell<BinopTable> = RefCell::new(HashMap::new()));

fn main() {
    let args: Vec<String> = std::env::args().collect();
    println!("Unnamed language 0.01");
    init_binop_table();
    if let Some(filename) = args.get(1) {
        let mut source_file = File::open(&Path::new(filename)).unwrap();
        let mut buffer = String::new();
        source_file.read_to_string(&mut buffer).unwrap();

        match parse(tokenize(&buffer)) {
            Ok(ast) => {
                let (result, env) = evaluate(ast, Environment::new());
                println!("{}", result);
            }, 
            Err(err) => println!("{}", err)
        }
    } else {
        repl();
    }
}

fn init_binop_table() {
    BINOP_TABLE.with(|hm| {
        macro_rules! insert_precedence {
            ($op:expr, $prec:expr) => { hm.borrow_mut().insert($op, $prec) }
        }
        insert_precedence!("+", 20);
        insert_precedence!("-", 20);
        insert_precedence!("*", 40);
        insert_precedence!("/", 40);
        insert_precedence!("%", 40);
        insert_precedence!("**", 50);
        insert_precedence!("==", 10);
        insert_precedence!(">", 15);
        insert_precedence!("<", 15);
        insert_precedence!("<=>", 15);
    });
}

fn repl() {

    let mut options = REPLOptions::new();

    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut buf = String::with_capacity(20);
    let mut env = Environment::new();
    loop {
        buf.clear();
        print!(">> ");
        stdout.flush().ok();
        let line = stdin.lock().read_line(&mut buf);

        match line {
            Ok(_) => {
                if buf.is_empty() {
                    break;
                }

                if handle_interpreter_directive(&buf, &env, &mut options) {
                    continue;
                }

                let tokens = tokenize(&buf);
                if options.show_tokenization {
                    println!("Tokens: {:?}", tokens);
                }

                match parse(tokens) {
                    Ok(ast) => {
                        if options.show_ast {
                            println!("AST: {:?}", ast);
                        }

                        let (eval, new_env) = evaluate(ast, env);
                        if !eval.is_empty() {
                            println!("{}", eval);
                        }
                        env = new_env;
                    },
                    Err(err) => println!("Error: {}", err)
                }
            },
            Err(err) => {
                println!("Error: {}", err);
            }
        }
    }
}

fn handle_interpreter_directive(input: &str,
                                env: &Environment,
                                options: &mut REPLOptions) -> bool {

    match input.chars().nth(0) {
        Some('.') => (),
        _ => return false
    }

    let commands: Vec<&str> = input.split(|c: char| c.is_whitespace()).collect();
    match commands.get(0) {
        Some(s) if *s == ".show" => {
            match commands.get(1) {
                Some(s) if *s == "parse"  => {
                    options.show_ast = true;
                    println!("Showing parse result");
                },
                Some(s) if *s == "tokens" => {
                    options.show_tokenization = true;
                    println!("Showing tokenization");
                },
                _ => println!("Bad option for show"),
            }
        },

        Some(s) if *s == ".hide" => {
            match commands.get(1) {
                Some(s) if *s == "parse" => {
                    options.show_ast = false;
                    println!("Hiding parse result");
                },
                Some(s) if *s == "tokens" => {
                    options.show_tokenization = false;
                    println!("Hiding tokenization");
                },
                _ => println!("Bad option for hide"),
            }
        },

        Some(s) if *s == ".quit" => {
            println!("Siturei simasu");
            process::exit(0);
        },
        Some(s) if *s == ".env" => {
            env.display();
        },
        Some(s) if *s == ".prec" => {
            BINOP_TABLE.with(|hm| {

                println!("{0: <10} | {1: <10}", "operator", "precedence");
                let prec_table = hm.borrow();
                for (op, prec) in prec_table.iter() {
                    println!("{0: <10} | {1: <10}", op, prec);
                }
            });
        },
        Some(s) => {
            println!("Unknown directive: {}", s);
        },
        None => () //should never happen
    }

    return true;
}
