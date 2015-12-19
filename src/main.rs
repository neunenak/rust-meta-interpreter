extern crate simplerepl;

use std::path::Path;
use std::fs::File;
use std::io::Read;

use simplerepl::REPL;

use tokenizer::tokenize;
mod tokenizer;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    println!("Schala v 0.02");
    if let Some(filename) = args.get(1) {
        let mut source_file = File::open(&Path::new(filename)).unwrap();
        let mut buffer = String::new();
        source_file.read_to_string(&mut buffer).unwrap();
        panic!("Not implemented yet");
    } else {
        REPL::default(repl_handler).run();
    }
}

fn repl_handler(input: &str) -> String {
    format!("{:?}", tokenize(input))
}
