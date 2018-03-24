extern crate schala_repl;

extern crate maaru_lang;
extern crate rukka_lang;
extern crate robo_lang;
extern crate schala_lang;
use schala_repl::{PLIGenerator, repl_main};

extern { }

fn main() {
  let generators: Vec<PLIGenerator> = vec![
    Box::new(|| { Box::new(schala_lang::Schala::new())}),
    Box::new(|| { Box::new(maaru_lang::Maaru::new())}),
    Box::new(|| { Box::new(robo_lang::Robo::new())}),
    Box::new(|| { Box::new(rukka_lang::Rukka::new())}),
  ];
  repl_main(generators);
}

