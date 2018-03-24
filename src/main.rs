#![feature(advanced_slice_patterns, slice_patterns, box_patterns, box_syntax)]
#![feature(plugin)]
extern crate itertools;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate maplit;

extern crate schala_repl;

extern crate maaru_lang;
extern crate rukka_lang;
extern crate robo_lang;
extern crate schala_lang;
use schala_repl::{PLIGenerator, schala_main};

extern { }

fn main() {
  let generators: Vec<PLIGenerator> = vec![
    Box::new(|| { Box::new(schala_lang::Schala::new())}),
    Box::new(|| { Box::new(maaru_lang::Maaru::new())}),
    Box::new(|| { Box::new(robo_lang::Robo::new())}),
    Box::new(|| { Box::new(rukka_lang::Rukka::new())}),
  ];
  schala_main(generators);
}

