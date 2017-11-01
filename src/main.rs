#![feature(link_args)]
#![feature(advanced_slice_patterns, slice_patterns, box_patterns, box_syntax)]
#![feature(plugin)]
extern crate itertools;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate maplit;

mod schala_lang;
mod maaru_lang;
mod robo_lang;

extern crate schala_lib;
use schala_lib::{PLIGenerator, schala_main};
use schala_lib::language::ProgrammingLanguageInterface;

#[link_args="-ltinfo"]
extern { }

fn main() {
  let generators: Vec<PLIGenerator> = vec![
    Box::new(|| { Box::new(schala_lang::Schala::new())}),
    Box::new(|| { Box::new(maaru_lang::Maaru::new())}),
    Box::new(|| { Box::new(robo_lang::Robo::new())}),
  ];
  schala_main(generators);
}

