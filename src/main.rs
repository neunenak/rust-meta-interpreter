#![feature(link_args)]
#![feature(advanced_slice_patterns, slice_patterns, box_patterns, box_syntax)]
#![feature(plugin)]
#![plugin(rocket_codegen)]
extern crate getopts;
extern crate linefeed;
extern crate itertools;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate maplit;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;
extern crate rocket;
extern crate rocket_contrib;
extern crate includedir;
extern crate phf;

use std::path::Path;
use std::fs::File;
use std::io::{Read, Write};
use std::process::exit;
use std::default::Default;

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
    Box::new(|| { let x: Box<ProgrammingLanguageInterface> = Box::new(schala_lang::Schala::new()); x }),
    Box::new(|| { let x: Box<ProgrammingLanguageInterface> = Box::new(maaru_lang::Maaru::new()); x }),
    Box::new(|| { let x: Box<ProgrammingLanguageInterface> = Box::new(robo_lang::Robo::new()); x }),
  ];
  schala_main(generators);
}

