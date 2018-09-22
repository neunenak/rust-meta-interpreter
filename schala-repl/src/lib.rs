#![feature(link_args)]
#![feature(slice_patterns, box_patterns, box_syntax)]
#![feature(plugin)]
#![plugin(rocket_codegen)]
extern crate getopts;
extern crate linefeed;
extern crate itertools;
extern crate colored;

#[macro_use]
extern crate serde_derive;
extern crate serde_json;
extern crate rocket;
extern crate rocket_contrib;
extern crate includedir;
extern crate phf;

use std::path::Path;
use std::fs::File;
use std::io::Read;
use std::process::exit;
use std::default::Default;

mod repl;
mod language;
mod webapp;
pub mod llvm_wrap;

const VERSION_STRING: &'static str = "0.1.0";

include!(concat!(env!("OUT_DIR"), "/static.rs"));

pub use language::{LLVMCodeString, ProgrammingLanguageInterface, EvalOptions,
  ExecutionMethod, TraceArtifact, FinishedComputation, UnfinishedComputation, PassDebugOptionsDescriptor, PassDescriptor};

pub type PLIGenerator = Box<Fn() -> Box<ProgrammingLanguageInterface> + Send + Sync>;

pub fn repl_main(generators: Vec<PLIGenerator>) {
  let languages: Vec<Box<ProgrammingLanguageInterface>> = generators.iter().map(|x| x()).collect();

  let option_matches = program_options().parse(std::env::args()).unwrap_or_else(|e| {
    println!("{:?}", e);
    exit(1);
  });

  if option_matches.opt_present("list-languages") {
    for lang in languages {
      println!("{}", lang.get_language_name());
    }
    exit(1);
  }

  if option_matches.opt_present("help") {
    println!("{}", program_options().usage("Schala metainterpreter"));
    exit(0);
  }

  if option_matches.opt_present("webapp") {
    webapp::web_main(generators);
    exit(0);
  }

  let mut options = EvalOptions::default();
  let debug_passes = if let Some(opts) = option_matches.opt_str("debug") {
    let output: Vec<String> = opts.split_terminator(",").map(|s| s.to_string()).collect();
    output
  } else {
    vec![]
  };

  let language_names: Vec<String> = languages.iter().map(|lang| {lang.get_language_name()}).collect();
  let initial_index: usize =
    option_matches.opt_str("lang")
    .and_then(|lang| { language_names.iter().position(|x| { x.to_lowercase() == lang.to_lowercase() }) })
    .unwrap_or(0);

  options.execution_method = match option_matches.opt_str("eval-style") {
    Some(ref s) if s == "compile" => ExecutionMethod::Compile,
    _ => ExecutionMethod::Interpret,
  };

  match option_matches.free[..] {
    [] | [_] => {
      let mut repl = repl::Repl::new(languages, initial_index);
      repl.run();
    }
    [_, ref filename, _..] => {

      run_noninteractive(filename, languages, options, debug_passes);
    }
  };
}

fn run_noninteractive(filename: &str, languages: Vec<Box<ProgrammingLanguageInterface>>, mut options: EvalOptions, debug_passes: Vec<String>) {
  let path = Path::new(filename);
  let ext = path.extension().and_then(|e| e.to_str()).unwrap_or_else(|| {
    println!("Source file lacks extension");
    exit(1);
  });
  let mut language = Box::new(languages.into_iter().find(|lang| lang.get_source_file_suffix() == ext)
    .unwrap_or_else(|| {
      println!("Extension .{} not recognized", ext);
      exit(1);
    }));

  let mut source_file = File::open(path).unwrap();
  let mut buffer = String::new();

  source_file.read_to_string(&mut buffer).unwrap();

  for pass in debug_passes.into_iter() {
    if let Some(_) = language.get_passes().iter().find(|desc| desc.name == pass) {
      options.debug_passes.insert(pass, PassDebugOptionsDescriptor { opts: vec![] });
    }
  }

  match options.execution_method {
    ExecutionMethod::Compile => {
      /*
      let llvm_bytecode = language.compile(&buffer);
      compilation_sequence(llvm_bytecode, filename);
      */
      panic!("Not ready to go yet");
    },
    ExecutionMethod::Interpret => {
      let output = language.execute_pipeline(&buffer, &options);
      output.to_noninteractive().map(|text| println!("{}", text));
    }
  }
}

/*
pub fn compilation_sequence(llvm_code: LLVMCodeString, sourcefile: &str) {
    use std::process::Command;

    let ll_filename = "out.ll";
    let obj_filename = "out.o";
    let q: Vec<&str> = sourcefile.split('.').collect();
    let bin_filename = match &q[..] {
        &[name, "maaru"] => name,
        _ => panic!("Bad filename {}", sourcefile),
    };

    let LLVMCodeString(llvm_str) = llvm_code;

    println!("Compilation process finished for {}", ll_filename);
    File::create(ll_filename)
        .and_then(|mut f| f.write_all(llvm_str.as_bytes()))
        .expect("Error writing file");

    let llc_output = Command::new("llc")
        .args(&["-filetype=obj", ll_filename, "-o", obj_filename])
        .output()
        .expect("Failed to run llc");


    if !llc_output.status.success() {
        println!("{}", String::from_utf8_lossy(&llc_output.stderr));
    }

    let gcc_output = Command::new("gcc")
        .args(&["-o", bin_filename, &obj_filename])
        .output()
        .expect("failed to run gcc");

    if !gcc_output.status.success() {
        println!("{}", String::from_utf8_lossy(&gcc_output.stdout));
        println!("{}", String::from_utf8_lossy(&gcc_output.stderr));
    }

    for filename in [obj_filename].iter() {
        Command::new("rm")
            .arg(filename)
            .output()
            .expect(&format!("failed to run rm {}", filename));
    }
}
*/

fn program_options() -> getopts::Options {
  let mut options = getopts::Options::new();
  options.optopt("s",
                 "eval-style",
                 "Specify whether to compile (if supported) or interpret the language. If not specified, the default is language-specific",
                 "[compile|interpret]"
                 );
  options.optflag("",
                  "list-languages",
                  "Show a list of all supported languages");
  options.optopt("l",
                 "lang",
                 "Start up REPL in a language",
                 "LANGUAGE");
  options.optflag("h",
                  "help",
                  "Show help text");
  options.optflag("w",
                  "webapp",
                  "Start up web interpreter");
  options.optopt("d",
                  "debug",
                  "Debug a stage (l = tokenizer, a = AST, r = parse trace, s = symbol table)",
                  "[l|a|r|s]");
  options
}
