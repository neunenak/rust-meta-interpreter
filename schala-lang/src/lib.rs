#![feature(slice_patterns, box_patterns, box_syntax)]
#![feature(proc_macro)]
extern crate itertools;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate maplit;

#[macro_use]
extern crate schala_repl;
extern crate schala_codegen;

use itertools::Itertools;
use schala_repl::{ProgrammingLanguageInterface, EvalOptions, TraceArtifact, UnfinishedComputation, FinishedComputation};

macro_rules! bx {
  ($e:expr) => { Box::new($e) }
}

mod builtin;

mod tokenizing;
mod parsing;
mod typechecking;
mod eval;

use self::typechecking::{TypeContext};

pub struct Schala {
  state: eval::State<'static>,
  type_context: TypeContext
}

impl Schala {
  pub fn new() -> Schala {
    Schala {
      state: eval::State::new(),
      type_context: TypeContext::new(),
    }
  }
}

fn tokenizing_stage(_handle: &mut Schala, input: &str, comp: Option<&mut UnfinishedComputation>) -> Result<Vec<tokenizing::Token>, ()> {
  let tokens = tokenizing::tokenize(input);
  comp.map(|comp| {
    println!("This should only be evaluated when debugging tokens and not other times!!!");
    let token_string = tokens.iter().map(|t| format!("{:?}<L:{},C:{}>", t.token_type, t.offset.0, t.offset.1)).join(", ");
    comp.add_artifact(TraceArtifact::new("tokens", token_string));
  });
  Ok(tokenizing::tokenize(input))
}

fn parsing_stage(_handle: &mut Schala, input: Vec<tokenizing::Token>, comp: Option<&mut UnfinishedComputation>) -> Result<parsing::AST, parsing::ParseError> {

  let (ast, trace) = parsing::parse(input);
  comp.map(|comp| {
    //TODO need to control which of these debug stages get added
    comp.add_artifact(TraceArtifact::new_parse_trace(trace));
    comp.add_artifact(TraceArtifact::new("ast", format!("{:#?}", ast)));
  });
  ast
}

fn symbol_table_stage(handle: &mut Schala, input: parsing::AST, comp: Option<&mut UnfinishedComputation>) -> Result<parsing::AST, String> {
  match handle.type_context.add_top_level_types(&input) {
    Ok(()) => Ok(input),
    Err(msg) => Err(msg)
  }
}

fn typechecking_stage(handle: &mut Schala, input: parsing::AST, comp: Option<&mut UnfinishedComputation>) -> Result<parsing::AST, String> {
  match handle.type_context.type_check_ast(&input) {
    Ok(ty) => {
      println!("FINAL TYPE: {:?}", ty);
      /*
         if options.debug.type_checking {
         evaluation.add_artifact(TraceArtifact::new("type_check", format!("{:?}", ty)));
         }
         */
      Ok(input)
    },
    Err(msg) => Err(msg)
  }
}

fn eval_stage(handle: &mut Schala, input: parsing::AST, comp: Option<&mut UnfinishedComputation>) -> Result<String, String> {
  let evaluation_outputs = handle.state.evaluate(input);
  let text_output: Result<Vec<String>, String> = evaluation_outputs
    .into_iter()
    .collect();

  let eval_output: Result<String, String> = text_output
    .map(|v| { v.into_iter().intersperse(format!("\n")).collect() });
  eval_output
}

impl ProgrammingLanguageInterface for Schala {
  fn get_language_name(&self) -> String {
    "Schala".to_string()
  }

  fn get_source_file_suffix(&self) -> String {
    format!("schala")
  }

  fn execute_pipeline(&mut self, input: &str, options: &EvalOptions) -> FinishedComputation {
    //let chain = pass_chain![tokenizing::tokenize, parsing::parse];
    let mut chain = pass_chain![self;
      tokenizing_stage,
      parsing_stage,
      symbol_table_stage,
      typechecking_stage,
      eval_stage
    ];
    chain(input)
  }

  /*
  fn execute(&mut self, input: &str, options: &EvalOptions) -> FinishedComputation {
    schala_codegen::print_a_thing!();

    let mut evaluation = UnfinishedComputation::default();

    //tokenzing
    let tokens = tokenizing::tokenize(input);
    if options.debug.tokens {
      let token_string = tokens.iter().map(|t| format!("{:?}<L:{},C:{}>", t.token_type, t.offset.0, t.offset.1)).join(", ");
      evaluation.add_artifact(TraceArtifact::new("tokens", token_string));
    }

    {
      let token_errors: Vec<&String> = tokens.iter().filter_map(|t| t.get_error()).collect();
      if token_errors.len() != 0 {
        return evaluation.output(Err(format!("Tokenization error: {:?}\n", token_errors)));
      }
    }

    // parsing
    let ast = match parsing::parse(tokens) {
      (Ok(ast), trace) => {
        if options.debug.parse_tree {
          evaluation.add_artifact(TraceArtifact::new_parse_trace(trace));
        }
        if options.debug.ast {
          evaluation.add_artifact(TraceArtifact::new("ast", format!("{:#?}", ast)));
        }
        ast
      },
      (Err(err), trace) => {
        if options.debug.parse_tree {
          evaluation.add_artifact(TraceArtifact::new_parse_trace(trace));
        }
        return evaluation.output(Err(format!("Parse error: {:?}\n", err.msg)));
      }
    };

    //symbol table
    match self.type_context.add_top_level_types(&ast) {
      Ok(()) => (),
      Err(msg) => {
        if options.debug.type_checking {
          evaluation.add_artifact(TraceArtifact::new("type_check", msg));
        }
      }
    };

    //typechecking
    match self.type_context.type_check_ast(&ast) {
      Ok(ty) => {
        if options.debug.type_checking {
          evaluation.add_artifact(TraceArtifact::new("type_check", format!("{:?}", ty)));
        }
      },
      Err(msg) => evaluation.add_artifact(TraceArtifact::new("type_check", msg)),
    };

    let text = self.type_context.debug_symbol_table();
    if options.debug.symbol_table {
      evaluation.add_artifact(TraceArtifact::new("symbol_table", text));
    }

    let evaluation_outputs = self.state.evaluate(ast);
    let text_output: Result<Vec<String>, String> = evaluation_outputs
      .into_iter()
      .collect();

    let eval_output = text_output
      .map(|v| { v.into_iter().intersperse(format!("\n")).collect() });
    evaluation.output(eval_output)
  }
    */
}
