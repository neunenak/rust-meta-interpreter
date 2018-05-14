#![feature(slice_patterns, box_patterns, box_syntax)]
#![feature(proc_macro)]
extern crate itertools;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate maplit;
#[macro_use]
extern crate schala_repl;
#[macro_use]
extern crate schala_codegen;

use std::cell::RefCell;
use std::rc::Rc;

use itertools::Itertools;
use schala_repl::{ProgrammingLanguageInterface, EvalOptions, TraceArtifact, UnfinishedComputation, FinishedComputation};

macro_rules! bx {
  ($e:expr) => { Box::new($e) }
}

mod util;
mod builtin;
mod tokenizing;
mod parsing;
mod typechecking;
mod ast_reducing;
mod eval;

#[derive(ProgrammingLanguageInterface)]
#[LanguageName = "Schala"]
#[SourceFileExtension = "schala"]
#[PipelineSteps(tokenizing, parsing, symbol_table, typechecking, ast_reducing, eval)]
pub struct Schala {
  state: eval::State<'static>,
  type_context: Rc<RefCell<typechecking::TypeContext>>,
}

impl Schala {
  pub fn new() -> Schala {
    let type_context = Rc::new(RefCell::new(typechecking::TypeContext::new()));
    Schala {
      type_context: type_context.clone(),
      state: eval::State::new(type_context),
    }
  }
}

fn tokenizing(_handle: &mut Schala, input: &str, comp: Option<&mut UnfinishedComputation>) -> Result<Vec<tokenizing::Token>, String> {
  let tokens = tokenizing::tokenize(input);
  comp.map(|comp| {
    let token_string = tokens.iter().map(|t| format!("{:?}<L:{},C:{}>", t.token_type, t.offset.0, t.offset.1)).join(", ");
    comp.add_artifact(TraceArtifact::new("tokens", token_string));
  });

  let errors: Vec<String> = tokens.iter().filter_map(|t| t.get_error()).collect();
  if errors.len() == 0 {
    Ok(tokens)
  } else {
    Err(format!("{:?}", errors))
  }
}

fn parsing(_handle: &mut Schala, input: Vec<tokenizing::Token>, comp: Option<&mut UnfinishedComputation>) -> Result<parsing::AST, String> {

  let (ast, trace) = parsing::parse(input);
  comp.map(|comp| {
    //TODO need to control which of these debug stages get added
    comp.add_artifact(TraceArtifact::new_parse_trace(trace));
    comp.add_artifact(TraceArtifact::new("ast", format!("{:#?}", ast)));
  });
  ast.map_err(|err| err.msg)
}

fn symbol_table(handle: &mut Schala, input: parsing::AST, comp: Option<&mut UnfinishedComputation>) -> Result<parsing::AST, String> {
  let add = handle.type_context.borrow_mut().add_top_level_types(&input);
  match add {
    Ok(()) => {
      let artifact = TraceArtifact::new("symbol_table", handle.type_context.borrow().debug_symbol_table());
      comp.map(|comp| comp.add_artifact(artifact));
      Ok(input)
    },
    Err(msg) => Err(msg)
  }
}

fn typechecking(handle: &mut Schala, input: parsing::AST, comp: Option<&mut UnfinishedComputation>) -> Result<parsing::AST, String> {
  match handle.type_context.borrow_mut().type_check_ast(&input) {
    Ok(ty) => {
      comp.map(|comp| comp.add_artifact(TraceArtifact::new("type_check", format!("{:?}", ty))));
      Ok(input)
    },
    Err(msg) => {
      comp.map(|comp| comp.add_artifact(TraceArtifact::new("type_check", format!("Type error: {:?}", msg))));
      Ok(input)
    }
  }
}

fn ast_reducing(_handle: &mut Schala, input: parsing::AST, comp: Option<&mut UnfinishedComputation>) -> Result<ast_reducing::ReducedAST, String> {
  let output = input.reduce();
  comp.map(|comp| comp.add_artifact(TraceArtifact::new("ast_reducing", format!("{:?}", output))));
  Ok(output)
}

fn eval(handle: &mut Schala, input: ast_reducing::ReducedAST, comp: Option<&mut UnfinishedComputation>) -> Result<String, String> {
  comp.map(|comp| comp.add_artifact(TraceArtifact::new("value_state", handle.state.debug_print())));
  let evaluation_outputs = handle.state.evaluate(input, true);
  let text_output: Result<Vec<String>, String> = evaluation_outputs
    .into_iter()
    .collect();

  let eval_output: Result<String, String> = text_output
    .map(|v| { v.into_iter().intersperse(format!("\n")).collect() });
  eval_output
}

