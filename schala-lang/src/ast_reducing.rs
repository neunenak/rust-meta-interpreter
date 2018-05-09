use std::rc::Rc;

use parsing::AST;

pub struct ReducedAST(pub Vec<Stmt>);

pub enum Stmt {
  Binding {
    name: Rc<String>,
    expr: Expr,
  },
  Expr(Expr),
}

pub enum Expr {
  Literal(Lit),
  Func(Func),
  Call {
    f: Func,
    args: Vec<Expr>,
  },
}

pub enum Lit {
  Int(u64),
  Bool(bool),
  StringLit(Rc<String>),
}

pub struct Func {
  params: Vec<Rc<String>>,
  body: Vec<Stmt>,
}


pub fn perform_ast_reduction(input: &AST) -> Result<ReducedAST, String> {
  Ok(ReducedAST(vec![]))
}
