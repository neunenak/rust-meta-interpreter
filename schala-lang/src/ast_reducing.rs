use std::rc::Rc;

use parsing::{AST, Expression, Declaration};

#[derive(Debug)]
pub struct ReducedAST(pub Vec<Stmt>);

#[derive(Debug)]
pub enum Stmt {
  Binding {
    name: Rc<String>,
    expr: Expr,
  },
  Expr(Expr),
}

#[derive(Debug)]
pub enum Expr {
  Lit(Lit),
  Func(Func),
  Call {
    f: Func,
    args: Vec<Expr>,
  },
}

#[derive(Debug)]
pub enum Lit {
  Int(u64),
  Bool(bool),
  StringLit(Rc<String>),
}

#[derive(Debug)]
pub struct Func {
  params: Vec<Rc<String>>,
  body: Vec<Stmt>,
}

pub fn perform_ast_reduction(ast: &AST) -> Result<ReducedAST, String> {
  use parsing::Statement::*;
  let mut output = vec![];
  for statement in ast.0.iter() {
    match statement {
      &ExpressionStatement(ref expr) => output.push(reduce_expr(expr)?),
      &Declaration(ref decl) => output.push(reduce_decl(decl)?),
    }
  }
  Ok(ReducedAST(output))
}

fn reduce_expr(expr: &Expression) -> Result<Stmt, String> {
  use parsing::ExpressionType::*;
  let ref input = expr.0;
  let output_expr = match input {
    &IntLiteral(ref n) => Expr::Lit(Lit::Int(*n)),
    &StringLiteral(ref s) => Expr::Lit(Lit::StringLit(s.clone())),
    &BoolLiteral(ref b) => Expr::Lit(Lit::Bool(*b)),
    e => return Err(format!("{:?} not implemented in reduction", e))
  };
  Ok(Stmt::Expr(output_expr))
}
fn reduce_decl(expr: &Declaration) -> Result<Stmt, String> {
  Ok(Stmt::Expr(Expr::Lit(Lit::Int(0))))
}
