use std::rc::Rc;

use parsing::{AST, Expression, Declaration};
use builtin::{BinOp, PrefixOp};

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
  Float(f64),
  Bool(bool),
  StringLit(Rc<String>),
}

#[derive(Debug)]
pub struct Func {
  params: Vec<Rc<String>>,
  body: Vec<Stmt>,
}

impl AST {
  pub fn reduce(&self) -> Result<ReducedAST, String> {
    use parsing::Statement::*;
    let mut output = vec![];
    for statement in self.0.iter() {
      match statement {
        &ExpressionStatement(ref expr) => output.push(expr.reduce()?),
        &Declaration(ref decl) => output.push(decl.reduce()?),
      }
    }
    Ok(ReducedAST(output))
  }
}

impl Expression {
  fn reduce(&self) -> Result<Stmt, String> {
    use parsing::ExpressionType::*;
    let ref input = self.0;
    let output_expr = match input {
      &IntLiteral(ref n) => Expr::Lit(Lit::Int(*n)),
      &FloatLiteral(ref f) => Expr::Lit(Lit::Float(*f)),
      &StringLiteral(ref s) => Expr::Lit(Lit::StringLit(s.clone())),
      &BoolLiteral(ref b) => Expr::Lit(Lit::Bool(*b)),
      &BinExp(ref binop, ref lhs, ref rhs) => binop.reduce(lhs, rhs)?,
      &PrefixExp(ref op, ref arg) => op.reduce(arg)?,
      e => return Err(format!("{:?} not implemented in reduction", e))
    };
    Ok(Stmt::Expr(output_expr))
  }
}

impl Declaration {
  fn reduce(&self) -> Result<Stmt, String> {
    Ok(Stmt::Expr(Expr::Lit(Lit::Int(0))))
  }
}

impl BinOp {
  fn reduce(&self, lhs: &Box<Expression>, rhs: &Box<Expression>) -> Result<Expr, String> {
    Err(format!("NOTDONE"))
  }
}

impl PrefixOp {
  fn reduce(&self, arg: &Box<Expression>) -> Result<Expr, String> {
    Err(format!("NOTDONE"))
  }
}
