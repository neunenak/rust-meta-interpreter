use std::rc::Rc;

use parsing::{AST, Expression, Declaration};
use builtin::{BinOp, PrefixOp};

#[derive(Debug)]
pub struct ReducedAST(pub Vec<Stmt>);

#[derive(Debug, Clone)]
pub enum Stmt {
  Binding {
    name: Rc<String>,
    constant: bool,
    expr: Expr,
  },
  Expr(Expr),
}

#[derive(Debug, Clone)]
pub enum Expr {
  Lit(Lit),
  Func(Func),
  Val(Rc<String>),
  Call {
    f: Func,
    args: Vec<Expr>,
  },
  UnimplementedSigilValue
}

#[derive(Debug, Clone)]
pub enum Lit {
  Nat(u64),
  Int(i64),
  Float(f64),
  Bool(bool),
  StringLit(Rc<String>),
}

#[derive(Debug, Clone)]
pub enum Func {
  BuiltIn(Rc<String>),
  UserDefined {
    params: Vec<Rc<String>>,
    body: Vec<Stmt>,
  }
}

impl AST {
  pub fn reduce(&self) -> ReducedAST {
    use parsing::Statement::*;
    let mut output = vec![];
    for statement in self.0.iter() {
      match statement {
        &ExpressionStatement(ref expr) => output.push(Stmt::Expr(expr.reduce())),
        &Declaration(ref decl) => output.push(decl.reduce()),
      }
    }
    ReducedAST(output)
  }
}

impl Expression {
  fn reduce(&self) -> Expr {
    use parsing::ExpressionType::*;
    let ref input = self.0;
    match input {
      &IntLiteral(ref n) => Expr::Lit(Lit::Nat(*n)), //TODO I should rename IntLiteral if I want the Nat/Int distinction, which I do
      &FloatLiteral(ref f) => Expr::Lit(Lit::Float(*f)),
      &StringLiteral(ref s) => Expr::Lit(Lit::StringLit(s.clone())),
      &BoolLiteral(ref b) => Expr::Lit(Lit::Bool(*b)),
      &BinExp(ref binop, ref lhs, ref rhs) => binop.reduce(lhs, rhs),
      &PrefixExp(ref op, ref arg) => op.reduce(arg),
      &Value(ref name) => Expr::Val(name.clone()),
      e => Expr::UnimplementedSigilValue,
    }
  }
}

impl Declaration {
  fn reduce(&self) -> Stmt {
    use self::Declaration::*;
    match self {
      &Binding { ref name, ref constant, ref expr } => Stmt::Binding { name: name.clone(), constant: *constant, expr: expr.reduce() },
      _ => Stmt::Expr(Expr::UnimplementedSigilValue)
    }
  }
}

impl BinOp {
  fn reduce(&self, lhs: &Box<Expression>, rhs: &Box<Expression>) -> Expr {
    let f = Func::BuiltIn(self.sigil().clone());
    Expr::Call { f, args: vec![lhs.reduce(), rhs.reduce()]}
  }
}

impl PrefixOp {
  fn reduce(&self, arg: &Box<Expression>) -> Expr {
    let f = Func::BuiltIn(self.sigil().clone());
    Expr::Call { f, args: vec![arg.reduce()]}
  }
}
