use std::rc::Rc;

use parsing::{AST, Statement, Expression, Declaration};
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
  Unit,
  Lit(Lit),
  Func(Func),
  Val(Rc<String>),
  Call {
    f: Box<Expr>,
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
    name: Option<Rc<String>>,
    params: Vec<Rc<String>>,
    body: Vec<Stmt>,
  }
}

impl AST {
  pub fn reduce(&self) -> ReducedAST {
    let mut output = vec![];
    for statement in self.0.iter() {
      output.push(statement.reduce());
    }
    ReducedAST(output)
  }
}

impl Statement {
  fn reduce(&self) -> Stmt { 
    use parsing::Statement::*;
    match self {
      ExpressionStatement(expr) => Stmt::Expr(expr.reduce()),
      Declaration(decl) => decl.reduce(),
    }
  }
}

impl Expression {
  fn reduce(&self) -> Expr {
    use parsing::ExpressionType::*;
    let ref input = self.0;
    match input {
      NatLiteral(n) => Expr::Lit(Lit::Nat(*n)),
      FloatLiteral(f) => Expr::Lit(Lit::Float(*f)),
      StringLiteral(s) => Expr::Lit(Lit::StringLit(s.clone())),
      BoolLiteral(b) => Expr::Lit(Lit::Bool(*b)),
      BinExp(binop, lhs, rhs) => binop.reduce(lhs, rhs),
      PrefixExp(op, arg) => op.reduce(arg),
      Value(name) => Expr::Val(name.clone()),
      Call { f, arguments } => Expr::Call {
        f: Box::new(f.reduce()),
        args: arguments.iter().map(|arg| arg.reduce()).collect(),
      },
      e => Expr::UnimplementedSigilValue,
    }
  }
}

impl Declaration {
  fn reduce(&self) -> Stmt {
    use self::Declaration::*;
    match self {
      Binding {name, constant, expr } => Stmt::Binding { name: name.clone(), constant: *constant, expr: expr.reduce() },
      FuncDecl(::parsing::Signature { name, params, type_anno }, statements) => Stmt::Binding {
        name: name.clone(),
        constant: true,
        expr: Expr::Func(Func::UserDefined {
          name: None,
          params: params.iter().map(|param| param.0.clone()).collect(),
          body: statements.iter().map(|stmt| stmt.reduce()).collect(),
        })
      },
      _ => Stmt::Expr(Expr::UnimplementedSigilValue)
    }
  }
}

impl BinOp {
  fn reduce(&self, lhs: &Box<Expression>, rhs: &Box<Expression>) -> Expr {
    let f = Box::new(Expr::Func(Func::BuiltIn(self.sigil().clone())));
    Expr::Call { f, args: vec![lhs.reduce(), rhs.reduce()]}
  }
}

impl PrefixOp {
  fn reduce(&self, arg: &Box<Expression>) -> Expr {
    let f = Box::new(Expr::Func(Func::BuiltIn(self.sigil().clone())));
    Expr::Call { f, args: vec![arg.reduce()]}
  }
}
