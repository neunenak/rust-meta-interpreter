use std::rc::Rc;

use ast::{AST, Statement, Expression, Declaration, Discriminator, IfExpressionBody};
use symbol_table::{Symbol, SymbolSpec, SymbolTable};
use builtin::{BinOp, PrefixOp};

#[derive(Debug)]
pub struct ReducedAST(pub Vec<Stmt>);

#[derive(Debug, Clone)]
pub enum Stmt {
  PreBinding {
    name: Rc<String>,
    func: Func,
  },
  Binding {
    name: Rc<String>,
    constant: bool,
    expr: Expr,
  },
  Expr(Expr),
  Noop,
}

#[derive(Debug, Clone)]
pub enum Expr {
  Unit,
  Lit(Lit),
  Tuple(Vec<Expr>),
  Func(Func),
  Val(Rc<String>),
  Constructor {
    name: Rc<String>,
  },
  Call {
    f: Box<Expr>,
    args: Vec<Expr>,
  },
  Assign {
    val: Box<Expr>,
    expr: Box<Expr>,
  },
  Conditional {
    cond: Box<Expr>,
    then_clause: Vec<Stmt>,
    else_clause: Vec<Stmt>,
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
  Custom(Rc<String>, Vec<Expr>),
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
  pub fn reduce(&self, symbol_table: &SymbolTable) -> ReducedAST {
    let mut output = vec![];
    for statement in self.0.iter() {
      output.push(statement.reduce(symbol_table));
    }
    ReducedAST(output)
  }
}

impl Statement {
  fn reduce(&self, symbol_table: &SymbolTable) -> Stmt { 
    use ast::Statement::*;
    match self {
      ExpressionStatement(expr) => Stmt::Expr(expr.reduce(symbol_table)),
      Declaration(decl) => decl.reduce(symbol_table),
    }
  }
}

impl Expression {
  fn reduce(&self, symbol_table: &SymbolTable) -> Expr {
    use ast::ExpressionType::*;
    let ref input = self.0;
    match input {
      NatLiteral(n) => Expr::Lit(Lit::Nat(*n)),
      FloatLiteral(f) => Expr::Lit(Lit::Float(*f)),
      StringLiteral(s) => Expr::Lit(Lit::StringLit(s.clone())),
      BoolLiteral(b) => Expr::Lit(Lit::Bool(*b)),
      BinExp(binop, lhs, rhs) => binop.reduce(symbol_table, lhs, rhs),
      PrefixExp(op, arg) => op.reduce(symbol_table, arg),
      Value(name) => {
        match symbol_table.values.get(name) {
          Some(Symbol { spec: SymbolSpec::DataConstructor { type_args, .. }, .. }) => {
            Expr::Constructor { name: name.clone() }
          },
          _ => Expr::Val(name.clone()),
        }
      },
      Call { f, arguments } => Expr::Call {
        f: Box::new(f.reduce(symbol_table)),
        args: arguments.iter().map(|arg| arg.reduce(symbol_table)).collect(),
      },
      TupleLiteral(exprs) => Expr::Tuple(exprs.iter().map(|e| e.reduce(symbol_table)).collect()),
      IfExpression { discriminator, body } => {
        let cond = Box::new(match **discriminator {
          Discriminator::Simple(ref expr) => expr.reduce(symbol_table),
          _ => panic!(),
        });
        match **body {
          IfExpressionBody::SimpleConditional(ref then_clause, ref else_clause) => {
            let then_clause = then_clause.iter().map(|expr| expr.reduce(symbol_table)).collect();
            let else_clause = match else_clause {
              None => vec![],
              Some(stmts) => stmts.iter().map(|expr| expr.reduce(symbol_table)).collect(),
            };
            Expr::Conditional { cond, then_clause, else_clause }
          },
          _ => panic!(),
        }
      },
      _ => Expr::UnimplementedSigilValue,
    }
  }
}

impl Declaration {
  fn reduce(&self, symbol_table: &SymbolTable) -> Stmt {
    use self::Declaration::*;
    use ::ast::Signature;
    match self {
      Binding {name, constant, expr } => Stmt::Binding { name: name.clone(), constant: *constant, expr: expr.reduce(symbol_table) },
      FuncDecl(Signature { name, params, .. }, statements) => Stmt::PreBinding {
        name: name.clone(),
        func: Func::UserDefined {
          name: Some(name.clone()),
          params: params.iter().map(|param| param.0.clone()).collect(),
          body: statements.iter().map(|stmt| stmt.reduce(symbol_table)).collect(),
        }
      },
      TypeDecl(_,_) => Stmt::Noop,
      _ => Stmt::Expr(Expr::UnimplementedSigilValue)
    }
  }
}

impl BinOp {
  fn reduce(&self, symbol_table: &SymbolTable, lhs: &Box<Expression>, rhs: &Box<Expression>) -> Expr {
    if **self.sigil() == "=" {
      Expr::Assign {
        val: Box::new(lhs.reduce(symbol_table)),
        expr: Box::new(rhs.reduce(symbol_table)),
      }
    } else {
      let f = Box::new(Expr::Func(Func::BuiltIn(self.sigil().clone())));
      Expr::Call { f, args: vec![lhs.reduce(symbol_table), rhs.reduce(symbol_table)]}
    }
  }
}

impl PrefixOp {
  fn reduce(&self, symbol_table: &SymbolTable, arg: &Box<Expression>) -> Expr {
    let f = Box::new(Expr::Func(Func::BuiltIn(self.sigil().clone())));
    Expr::Call { f, args: vec![arg.reduce(symbol_table)]}
  }
}
