use std::rc::Rc;

use ast::{AST, Statement, Expression, Declaration, Discriminator, IfExpressionBody, Pattern, PatternLiteral};
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
    type_name: Rc<String>,
    name: Rc<String>,
    tag: usize,
    arity: usize,
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
  CaseMatch {
    cond: Box<Expr>,
    alternatives: Vec<Alternative>
  },
  UnimplementedSigilValue
}

#[derive(Debug, Clone)]
pub struct Alternative {
  pub tag: Option<usize>,
  pub bound_vars: Vec<Option<Rc<String>>>, //remember that order matters here
  pub item: Vec<Stmt>,
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
      Value(name) => match symbol_table.lookup_by_name(name) {
        Some(Symbol { spec: SymbolSpec::DataConstructor { index, type_args, type_name}, .. }) => Expr::Constructor {
          type_name: type_name.clone(),
          name: name.clone(),
          tag: index.clone(),
          arity: type_args.len(),
        },
        _ => Expr::Val(name.clone()),
      },
      Call { f, arguments } => Expr::Call {
        f: Box::new(f.reduce(symbol_table)),
        args: arguments.iter().map(|arg| arg.reduce(symbol_table)).collect(),
      },
      TupleLiteral(exprs) => Expr::Tuple(exprs.iter().map(|e| e.reduce(symbol_table)).collect()),
      IfExpression { discriminator, body } => reduce_if_expression(discriminator, body, symbol_table),
      _ => Expr::UnimplementedSigilValue,
    }
  }
}

fn reduce_if_expression(discriminator: &Discriminator, body: &IfExpressionBody, symbol_table: &SymbolTable) -> Expr {
  let cond = Box::new(match *discriminator {
    Discriminator::Simple(ref expr) => expr.reduce(symbol_table),
    _ => panic!(),
  });
  match *body {
    IfExpressionBody::SimpleConditional(ref then_clause, ref else_clause) => {
      let then_clause = then_clause.iter().map(|expr| expr.reduce(symbol_table)).collect();
      let else_clause = match else_clause {
        None => vec![],
        Some(stmts) => stmts.iter().map(|expr| expr.reduce(symbol_table)).collect(),
      };
      Expr::Conditional { cond, then_clause, else_clause }
    },
    IfExpressionBody::SimplePatternMatch(ref pat, ref then_clause, ref else_clause) => {
      let then_clause = then_clause.iter().map(|expr| expr.reduce(symbol_table)).collect();
      let else_clause = match else_clause {
        None => vec![],
        Some(stmts) => stmts.iter().map(|expr| expr.reduce(symbol_table)).collect(),
      };

      let first_alt: Alternative = match pat {
        Pattern::TupleStruct(name, subpatterns) => {
          let symbol = symbol_table.values.get(name).expect(&format!("Symbol {} not found", name));
          let tag = match symbol.spec {
            SymbolSpec::DataConstructor { index, .. } => index.clone(),
            _ => panic!("Bad symbol"),
          };
          let bound_vars = subpatterns.iter().map(|p| match p {
            Pattern::Literal(PatternLiteral::VarPattern(var)) => Some(var.clone()),
            Pattern::Ignored => None,
            _ => None,
          }).collect();
          Alternative {
            tag: Some(tag),
            bound_vars,
            item: then_clause,
          }
        },
        _ => panic!()
      };

      let alternatives = vec![
        first_alt,
        Alternative {
          tag: None,
          bound_vars: vec![],
          item: else_clause,
        },
      ];

      Expr::CaseMatch {
        cond,
        alternatives,
      }
    },
    IfExpressionBody::GuardList(ref guard_arms) => {
      let alternatives = guard_arms.iter().map(|arm| {
        Alternative {
          tag: Some(0),
          bound_vars: vec![],
          item: arm.body.iter().map(|expr| expr.reduce(symbol_table)).collect(),
        }
      });
      Expr::UnimplementedSigilValue
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
      TypeDecl { .. } => Stmt::Noop,
      TypeAlias(_, _) => Stmt::Noop,
      Interface { .. } => Stmt::Noop,
      Impl { .. } => Stmt::Expr(Expr::UnimplementedSigilValue),
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
