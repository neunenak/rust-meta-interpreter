use schala_lang::parsing::{AST, Statement, Declaration, Expression, ExpressionType, Operation};

pub struct ReplState {
}

type EvalResult<T> = Result<T, String>;

enum FullyEvaluatedExpr {
  UnsignedInt(u64),
  SignedInt(i64),
  Float(f64),
  Str(String),
  Bool(bool),
}

impl ReplState {
  pub fn new() -> ReplState {
    ReplState { }
  }

  pub fn evaluate(&mut self, ast: AST) -> Vec<String> {
    let mut acc = vec![];
    for statement in ast.0 {
      match self.eval_statement(statement) {
        Ok(output) => {
          if let Some(s) = output {
            acc.push(s);
          }
        },
        Err(error) => {
          acc.push(format!("Error: {}", error));
          return acc;
        },
      }
    }
    acc
  }
}

impl ReplState {
  fn eval_statement(&mut self, statement: Statement) -> EvalResult<Option<String>> {
    use self::FullyEvaluatedExpr::*;
    match statement {
      Statement::ExpressionStatement(expr) => {
        self.eval_expr(expr).map( |eval| {
          match eval {
            UnsignedInt(n) => Some(format!("{}", n)),
            SignedInt(n) => Some(format!("{}", n)),
            Float(f) => Some(format!("{}", f)),
            Str(s) => Some(format!("\"{}\"", s)),
            Bool(b) => Some(format!("{}", b)),
          }
        })
      },
      Statement::Declaration(decl) => {
        self.eval_decl(decl).map(|_| None)
      }
    }
  }

  fn eval_decl(&mut self, _decl: Declaration) -> EvalResult<()> {
    Err("Not implmemented".to_string())
  }

  fn eval_expr(&mut self, expr: Expression) -> EvalResult<FullyEvaluatedExpr> {
    use self::ExpressionType::*;
    use self::FullyEvaluatedExpr::*;

    let expr_type = expr.0;
    match expr_type {
      IntLiteral(n) => Ok(UnsignedInt(n)),
      FloatLiteral(f) => Ok(Float(f)),
      StringLiteral(s) => Ok(Str(s.to_string())),
      BoolLiteral(b) => Ok(Bool(b)),
      PrefixExp(op, expr) => self.eval_prefix_exp(op, expr),
      BinExp(op, lhs, rhs) => self.eval_binexp(op, lhs, rhs),
      _ => Err(format!("Unimplemented")),
    }
  }

  fn eval_binexp(&mut self, op: Operation, lhs: Box<Expression>, rhs: Box<Expression>) -> EvalResult<FullyEvaluatedExpr> {
    use self::FullyEvaluatedExpr::*;
    let evaled_lhs = self.eval_expr(*lhs)?;
    let evaled_rhs = self.eval_expr(*rhs)?;
    let opstr: &str = &op.0;
    Ok(match (opstr, evaled_lhs, evaled_rhs) {
      ("+", UnsignedInt(l), UnsignedInt(r)) => UnsignedInt(l + r),
      ("+", Str(s1), Str(s2)) => Str(format!("{}{}", s1, s2)),
      ("-", UnsignedInt(l), UnsignedInt(r)) => UnsignedInt(l - r),
      ("*", UnsignedInt(l), UnsignedInt(r)) => UnsignedInt(l * r),
      ("/", UnsignedInt(l), UnsignedInt(r)) => UnsignedInt(l / r),
      ("%", UnsignedInt(l), UnsignedInt(r)) => UnsignedInt(l % r),
      _ => return Err(format!("Runtime error: not yet implemented")),
    })
  }

  fn eval_prefix_exp(&mut self, op: Operation, expr: Box<Expression>) -> EvalResult<FullyEvaluatedExpr> {
    use self::FullyEvaluatedExpr::*;
    let evaled_expr = self.eval_expr(*expr)?;
    let opstr: &str = &op.0;

    Ok(match (opstr, evaled_expr) {
      ("!", Bool(true)) => Bool(false),
      ("!", Bool(false)) => Bool(true),
      ("-", UnsignedInt(n)) => SignedInt(-1*(n as i64)),
      ("-", SignedInt(n)) => SignedInt(-1*(n as i64)),
      _ => return Err(format!("Runtime error: not yet implemented")),
    })
  }
}
