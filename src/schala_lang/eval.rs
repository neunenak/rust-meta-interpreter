use schala_lang::parsing::{AST, Statement, Declaration, Expression, ExpressionType, Operation, TypeAnno};

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


// from Niko's talk
/* fn type_check(expression, expected_ty) -> Ty {
    let ty = bare_type_check(expression, expected_type);
    if ty icompatible with expected_ty {
        try_coerce(expression, ty, expected_ty)
    } else {
      ty
    }
  }

  fn bare_type_check(exprssion, expected_type) -> Ty { ... }
 */
pub enum TypeCheck {
  OK,
  Error(String)
}
impl TypeCheck {
  fn new(msg: &str) -> TypeCheck {
    TypeCheck::Error(msg.to_string())
  }
}

impl ReplState {
  pub fn type_check(&mut self, ast: &AST) -> TypeCheck {
    use self::ExpressionType::*;
    for statement in ast.0.iter() {
      match statement {
        &Statement::Declaration(ref _decl) => {
          return TypeCheck::new("Declarations not supported");
        },
        &Statement::ExpressionStatement(ref expr) => {
          match (&expr.0, &expr.1) {
            (&IntLiteral(_), &Some(ref t)) => {
              match t {
                &TypeAnno::Singleton { ref name, ref params } if **name == "Int" && params.len() == 0 => (),
                t => return TypeCheck::new(&format!("Bad type {:?} for int literal", t)),
              }
            },
            _ => (),
          }
        }
      }
    }
    TypeCheck::OK
  }
}

