use schala_lang::parsing::{AST, Statement, Declaration, Expression, ExpressionType, TypeAnno};

pub struct ReplState {
}

impl ReplState {
  pub fn new() -> ReplState {
    ReplState { }
  }

  pub fn evaluate(&mut self, ast: AST) -> String {
    let mut acc = String::new();
    for statement in ast.0 {
      if let Some(output) = self.eval_statement(statement) {
        acc.push_str(&output);
        acc.push_str("\n");
      }
    }
    format!("{}", acc)
  }
}

impl ReplState {
  fn eval_statement(&mut self, statement: Statement) -> Option<String> {
    match statement {
      Statement::ExpressionStatement(expr) => self.eval_expr(expr),
      Statement::Declaration(decl) => self.eval_decl(decl),
    }
  }

  fn eval_decl(&mut self, decl: Declaration) -> Option<String> {
    Some("UNIMPLEMENTED".to_string())
  }

  fn eval_expr(&mut self, expr: Expression) -> Option<String> {
    use self::ExpressionType::*;

    let expr_type = expr.0;
    Some(match expr_type {
      IntLiteral(n) => format!("{}", n),
      FloatLiteral(f) => format!("{}", f),
      StringLiteral(s) => format!("{}", s),
      BoolLiteral(b) => format!("{}", b),
      _ => format!("UNIMPLEMENTED"),
    })
  }
}

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
        &Statement::Declaration(ref decl) => {
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

