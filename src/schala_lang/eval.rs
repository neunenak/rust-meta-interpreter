use schala_lang::parsing::{AST, Statement, Declaration, Expression, ExpressionType};

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

impl ReplState {
  pub fn type_check(&mut self, _ast: &AST) -> TypeCheck {
    //TypeCheck::Error("type lol".to_string())
    TypeCheck::OK
  }
}

