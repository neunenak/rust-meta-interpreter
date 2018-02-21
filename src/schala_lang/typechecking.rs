use std::rc::Rc;

use schala_lang::parsing;

pub struct TypeContext { }

#[derive(Debug, PartialEq)]
pub enum Type {
  Unit,
  Void
}

type TypeResult<T> = Result<T, String>;

impl TypeContext {
  pub fn new() -> TypeContext {
    TypeContext { }
  }
  pub fn type_check_ast(&mut self, ast: &parsing::AST) -> TypeResult<Type> {
    use self::Type::*;
    let mut ret_type = Unit;
    for statement in ast.0.iter() {
      ret_type = self.type_check_statement(statement)?;
    }
    Ok(ret_type)
  }
  fn type_check_statement(&mut self, statement: &parsing::Statement) -> TypeResult<Type> {
    use self::parsing::Statement::*;
    match statement {
      &ExpressionStatement(ref expr) => self.type_check_expression(expr),
      &Declaration(ref decl) => self.type_check_declaration(decl),
    }
  }
  fn type_check_declaration(&mut self, decl: &parsing::Declaration) -> TypeResult<Type> {
    use self::Type::*;
    Ok(Unit)
  }
  fn type_check_expression(&mut self, expr: &parsing::Expression) -> TypeResult<Type> {
    use self::Type::*;
    Ok(Void)
  }
}

