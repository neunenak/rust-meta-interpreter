use std::rc::Rc;

use schala_lang::parsing;

pub struct TypeContext { }

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
  Unit,
  Int,
  Float,
  StringT,
  Bool,
  Custom(Rc<String>),
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
    println!("statement should be: {:?}", statement);

    use self::parsing::Statement::*;
    match statement {
      &ExpressionStatement(ref expr) => self.type_infer(expr),
      &Declaration(ref decl) => self.type_check_declaration(decl),
    }
  }
  fn type_check_declaration(&mut self, decl: &parsing::Declaration) -> TypeResult<Type> {
    use self::Type::*;
    Ok(Unit)
  }
  fn type_infer(&mut self, expr: &parsing::Expression) -> TypeResult<Type> {
    use self::parsing::Expression;
    use self::parsing::ExpressionType::*;
    use self::Type::*;
    match expr {
      &Expression(ref e, Some(ref ty)) => Err(format!("Anno not implemented")),
      &Expression(ref e, None) => match e {
        &IntLiteral(_) => Ok(Int),
        &FloatLiteral(_) => Ok(Float),
        &StringLiteral(_) => Ok(StringT),
        &BoolLiteral(_) => Ok(Bool),
        _ => Err(format!("Type not yet implemented"))
      }
    }
  }
  fn unify(&mut self, t1: Type, t2: Type) -> TypeResult<Type> {
    use self::Type::*;
    Ok(Unit)
  }
}

