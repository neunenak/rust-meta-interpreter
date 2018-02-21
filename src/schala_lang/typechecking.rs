use std::rc::Rc;

use schala_lang::parsing;

pub struct TypeContext { }

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
  Unit,
  Const(TConst),
  Void
}

#[derive(Debug, PartialEq, Clone)]
pub enum TConst {
  Int,
  Float,
  StringT,
  Bool,
  Custom(String),
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
    match expr {
      &Expression(ref e, Some(ref anno)) => {
        let anno_ty = self.type_from_anno(anno)?;
        let ty = self.type_infer_exprtype(&e)?;
        self.unify(ty, anno_ty)
      },
      &Expression(ref e, None) => self.type_infer_exprtype(e)
    }
  }
  fn type_infer_exprtype(&mut self, expr: &parsing::ExpressionType) -> TypeResult<Type> {
    use self::parsing::ExpressionType::*;
    use self::Type::*; use self::TConst::*;
    match expr {
      &IntLiteral(_) => Ok(Const(Int)),
      &FloatLiteral(_) => Ok(Const(Float)),
      &StringLiteral(_) => Ok(Const(StringT)),
      &BoolLiteral(_) => Ok(Const(Bool)),
      _ => Err(format!("Type not yet implemented"))
    }
  }
  fn type_from_anno(&mut self, anno: &parsing::TypeName) -> TypeResult<Type> {
    use self::Type::*; use self::TConst::*;
    Ok(Unit)
  }
  fn unify(&mut self, t1: Type, t2: Type) -> TypeResult<Type> {
    use self::Type::*; use self::TConst::*;
    match (t1, t2) {
      (Const(ref a), Const(ref b)) if a == b => Ok(Const(a.clone())),
      (a, b) => Err(format!("Types {:?} and {:?} don't unify", a, b))
    }
  }
}

