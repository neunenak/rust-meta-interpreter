use std::rc::Rc;
use std::collections::HashMap;

macro_rules! bx {
  ($e:expr) => { Box::new($e) }
}

use schala_lang::parsing;

pub struct TypeContext { 
  bindings: HashMap<Rc<String>, Type>
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
  Const(TConst),
  Func(Box<Type>, Box<Type>),
  Void
}

#[derive(Debug, PartialEq, Clone)]
pub enum TConst {
  Unit,
  Int,
  Float,
  StringT,
  Bool,
  Custom(String),
}

type TypeResult<T> = Result<T, String>;

impl TypeContext {
  pub fn new() -> TypeContext {
    TypeContext { bindings: HashMap::new() }
  }
  pub fn type_check_ast(&mut self, ast: &parsing::AST) -> TypeResult<Type> {
    use self::Type::*; use self::TConst::*;
    let mut ret_type = Const(Unit);
    for statement in ast.0.iter() {
      ret_type = self.type_check_statement(statement)?;
    }
    Ok(ret_type)
  }
  fn type_check_statement(&mut self, statement: &parsing::Statement) -> TypeResult<Type> {
    println!("statement should be: {:?}", statement);

    use self::parsing::Statement::*;
    match statement {
      &ExpressionStatement(ref expr) => self.infer(expr),
      &Declaration(ref decl) => self.add_declaration(decl),
    }
  }
  fn add_declaration(&mut self, decl: &parsing::Declaration) -> TypeResult<Type> {
    use self::parsing::Declaration::*;
    use self::Type::*;
    match decl {
      &Binding { ref name, ref expr, .. } => {
        let ty = self.infer(expr)?;
        self.bindings.insert(name.clone(), ty);
      },
      _ => return Err(format!("other formats not done"))
    }
    Ok(Void)
  }
  fn infer(&mut self, expr: &parsing::Expression) -> TypeResult<Type> {
    use self::parsing::Expression;
    match expr {
      &Expression(ref e, Some(ref anno)) => {
        let anno_ty = self.type_from_anno(anno)?;
        let ty = self.infer_exprtype(&e)?;
        self.unify(ty, anno_ty)
      },
      &Expression(ref e, None) => self.infer_exprtype(e)
    }
  }
  fn infer_exprtype(&mut self, expr: &parsing::ExpressionType) -> TypeResult<Type> {
    use self::parsing::ExpressionType::*;
    use self::Type::*; use self::TConst::*;
    match expr {
      &IntLiteral(_) => Ok(Const(Int)),
      &FloatLiteral(_) => Ok(Const(Float)),
      &StringLiteral(_) => Ok(Const(StringT)),
      &BoolLiteral(_) => Ok(Const(Bool)),
      &BinExp(ref op, ref lhs, ref rhs) => { /* remember there are both the haskell convention talk and the write you a haskell ways to do this! */
        match self.infer_optype(op)? {
          Func(box t1, box Func(box t2, box t3)) => {
            let lhs_ty = self.infer(lhs)?;
            let rhs_ty = self.infer(rhs)?;
            self.unify(t1, lhs_ty)?;
            self.unify(t2, rhs_ty)?;
            Ok(t3)
          },
          other => return Err(format!("{:?} is not a binary function type", other))
        }
      },
      /*
  BinExp(Operation, Box<Expression>, Box<Expression>),
  PrefixExp(Operation, Box<Expression>),
  TupleLiteral(Vec<Expression>),
  Value(Rc<String>, Vec<(Rc<String>, Expression)>),
  Call {
    f: Box<Expression>,
    arguments: Vec<Expression>,
  },
  Index {
    indexee: Box<Expression>,
    indexers: Vec<Expression>,
  },
  IfExpression(Box<Expression>, Vec<Statement>, Option<Vec<Statement>>),
  MatchExpression(Box<Expression>, Vec<MatchArm>),
  ForExpression
      */
      _ => Err(format!("Type not yet implemented"))
    }
  }
  fn infer_optype(&mut self, _op: &parsing::Operation) -> TypeResult<Type> {
    use self::Type::*; use self::TConst::*;
    //this is a shim; not all ops are binops from int -> int -> int
    Ok(Func(bx!(Const(Int)), bx!(Func(bx!(Const(Int)), bx!(Const(Int))))))
  }
  fn type_from_anno(&mut self, anno: &parsing::TypeName) -> TypeResult<Type> {
    use self::parsing::TypeSingletonName;
    use self::parsing::TypeName::*;
    use self::Type::*; use self::TConst::*;
    Ok(match anno {
      &Tuple(_) => return Err(format!("Tuples not yet implemented")),
      &Singleton(ref name) => match name {
        &TypeSingletonName { ref name, .. } => match &name[..] {
          "Int" => Const(Int),
          "Float" => Const(Float),
          "Bool" => Const(Bool),
          "String" => Const(StringT),
          n => Const(Custom(n.to_string()))
        }
      }
    })
  }
  fn unify(&mut self, t1: Type, t2: Type) -> TypeResult<Type> {
    use self::Type::*;// use self::TConst::*;
    match (t1, t2) {
      (Const(ref a), Const(ref b)) if a == b => Ok(Const(a.clone())),
      (a, b) => Err(format!("Types {:?} and {:?} don't unify", a, b))
    }
  }
}

