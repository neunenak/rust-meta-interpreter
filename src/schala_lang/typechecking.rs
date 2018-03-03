use std::rc::Rc;
use std::collections::HashMap;
use std::char;
use std::fmt;
use std::fmt::Write;

use schala_lang::parsing;

pub struct TypeContext { 
  type_var_count: u64,
  bindings: HashMap<Rc<String>, Type>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
  Const(TConst),
  Func(Box<Type>, Box<Type>),
  UVar(String),
  EVar(u64),
  Void
}

impl fmt::Display for Type {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    use self::Type::*;
    match self {
      &Const(ref c) => write!(f, "{:?}", c),
      &Func(ref a, ref b) => write!(f, "{} -> {}", a, b),
      &UVar(ref s) => write!(f, "{}_u", s),
      &EVar(ref n) => write!(f, "{}_e", n),
      &Void => write!(f, "Void")
    }
  }
}

#[derive(Default)]
struct UVarGenerator {
  n: u32,
}
impl UVarGenerator {
  fn new() -> UVarGenerator {
    UVarGenerator::default()
  }
  fn next(&mut self) -> Type {
    //TODO handle this in the case where someone wants to make a function with more than 26 variables
    let s = format!("{}", unsafe { char::from_u32_unchecked(self.n + ('a' as u32)) });
    self.n += 1;
    Type::UVar(s)
  }
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

impl parsing::TypeName {
  fn to_type(&self) -> TypeResult<Type> {
    use self::parsing::TypeSingletonName;
    use self::parsing::TypeName::*;
    use self::Type::*; use self::TConst::*;
    Ok(match self {
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
}

pub type TypeResult<T> = Result<T, String>;

impl TypeContext {
  pub fn new() -> TypeContext {
    TypeContext { bindings: HashMap::new(), type_var_count: 0 }
  }
  pub fn fresh(&mut self) -> Type {
    let ret = self.type_var_count;
    self.type_var_count += 1;
    Type::EVar(ret)
  }
}

impl TypeContext {
  pub fn add_top_level_types(&mut self, ast: &parsing::AST) -> TypeResult<()> {
    use self::parsing::TypeName;
    use self::parsing::Declaration::*;
    use self::Type::*;
    for statement in ast.0.iter() {
      if let &self::parsing::Statement::Declaration(ref decl) = statement {
        match decl {
          &FuncSig(ref signature) | &FuncDecl(ref signature, _) => {
            let mut uvar_gen = UVarGenerator::new();
            let mut ty: Type = signature.type_anno.as_ref().map(|name: &TypeName| name.to_type()).unwrap_or_else(|| {Ok(uvar_gen.next())} )?;
            for &(_, ref type_name) in signature.params.iter().rev() {
              let arg_type = type_name.as_ref().map(|name| name.to_type()).unwrap_or_else(|| {Ok(uvar_gen.next())} )?;
              ty = Func(bx!(arg_type), bx!(ty));
            }
            self.bindings.insert(signature.name.clone(), ty);
          },
          _ => ()
        }
      }
    }
    Ok(())
  }
  pub fn debug_symbol_table(&self) -> String {
    let mut output = format!("Symbols\n");
    for (sym, ty) in &self.bindings {
      write!(output, "{} : {}\n", sym, ty).unwrap();
    }
    output
  }
}

impl TypeContext {
  pub fn type_check_ast(&mut self, ast: &parsing::AST) -> TypeResult<Type> {
    use self::Type::*; use self::TConst::*;
    let mut ret_type = Const(Unit);
    for statement in ast.0.iter() {
      ret_type = self.type_check_statement(statement)?;
    }
    Ok(ret_type)
  }
  fn type_check_statement(&mut self, statement: &parsing::Statement) -> TypeResult<Type> {
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
        let anno_ty = anno.to_type()?;
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
        match op.get_type()? {
          Func(box t1, box Func(box t2, box t3)) => {
            let lhs_ty = self.infer(lhs)?;
            let rhs_ty = self.infer(rhs)?;
            self.unify(t1, lhs_ty)?;
            self.unify(t2, rhs_ty)?;
            Ok(t3)
          },
          other => Err(format!("{:?} is not a binary function type", other))
        }
      },
      &PrefixExp(ref op, ref expr) => match op.get_type()? {
        Func(box t1, box t2) => {
          let expr_ty = self.infer(expr)?;
          self.unify(t1, expr_ty)?;
          Ok(t2)
        },
        other => Err(format!("{:?} is not a prefix op function type", other))
      },
      &Value(ref name) => {
        match self.bindings.get(name) {
          Some(ty) => Ok(ty.clone()),
          None => Err(format!("No binding found for variable: {}", name)),
        }
      },
      &Call { ref f, ref arguments } => {
        let mut tf = self.infer(f)?;
        for arg in arguments.iter() {
          match tf {
            Func(box t, box rest) => {
              let t_arg = self.infer(arg)?;
              self.unify(t, t_arg)?;
              tf = rest;
            },
            other => return Err(format!("Function call failed to unify; last type: {:?}", other)),
          }
        }
        Ok(tf)
      },
      /*
  TupleLiteral(Vec<Expression>),
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
  fn unify(&mut self, t1: Type, t2: Type) -> TypeResult<Type> {
    use self::Type::*;// use self::TConst::*;
    match (t1, t2) {
      (Const(ref a), Const(ref b)) if a == b => Ok(Const(a.clone())),
      (a, b) => Err(format!("Types {:?} and {:?} don't unify", a, b))
    }
  }
}

