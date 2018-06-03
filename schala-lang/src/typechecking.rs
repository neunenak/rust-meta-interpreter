use std::cell::RefCell;
use std::rc::Rc;
use std::collections::{HashSet, HashMap};
/*
use std::collections::hash_set::Union;
use std::iter::Iterator;
use std::fmt;
use std::fmt::Write;
use itertools::Itertools;
*/

use parsing;
use util::StateStack;
use symbol_table::{SymbolSpec, Symbol, SymbolTable};

pub type TypeName = Rc<String>;
type TypeResult<T> = Result<T, String>;

#[derive(Debug, PartialEq, Clone)]
enum Type {
  Const(TConst),
  Var(TypeName),
  Func(Vec<Type>),
}

#[derive(Debug, PartialEq, Clone)]
enum TConst {
  Unit,
  Nat,
  StringT,
  Custom(String)
}

#[derive(Debug, PartialEq, Clone)]
struct Scheme {
  names: Vec<TypeName>,
  ty: Type,
}

#[derive(Debug, PartialEq, Clone)]
struct Substitution(HashMap<TypeName, Type>);
impl Substitution {
  fn empty() -> Substitution {
    Substitution(HashMap::new())
  }
}

#[derive(Debug, PartialEq, Clone)]
struct TypeEnv(HashMap<TypeName, Scheme>);

impl TypeEnv {
  fn default() -> TypeEnv {
    TypeEnv(HashMap::new())
  }
}

pub struct TypeContext<'a> {
  values: StateStack<'a, TypeName, Type>,
  symbol_table_handle: Rc<RefCell<SymbolTable>>,
  global_env: TypeEnv
}

impl<'a> TypeContext<'a> {
  pub fn new(symbol_table_handle: Rc<RefCell<SymbolTable>>) -> TypeContext<'static> {
    TypeContext { values: StateStack::new(None), global_env: TypeEnv::default(), symbol_table_handle }
  }

  pub fn debug_types(&self) -> String {
    format!("{:?}", self.global_env)
  }

  pub fn type_check_ast(&mut self, input: &parsing::AST) -> Result<String, String> {
    let ref mut global_env = self.global_env;
    let output = global_env.infer_block(&input.0)?;
    Ok(format!("{:?}", output))
  }
}

impl TypeEnv {
  fn instantiate(&mut self, sigma: Scheme) -> Type {
    match sigma {
      Scheme { ty, .. } => ty,
    }
  }
  fn generate(&mut self, ty: Type) -> Scheme {
    Scheme {
      names: vec![], //TODO incomplete
      ty
    }
  }
  fn infer_block(&mut self, block: &Vec<parsing::Statement>) -> TypeResult<Type> {
    let mut output = Type::Const(TConst::Unit);
    for statement in block {
      output = self.infer_statement(statement)?;
    }
    Ok(output)
  }
  fn infer_statement(&mut self, statement: &parsing::Statement) -> TypeResult<Type> {
    match statement {
      parsing::Statement::ExpressionStatement(expr) => self.infer_expr(expr),
      parsing::Statement::Declaration(decl) => self.infer_decl(decl)
    }
  }
  fn infer_decl(&mut self, decl: &parsing::Declaration) -> TypeResult<Type> {
    use parsing::Declaration::*;
    match decl {
      Binding { name, expr, .. } => {
        let ty = self.infer_expr(expr)?;
        let sigma = self.generate(ty);
        self.0.insert(name.clone(), sigma);
      },
      _ => (),
    }
    Ok(Type::Const(TConst::Unit))
  }
  fn infer_expr(&mut self, expr: &parsing::Expression) -> TypeResult<Type> {
    match expr {
      parsing::Expression(expr, Some(anno)) => {
        self.infer_exprtype(expr)
      },
      parsing::Expression(expr, None) => {
        self.infer_exprtype(expr)
      }
    }
  }

  fn infer_exprtype(&mut self, expr: &parsing::ExpressionType) -> TypeResult<Type> {
    use self::TConst::*;
    use parsing::ExpressionType::*;
    Ok(match expr {
      NatLiteral(_) => Type::Const(Nat),
      StringLiteral(_) => Type::Const(StringT),
      Call { f, arguments } =>  {

        return Err(format!("NOTDONE"))
      },
      Value(name) => {
        let s = match self.0.get(name) {
          Some(sigma) => sigma.clone(),
          None => return Err(format!("Unknown variable: {}", name))
        };
        self.instantiate(s)
      },
      _ => Type::Const(Unit)
    })
  }
}


/* GIANT TODO - use the rust im crate, unless I make this code way less haskell-ish after it's done
  */

/*


pub type TypeResult<T> = Result<T, String>;
*/

/* TODO this should just check the name against a map, and that map should be pre-populated with
 * types */
/*
impl parsing::TypeName {
  fn to_type(&self) -> TypeResult<Type> {
	use self::parsing::TypeSingletonName;
	use self::parsing::TypeName::*;
	use self::Type::*; use self::TConstOld::*;
    Ok(match self {
      Tuple(_) => return Err(format!("Tuples not yet implemented")),
      Singleton(name) => match name {
        TypeSingletonName { name, .. } => match &name[..] {
          /*
          "Nat" => Const(Nat),
          "Int" => Const(Int),
          "Float" => Const(Float),
          "Bool" => Const(Bool),
          "String" => Const(StringT),
          */
          n => Const(Custom(n.to_string()))
        }
      }
    })
  }
}
*/



/*
impl TypeContext {
  pub fn type_check_ast(&mut self, ast: &parsing::AST) -> TypeResult<String> {
    let ref block = ast.0;
    let mut infer = Infer::default();
    let env = TypeEnvironment::default();
    let output = infer.infer_block(block, &env);
    match output {
      Ok(s) => Ok(format!("{:?}", s)),
      Err(s) => Err(format!("Error: {:?}", s))
    }
  }
}

// this is the equivalent of the Haskell Infer monad
#[derive(Debug, Default)]
struct Infer {
  _idents: u32,
}

#[derive(Debug)]
enum InferError {
  CannotUnify(MonoType, MonoType),
  OccursCheckFailed(Rc<String>, MonoType),
  UnknownIdentifier(Rc<String>),
  Custom(String),
}

type InferResult<T> = Result<T, InferError>;


impl Infer {
  fn fresh(&mut self) -> MonoType {
    let i = self._idents;
    self._idents += 1;
    let name = Rc::new(format!("{}", ('a' as u8 + 1) as char));
    MonoType::Var(name)
  }

  fn unify(&mut self, a: MonoType, b: MonoType) -> InferResult<Substitution> {
    use self::InferError::*; use self::MonoType::*;
    Ok(match (a, b) {
      (Const(ref a), Const(ref b)) if a == b => Substitution::new(),
      (Var(ref name), ref var) => Substitution::bind_variable(name, var),
      (ref var, Var(ref name)) => Substitution::bind_variable(name, var),
      (Function(box a1, box b1), Function(box a2, box b2)) => {
        let s1 = self.unify(a1, a2)?;
        let s2 = self.unify(b1.apply_substitution(&s1), b2.apply_substitution(&s1))?;
        s1.merge(s2)
      },
      (a, b) => return Err(CannotUnify(a, b))
    })
  }

  fn infer_block(&mut self, block: &Vec<parsing::Statement>, env: &TypeEnvironment) -> InferResult<MonoType> {
    use self::parsing::Statement;
    let mut ret = MonoType::Const(TypeConst::Unit);
    for statement in block.iter() {
      ret = match statement {
        Statement::ExpressionStatement(expr) => {
          let (sub, ty) = self.infer_expr(expr, env)?;
          //TODO handle substitution monadically

          ty
        }
        Statement::Declaration(decl) => MonoType::Const(TypeConst::Unit),
      }
    }
    Ok(ret)
  }

  fn infer_expr(&mut self, expr: &parsing::Expression, env: &TypeEnvironment) -> InferResult<(Substitution, MonoType)> {
    use self::parsing::Expression;
    match expr {
      Expression(e, Some(anno)) => self.infer_annotated_expr(e, anno, env),
      /*
        let anno_ty = anno.to_type()?;
        let ty = self.infer_exprtype(&e)?;
        self.unify(ty, anno_ty)
      },
      */
      Expression(e, None) => self.infer_exprtype(e, env)
    }
  }

  fn infer_annotated_expr(&mut self, expr: &parsing::ExpressionType, anno: &parsing::TypeName, env: &TypeEnvironment) -> InferResult<(Substitution, MonoType)> {
    Err(InferError::Custom(format!("exprtype not done: {:?}", expr)))
  }
  fn infer_exprtype(&mut self, expr: &parsing::ExpressionType, env: &TypeEnvironment) -> InferResult<(Substitution, MonoType)> {
    use self::parsing::ExpressionType::*;
    use self::TypeConst::*;
    Ok(match expr {
      NatLiteral(_) => (Substitution::new(), MonoType::Const(Nat)),
      FloatLiteral(_) => (Substitution::new(), MonoType::Const(Float)),
      StringLiteral(_) => (Substitution::new(), MonoType::Const(StringT)),
      BoolLiteral(_) => (Substitution::new(), MonoType::Const(Bool)),
      Value(name) => match env.lookup(name) {
        Some(sigma) => {
          let tau = self.instantiate(&sigma);
          (Substitution::new(), tau)
        },
        None => return Err(InferError::UnknownIdentifier(name.clone())),
      },
      e => return Err(InferError::Custom(format!("Type inference for {:?} not done", e)))
    })
  }
  fn instantiate(&mut self, sigma: &PolyType) -> MonoType {
    let ref ty: MonoType = sigma.1;
    let mut subst = Substitution::new();

    for name in sigma.0.iter() {
      let fresh_mvar = self.fresh();
      let new = Substitution::bind_variable(name, &fresh_mvar);
      subst = subst.merge(new);
    }
    ty.apply_substitution(&subst)
  }
}
*/












/* OLD STUFF DOWN HERE */



/*
impl TypeContext {
  fn infer_block(&mut self, statements: &Vec<parsing::Statement>) -> TypeResult<Type> {
    let mut ret_type = Type::Const(TConst::Unit);
    for statement in statements {
      ret_type = self.infer_statement(statement)?;
    }
    Ok(ret_type)
  }

  fn infer_statement(&mut self, statement: &parsing::Statement) -> TypeResult<Type> {
    use self::parsing::Statement::*;
    match statement {
      ExpressionStatement(expr) => self.infer(expr),
      Declaration(decl) => self.add_declaration(decl),
    }
  }
  fn add_declaration(&mut self, decl: &parsing::Declaration) -> TypeResult<Type> {
    use self::parsing::Declaration::*;
    use self::Type::*;
    match decl {
      Binding { name, expr, .. } => {
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
      Expression(e, Some(anno)) => {
        let anno_ty = anno.to_type()?;
        let ty = self.infer_exprtype(&e)?;
        self.unify(ty, anno_ty)
      },
      Expression(e, None) => self.infer_exprtype(e)
    }
  }
  fn infer_exprtype(&mut self, expr: &parsing::ExpressionType) -> TypeResult<Type> {
    use self::parsing::ExpressionType::*;
    use self::Type::*; use self::TConst::*;
    match expr {
      NatLiteral(_) => Ok(Const(Nat)),
      FloatLiteral(_) => Ok(Const(Float)),
      StringLiteral(_) => Ok(Const(StringT)),
      BoolLiteral(_) => Ok(Const(Bool)),
      BinExp(op, lhs, rhs) => { /* remember there are both the haskell convention talk and the write you a haskell ways to do this! */
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
      PrefixExp(op, expr) => match op.get_type()? {
        Func(box t1, box t2) => {
          let expr_ty = self.infer(expr)?;
          self.unify(t1, expr_ty)?;
          Ok(t2)
        },
        other => Err(format!("{:?} is not a prefix op function type", other))
      },
      Value(name) => {
        match self.bindings.get(name) {
          Some(ty) => Ok(ty.clone()),
          None => Err(format!("No binding found for variable: {}", name)),
        }
      },
      Call { f, arguments } => {
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
      TupleLiteral(expressions) => {
        let mut types = vec![];
        for expr in expressions {
          types.push(self.infer(expr)?);
        }
        Ok(Sum(types))
      },
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
*/


#[cfg(test)]
mod tests {
  use super::{Type, TConst, TypeContext};
  use super::Type::*;
  use super::TConst::*;

  macro_rules! type_test {
    ($input:expr, $correct:expr) => {
      {
      let mut tc = TypeContext::new();
      let ast = ::parsing::parse(::tokenizing::tokenize($input)).0.unwrap() ;
      //tc.add_symbols(&ast);
      assert_eq!($correct, tc.infer_block(&ast.0).unwrap())
      }
    }
  }

  #[test]
  fn basic_inference() {
    type_test!("30", Const(Nat));
    //type_test!("fn x(a: Int): Bool {}; x(1)", TConst(Boolean));
  }
}
