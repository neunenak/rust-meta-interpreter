use std::rc::Rc;
use std::collections::{HashSet, HashMap};
use std::collections::hash_set::Union;
use std::iter::Iterator;
use std::fmt;
use std::fmt::Write;

use itertools::Itertools;

/* GIANT TODO - use the rust im crate, unless I make this code way less haskell-ish after it's done
  */

use parsing;

type TypeName = Rc<String>;

pub type TypeResult<T> = Result<T, String>;

#[derive(Debug, PartialEq, Clone)]
enum MonoType {
  Const(TypeConst),
  Var(TypeName),
  Function(Box<MonoType>, Box<MonoType>),
}

#[derive(Debug, PartialEq, Clone)]
enum TypeConst {
  Unit,
  Nat,
  Int,
  Float,
  StringT,
  Bool,
  Tuple(Vec<MonoType>),
}

impl MonoType {
  fn free_vars(&self) -> HashSet<TypeName> {
    use self::MonoType::*;
    match self {
      Const(_) => HashSet::new(),
      Var(a) => {
        let mut h = HashSet::new();
        h.insert(a.clone());
        h
      },
      Function(a, b) => {
        a.free_vars().union(&b.free_vars()).cloned().collect()
      },
    }
  }

  //TODO maybe this should be type self, and consume?
  fn apply_substitution(&self, s: &Substitution) -> MonoType {
    use self::MonoType::*;
    match self {
      Const(t) => Const(t.clone()),
      Var(a) => s.0.get(a).map(|x| x.clone()).unwrap_or(Var(a.clone())),
      Function(a, b) => Function(
        Box::new(a.apply_substitution(s)),
        Box::new(b.apply_substitution(s))
      )
    }
  }
}

#[derive(Debug, PartialEq, Clone)]
struct PolyType(HashSet<TypeName>, MonoType);

impl PolyType {
  fn free_vars(&self) -> HashSet<TypeName> {
    let mtype = self.1.free_vars();
    self.0.difference(&mtype).cloned().collect()
  }

  fn apply_substitution(&self, s: &Substitution) -> PolyType {
    let mut map: HashMap<TypeName, MonoType> = HashMap::new();
    for (name, monotype) in s.0.iter() {
      if let None = self.0.get(name) {
        map.insert(name.clone(), monotype.clone());
      }
    }
    let newsub = Substitution(map);
    let new = self.1.apply_substitution(&newsub);
    PolyType(self.0.clone(), new)
  }
}

#[derive(Debug, PartialEq, Clone)]
struct Substitution(HashMap<TypeName, MonoType>);

impl Substitution {
  fn new() -> Substitution {
    Substitution(HashMap::new())
  }

  fn bind_variable(name: &TypeName, var: &MonoType) -> Substitution {
    Substitution(hashmap! {
      name.clone() => var.clone()
    })
  }

  fn merge(self, other: Substitution) -> Substitution {
    let mut map = HashMap::new();
    for (name, ty) in self.0.into_iter() {
      map.insert(name, ty);
    }
    for (name, ty) in other.0.into_iter() {
      map.insert(name, ty);
    }
    Substitution(map)
  }
}


#[derive(Debug, Default)]
struct TypeEnvironment {
  map: HashMap<TypeName, PolyType>,
}

impl TypeEnvironment {
  fn apply_substitution(&self, s: &Substitution) -> TypeEnvironment {
    let mut map = HashMap::new();
    for (name, polytype) in self.map.iter() {
      map.insert(name.clone(), polytype.apply_substitution(s));
    }
    TypeEnvironment { map }
  }

  fn lookup(&self, name: &TypeName) -> Option<PolyType> {
    self.map.get(name).map(|x| x.clone())
  }

  fn extend(&mut self, name: &TypeName, ty: PolyType) {
    self.map.insert(name.clone(), ty);
  }

  fn free_vars(&self) -> HashSet<TypeName> {
    let mut free = HashSet::new();
    for (_, ptype) in self.map.iter() {
      free = free.union(&ptype.free_vars()).cloned().collect()
    }
    free
  }
}

pub struct TypeContext {
  environment: TypeEnvironment,
}

impl TypeContext {
  pub fn new() -> TypeContext {
    TypeContext { environment: TypeEnvironment::default() }
  }
  
  pub fn debug_types(&self) -> String {
    let mut output = format!("Type context\n");
    for (sym, ty) in &self.environment.map {
      write!(output, "{} -> {:?}\n", sym, ty).unwrap();
    }
    output
  }

  pub fn type_check_ast(&mut self, ast: &parsing::AST) -> TypeResult<String> {
    let ref block = ast.0;
    let output = {
      let mut infer = Infer::new(&mut self.environment);
      let output = infer.block(block);
      output
    };

    println!("ENV LOOKS LIKE: {:?}", self.environment);

    match output {
      Ok(s) => Ok(format!("{:?}", s)),
      Err(s) => Err(format!("Error: {:?}", s))
    }
  }
}

struct Infer<'a> {
  env: &'a mut TypeEnvironment,
  _idents: usize
}

#[derive(Debug)]
enum InferError {
  CannotUnify(MonoType, MonoType),
  OccursCheckFailed(TypeName, MonoType),
  UnknownIdentifier(TypeName),
  Custom(String),
}

type InferResult<T> = Result<T, InferError>;

impl<'a> Infer<'a> {

  fn new(env: &'a mut TypeEnvironment) -> Infer {
    Infer {
      env,
      _idents: 0
    }
  }

  fn fresh(&mut self) -> MonoType {
    let i = self._idents;
    self._idents += 1;
    let name = Rc::new(format!("{}", ('a' as u8 + 1) as char));
    MonoType::Var(name)
  }

  fn instantiate(&mut self, ptype: PolyType) -> MonoType {
    let mtype = ptype.1.clone();
    let mut m = HashMap::new();
    for name in ptype.0.iter() {
      m.insert(name.clone(), self.fresh());
    }
    let sub = Substitution(m);
    mtype.apply_substitution(&sub)
  }

  fn generalize(&mut self, ty: MonoType) -> PolyType {
    let free_mtype = ty.free_vars();
    let free_env = self.env.free_vars();
    let diff: HashSet<TypeName> = free_mtype.difference(&free_env).cloned().collect();
    PolyType(diff, ty)
  }

  fn block(&mut self, block: &Vec<parsing::Statement>) -> InferResult<MonoType> {
    let mut ret = MonoType::Const(TypeConst::Unit);
    for s in block {
      ret = match s {
        parsing::Statement::ExpressionStatement(expr) => self.infer_expression(expr)?,
        parsing::Statement::Declaration(decl) => {
          self.infer_declaration(decl)?;
          MonoType::Const(TypeConst::Unit)
        }
      }
    }
    Ok(ret)
  }

  fn infer_declaration(&mut self, decl: &parsing::Declaration) -> InferResult<MonoType> {
    use parsing::Declaration::*;
    use parsing::Signature;
    match decl {
      Binding { name, expr, .. } => {
        let tau: MonoType = self.infer_expression(&expr)?;
        let sigma = self.generalize(tau);
        self.env.extend(name, sigma);
      },
      FuncDecl(Signature { name, params, type_anno }, block) => {

        let mut fn_type_env = TypeEnvironment::default();
        let mut local_infer = Infer::new(&mut fn_type_env);

        let mut arg_types: Vec<MonoType> = Vec::new();

        for (param_name, maybe_type) in params {
          println!("HANDLING PARAM: {}", param_name);
          let tau = local_infer.fresh();
          let sigma = PolyType(HashSet::new(), tau);
          local_infer.env.extend(param_name, sigma);
        }

        let ret_type = local_infer.block(block)?;
        println!("RET TYPE: {:?}", ret_type);

        let mut final_type = MonoType::Function(Box::new(MonoType::Const(TypeConst::Unit)), Box::new(ret_type));
        println!("ARG TYPES: {:?}", arg_types);

        for ty in arg_types.into_iter().rev() {
          final_type = MonoType::Function(Box::new(ty), Box::new(final_type));
        }
        
        let final_ptype = self.generalize(final_type);

        self.env.extend(name, final_ptype);
      },
      _ => return Err(InferError::Custom(format!("This decl not yet supported")))
    }
    Ok(MonoType::Const(TypeConst::Unit))
  }

  fn infer_expression(&mut self, expr: &parsing::Expression) -> InferResult<MonoType> {
    match expr {
      parsing::Expression(e, Some(anno)) => {
        return Err(InferError::Custom(format!("Annotations not done yet")))
        /*
        let anno_ty = anno.to_type()?;
        let ty = self.infer_exprtype(&e)?;
        self.unify(ty, anno_ty)
        */
      },
      parsing::Expression(e, None) => self.infer_expression_type(e)
    }
  }

  fn infer_expression_type(&mut self, expr: &parsing::ExpressionType) -> InferResult<MonoType> {
    use self::parsing::ExpressionType::*;
    Ok(match expr {
      NatLiteral(_) => MonoType::Const(TypeConst::Nat),
      FloatLiteral(_) => MonoType::Const(TypeConst::Float),
      StringLiteral(_) => MonoType::Const(TypeConst::StringT),
      BoolLiteral(_) => MonoType::Const(TypeConst::Bool),
      Value(name) => {
        let sigma = match self.env.lookup(name) {
          Some(ty) => ty,
          None => return Err(InferError::UnknownIdentifier(name.clone())),
        };
        let tau = self.instantiate(sigma);
        tau
      },
      Call { f, arguments } => {
        /*
        let sigma = match sel
        */
        unimplemented!()
      },
      e => return Err(InferError::Custom(format!("this expression type not done yet: {:?}", e)))
    })
  }
}

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

