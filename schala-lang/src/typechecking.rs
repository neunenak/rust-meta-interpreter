use std::rc::Rc;
use std::collections::{HashSet, HashMap};
use std::collections::hash_set::Union;
use std::iter::Iterator;
//use std::char;
use std::fmt;
use std::fmt::Write;

use itertools::Itertools;


/* GIANT TODO - use the rust im crate, unless I make this code way less haskell-ish after it's done
  */

use parsing;

pub struct TypeContext {
  bindings: HashMap<Rc<String>, Type>,
  pub symbol_table: SymbolTable
}

//cf. p. 150 or so of Language Implementation Patterns
pub struct SymbolTable {
  pub values: HashMap<Rc<String>, Symbol> //TODO this will eventually have real type information
}

impl SymbolTable {
  fn new() -> SymbolTable {
    SymbolTable { values: HashMap::new() }
  }
}

#[derive(Debug)]
pub struct Symbol {
  pub name: Rc<String>,
  pub spec: SymbolSpec,
}

#[derive(Debug)]
pub enum SymbolSpec {
  Func, Custom(String)
}


/* real meat of type stuff here */

#[derive(Debug, PartialEq, Clone)]
enum MonoType {
  Const(TypeConst),
  Var(Rc<String>),
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
  fn free_vars(&self) -> HashSet<Rc<String>> {
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
struct PolyType(HashSet<Rc<String>>, MonoType);

impl PolyType {
  fn free_vars(&self) -> HashSet<Rc<String>> {
    let mtype = self.1.free_vars();
    self.0.difference(&mtype).cloned().collect()
  }

  fn apply_substitution(&self, s: &Substitution) -> PolyType {
    let mut map: HashMap<Rc<String>, MonoType> = HashMap::new();
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
struct Substitution(HashMap<Rc<String>, MonoType>);

impl Substitution {
  fn new() -> Substitution {
    Substitution(HashMap::new())
  }

  fn bind_variable(name: &Rc<String>, var: &MonoType) -> Substitution {
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


#[derive(Debug)]
struct TypeEnvironment {
  map: HashMap<Rc<String>, PolyType>,
}

impl TypeEnvironment {
  fn apply_substitution(&self, s: &Substitution) -> TypeEnvironment {
    let mut map = HashMap::new();
    for (name, polytype) in self.map.iter() {
      map.insert(name.clone(), polytype.apply_substitution(s));
    }
    TypeEnvironment { map }
  }
}




#[derive(Debug, PartialEq, Clone)]
pub enum Type {
  Const(TConstOld),
  Func(Box<Type>, Box<Type>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum TConstOld {
  Nat,
  Int,
  Float,
  StringT,
  Bool,
  Custom(String),
}

impl fmt::Display for Type {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{:?}", self)
  }
}

/* TODO this should just check the name against a map, and that map should be pre-populated with
 * types */
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

pub type TypeResult<T> = Result<T, String>;

impl TypeContext {
  pub fn new() -> TypeContext {
    TypeContext { bindings: HashMap::new(), /*type_var_count: 0*/ symbol_table: SymbolTable::new() }
  }

  /* note: this adds names for *forward reference* but doesn't actually create any types. solve that problem
   * later */
  pub fn add_top_level_types(&mut self, ast: &parsing::AST) -> TypeResult<()> {
    use self::parsing::{Statement, TypeName, Variant, TypeSingletonName, TypeBody};
    use self::parsing::Declaration::*;
    use self::Type::*;
    for statement in ast.0.iter() {
      if let Statement::Declaration(decl) = statement {
        match decl {
          FuncSig(signature) | FuncDecl(signature, _) => {
            self.symbol_table.values.insert(
              signature.name.clone(),
              Symbol { name: signature.name.clone(), spec: SymbolSpec::Func }
            );
          },
          TypeDecl(TypeSingletonName { name, ..}, TypeBody(variants)) => {
            for var in variants {
              match var {
                Variant::UnitStruct(variant_name) => {
                  //TODO will have to make this a function to this type eventually
                  let spec = SymbolSpec::Custom(format!("{}", name));
                  self.symbol_table.values.insert(variant_name.clone(), Symbol { name: variant_name.clone(), spec });
                },
                e => return Err(format!("{:?} not supported in typing yet", e)),
              }
            }
          },
          _ => ()
        }
      }
    }
    Ok(())
  }
  pub fn debug_symbol_table(&self) -> String {
    let mut output = format!("Symbol table\n");
    for (sym, ty) in &self.symbol_table.values {
      write!(output, "{} -> {:?}\n", sym, ty).unwrap();
    }
    write!(output, "\nBindings\n").unwrap();
    for (sym, ty) in &self.bindings {
      write!(output, "{} : {:?}\n", sym, ty).unwrap();
    }
    output
  }

  pub fn type_check_ast(&mut self, ast: &parsing::AST) -> TypeResult<String> {
    let ref block = ast.0;
    let mut infer = Infer::default();
    let output = infer.infer_block(block);
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
  UnknownIdentifier(Rc<String>)
}

impl Infer {
  fn fresh(&mut self) -> MonoType {
    let i = self._idents;
    self._idents += 1;
    let name = Rc::new(format!("{}", ('a' as u8 + 1) as char));
    MonoType::Var(name)
  }

  fn infer_block(&mut self, block: &Vec<parsing::Statement>) -> Result<MonoType, InferError> {
    Ok(MonoType::Const(TypeConst::Unit))
  }

  fn unify(&mut self, a: MonoType, b: MonoType) -> Result<Substitution, InferError> {
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
}




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

