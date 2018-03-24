use std::collections::HashMap;
use std::rc::Rc;


use parsing::{AST, Statement, Declaration, Signature, Expression, ExpressionType, Operation, Variant, TypeName, TypeSingletonName};

// from Niko's talk
/* fn type_check(expression, expected_ty) -> Ty {
    let ty = bare_type_check(expression, expected_type);
    if ty icompatible with expected_ty {
        try_coerce(expression, ty, expected_ty)
    } else {
      ty
    }
  }

  fn bare_type_check(exprssion, expected_type) -> Ty { ... }
 */

/* H-M ALGO NOTES
from https://www.youtube.com/watch?v=il3gD7XMdmA
(also check out http://dev.stephendiehl.com/fun/006_hindley_milner.html)

typeInfer :: Expr a -> Matching (Type a)
unify :: Type a -> Type b -> Matching (Type c)

(Matching a) is a monad in which unification is done

ex:

typeInfer (If e1 e2 e3) = do
    t1 <- typeInfer e1
    t2 <- typeInfer e2
    t3 <- typeInfer e3
    _ <- unify t1 BoolType
    unify t2 t3 -- b/c t2 and t3 have to be the same type

typeInfer (Const (ConstInt _)) = IntType -- same for other literals

--function application
typeInfer (Apply f x) = do
    tf <- typeInfer f
    tx <- typeInfer x
    case tf of
        FunctionType t1 t2 -> do
            _ <- unify t1 tx
            return t2
        _ -> fail "Not a function"

--type annotation
typeInfer (Typed x t) = do
    tx <- typeInfer x
    unify tx t

--variable and let expressions - need to pass around a map of variable names to types here
typeInfer :: [ (Var, Type Var) ] -> Expr Var -> Matching (Type Var)

typeInfer ctx (Var x) = case (lookup x ctx) of
    Just t -> return t
    Nothing -> fail "Unknown variable"

--let x = e1 in e2
typeInfer ctx (Let x e1 e2) = do
    t1 <- typeInfer ctx e1
    typeInfer ((x, t1) :: ctx) e2

--lambdas are complicated (this represents ʎx.e)
typeInfer ctx (Lambda x e) = do
    t1 <- allocExistentialVariable
    t2 <- typeInfer ((x, t1) :: ctx) e
    return $ FunctionType t1 t2 -- ie. t1 -> t2


--to solve the problem of map :: (a -> b) -> [a] -> [b]
when we use a variable whose type has universal tvars, convert those universal
tvars to existential ones
    -and each  distinct universal tvar needs to map to the same existential type

-so we change typeinfer:
typeInfer ctx (Var x) = do
    case (lookup x ctx) of
        Nothing -> ...
        Just t -> do
            let uvars = nub (toList t) -- nub removes duplicates, so this gets unique universally quantified variables
            evars <- mapM (const allocExistentialVariable) uvars
            let varMap = zip uvars evars
            let vixVar varMap v = fromJust $ lookup v varMap
            return (fmap (fixVar varMap) t)

--how do we define unify??

-recall, type signature is:
unify :: Type a -> Type b -> Matching (Type c)
unify BoolType BoolType = BoolType --easy, same for all constants
unify (FunctionType t1 t2) (FunctionType t3 t4) = do
    t5 <- unify t1 t3
    t6 <- unify t2 t4
    return $ FunctionType t5 t6
unify (TVar a) (TVar b) = if a == b then TVar a else fail
--existential types can be assigned another type at most once
--some complicated stuff about hanlding existential types
--everything else is a type error
unify a b = fail


SKOLEMIZATION - how you prevent an unassigned existential type variable from leaking!
-before a type gets to global scope, replace all unassigned existential vars w/  new unique universal
type variables

*/

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
  TVar(TypeVar),
  TConst(TypeConst),
  TFunc(Box<Type>, Box<Type>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeVar {
  Univ(Rc<String>),
  Exist(u64),
}
impl TypeVar {
  fn univ(label: &str) -> TypeVar {
    TypeVar::Univ(Rc::new(label.to_string()))
  }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeConst {
  UserT(Rc<String>),
  Integer,
  Float,
  StringT,
  Boolean,
  Unit,
  Bottom,
}

type TypeCheckResult = Result<Type, String>;

#[derive(Debug, PartialEq, Eq, Hash)]
struct PathSpecifier(Rc<String>);

#[derive(Debug, PartialEq, Clone)]
struct TypeContextEntry {
  ty: Type,
  constant: bool
}

pub struct TypeContext {
  symbol_table: HashMap<PathSpecifier, TypeContextEntry>,
  evar_table: HashMap<u64, Type>,
  existential_type_label_count: u64
}

impl TypeContext {
  pub fn new() -> TypeContext {
    TypeContext {
      symbol_table: HashMap::new(),
      evar_table: HashMap::new(),
      existential_type_label_count: 0,
    }
  }
  pub fn add_symbols(&mut self, ast: &AST) {
    use self::Declaration::*;
    use self::Type::*;
    use self::TypeConst::*;

    for statement in ast.0.iter() {
      match *statement {
        Statement::ExpressionStatement(_) => (),
        Statement::Declaration(ref decl) => match *decl {
          FuncSig(_) => (),
          Impl { .. } => (),
          TypeDecl(ref type_constructor, ref body) => {
            for variant in body.0.iter() {
              let (spec, ty) = match variant {
                &Variant::UnitStruct(ref data_constructor) => {
                  let spec = PathSpecifier(data_constructor.clone());
                  let ty = TConst(UserT(type_constructor.name.clone()));
                  (spec, ty)
                },
                &Variant::TupleStruct(ref data_construcor, ref args) => {
                  //TODO fix
                  let arg = args.get(0).unwrap();
                  let type_arg = self.from_anno(arg);
                  let spec = PathSpecifier(data_construcor.clone());
                  let ty = TFunc(Box::new(type_arg), Box::new(TConst(UserT(type_constructor.name.clone()))));
                  (spec, ty)
                },
                &Variant::Record(_, _) => unimplemented!(),
              };
              let entry = TypeContextEntry { ty, constant: true };
              self.symbol_table.insert(spec, entry);
            }
          },
          TypeAlias { .. } => (),
          Binding {ref name, ref constant, ref expr} => {
            let spec = PathSpecifier(name.clone());
            let ty = expr.1.as_ref()
              .map(|ty| self.from_anno(ty))
              .unwrap_or_else(|| { self.alloc_existential_type() }); // this call to alloc_existential is OK b/c a binding only ever has one type, so if the annotation is absent, it's fine to just make one de novo
            let entry = TypeContextEntry { ty, constant: *constant };
            self.symbol_table.insert(spec, entry);
          },
          FuncDecl(ref signature, _) => {
            let spec = PathSpecifier(signature.name.clone());
            let ty = self.from_signature(signature);
            let entry = TypeContextEntry { ty, constant: true };
            self.symbol_table.insert(spec, entry);
          },
        }
      }
    }
  }
  fn lookup(&mut self, binding: &Rc<String>) -> Option<TypeContextEntry> {
    let key = PathSpecifier(binding.clone());
    self.symbol_table.get(&key).map(|entry| entry.clone())
  }
  pub fn debug_symbol_table(&self) -> String  {
    format!("Symbol table:\n {:?}\nEvar table:\n{:?}", self.symbol_table, self.evar_table)
  }
  fn alloc_existential_type(&mut self) -> Type {
    let ret = Type::TVar(TypeVar::Exist(self.existential_type_label_count));
    self.existential_type_label_count += 1;
    ret
  }

  fn from_anno(&mut self, anno: &TypeName) -> Type {
    use self::Type::*;
    use self::TypeConst::*;

    match anno {
      &TypeName::Singleton(TypeSingletonName { ref name, .. }) => {
        match name.as_ref().as_ref() {
          "Int" => TConst(Integer),
          "Float" => TConst(Float),
          "Bool" => TConst(Boolean),
          "String" => TConst(StringT),
          s => TVar(TypeVar::Univ(Rc::new(format!("{}",s)))),
        }
      },
      &TypeName::Tuple(ref items) => {
        if items.len() == 1 {
          TConst(Unit)
        } else {
          TConst(Bottom)
        }
      }
    }
  }

  fn from_signature(&mut self, sig: &Signature) -> Type {
    use self::Type::*;
    use self::TypeConst::*;

    //TODO this won't work properly until you make sure that all (universal) type vars in the function have the same existential type var
    // actually this should never even put existential types into the symbol table at all

    //this will crash if more than 5 arg function is used
    let names = vec!["a", "b", "c", "d", "e", "f"];
    let mut idx = 0;
    
    let mut get_type = || { let q = TVar(TypeVar::Univ(Rc::new(format!("{}", names.get(idx).unwrap())))); idx += 1; q };

    let return_type = sig.type_anno.as_ref().map(|anno| self.from_anno(&anno)).unwrap_or_else(|| { get_type() });
    if sig.params.len() == 0 {
      TFunc(Box::new(TConst(Unit)), Box::new(return_type))
    } else {
      let mut output_type = return_type;
      for p in sig.params.iter() {
        let p_type = p.1.as_ref().map(|anno| self.from_anno(anno)).unwrap_or_else(|| { get_type() });
        output_type = TFunc(Box::new(p_type), Box::new(output_type));
      }
      output_type
    }
  }

  pub fn type_check(&mut self, ast: &AST) -> TypeCheckResult {
    use self::Type::*;
    use self::TypeConst::*;

    let mut last = TConst(Unit);

    for statement in ast.0.iter() {
      match statement {
        &Statement::Declaration(ref _decl) => {
          //return Err(format!("Declarations not supported"));
        },
        &Statement::ExpressionStatement(ref expr) => {
          last = self.infer(expr)?;
        }
      }
    }
    Ok(last)
  }
  fn infer(&mut self, expr: &Expression) -> TypeCheckResult {
    match (&expr.0, &expr.1) {
      (exprtype, &Some(ref anno)) => {
        let tx = self.infer_no_anno(exprtype)?;
        let ty = self.from_anno(anno);
        self.unify(tx, ty)
      },
      (exprtype, &None) => self.infer_no_anno(exprtype),
    }
  }

  fn infer_no_anno(&mut self, ex: &ExpressionType) -> TypeCheckResult { 
    use self::ExpressionType::*;
    use self::Type::*;
    use self::TypeConst::*;

    Ok(match ex {
      &IntLiteral(_) => TConst(Integer),
      &FloatLiteral(_) => TConst(Float),
      &StringLiteral(_) => TConst(StringT),
      &BoolLiteral(_) => TConst(Boolean),
      &Value(ref name, _) => {
        self.lookup(name)
          .map(|entry| entry.ty)
          .ok_or(format!("Couldn't find {}", name))?
      },
      &BinExp(ref op, ref lhs, ref rhs) => {
        let t_lhs = self.infer(lhs)?;
        match self.infer_op(op)? {
          TFunc(t1, t2) => {
            let _ = self.unify(t_lhs, *t1)?;
            let t_rhs = self.infer(rhs)?;
            let x = *t2;
            match x {
              TFunc(t3, t4) => {
                let _ = self.unify(t_rhs, *t3)?;
                *t4
              },
              _ => return Err(format!("Not a function type either")),
            }
          },
          _ => return Err(format!("Op {:?} is not a function type", op)),
        }
      },
      &Call { ref f, ref arguments } => {
        let tf = self.infer(f)?;
        let targ = self.infer(arguments.get(0).unwrap())?;
        match tf {
          TFunc(box t1, box t2) => {
            let _ = self.unify(t1, targ)?;
            t2
          },
          _ => return Err(format!("Not a function!")),
        }
      },
      _ => TConst(Bottom),
    })
  }

  fn infer_op(&mut self, op: &Operation) -> TypeCheckResult {
    use self::Type::*;
    use self::TypeConst::*;
    macro_rules! binoptype {
      ($lhs:expr, $rhs:expr, $out:expr) => { TFunc(Box::new($lhs), Box::new(TFunc(Box::new($rhs), Box::new($out)))) };
    }

    Ok(match (*op.0).as_ref() {
      "+" => binoptype!(TConst(Integer), TConst(Integer), TConst(Integer)),
      "++" => binoptype!(TConst(StringT), TConst(StringT), TConst(StringT)),
      "-" => binoptype!(TConst(Integer), TConst(Integer), TConst(Integer)),
      "*" => binoptype!(TConst(Integer), TConst(Integer), TConst(Integer)),
      "/" => binoptype!(TConst(Integer), TConst(Integer), TConst(Integer)),
      "%" => binoptype!(TConst(Integer), TConst(Integer), TConst(Integer)),
      _ => TConst(Bottom)
    })
  }

  fn unify(&mut self, t1: Type, t2: Type) -> TypeCheckResult {
    use self::Type::*;
    use self::TypeVar::*;

    println!("Calling unify with `{:?}` and `{:?}`", t1, t2);

    match (&t1, &t2) {
      (&TConst(ref c1), &TConst(ref c2)) if c1 == c2 => Ok(TConst(c1.clone())),
      (&TFunc(ref t1, ref t2), &TFunc(ref t3, ref t4)) => {
        let t5 = self.unify(*t1.clone().clone(), *t3.clone().clone())?;
        let t6 = self.unify(*t2.clone().clone(), *t4.clone().clone())?;
        Ok(TFunc(Box::new(t5), Box::new(t6)))
      },
      (&TVar(Univ(ref a)), &TVar(Univ(ref b))) => {
        if a == b {
          Ok(TVar(Univ(a.clone())))
        } else {
          Err(format!("Couldn't unify universal types {} and {}", a, b))
        }
      },
      //the interesting case!!
      (&TVar(Exist(ref a)), ref t2) => {
        let x = self.evar_table.get(a).map(|x| x.clone());
        match x {
          Some(ref t1) => self.unify(t1.clone().clone(), t2.clone().clone()),
          None => {
            self.evar_table.insert(*a, t2.clone().clone());
            Ok(t2.clone().clone())
          }
        }
      },
      (ref t1, &TVar(Exist(ref a))) => {
        let x = self.evar_table.get(a).map(|x| x.clone());
        match x {
          Some(ref t2) => self.unify(t2.clone().clone(), t1.clone().clone()),
          None => {
            self.evar_table.insert(*a, t1.clone().clone());
            Ok(t1.clone().clone())
          }
        }
      },
      _ => Err(format!("Types {:?} and {:?} don't unify", t1, t2))
    }
  }
}

#[cfg(test)]
mod tests {
  use super::{Type, TypeVar, TypeConst, TypeContext};
  use super::Type::*;
  use super::TypeConst::*;
  use schala_lang::parsing::{parse, tokenize};

  macro_rules! type_test {
    ($input:expr, $correct:expr) => {
      {
      let mut tc = TypeContext::new();
      let ast = parse(tokenize($input)).0.unwrap() ;
      tc.add_symbols(&ast);
      assert_eq!($correct, tc.type_check(&ast).unwrap())
      }
    }
  }

  #[test]
  fn basic_inference() {
    type_test!("30", TConst(Integer));
    type_test!("fn x(a: Int): Bool {}; x(1)", TConst(Boolean));
  }
}
