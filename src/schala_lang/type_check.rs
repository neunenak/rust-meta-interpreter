use std::collections::HashMap;
use std::rc::Rc;

use schala_lang::parsing::{AST, Statement, Declaration, Signature, Expression, ExpressionType, Operation, Variant, TypeName};

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

// from https://www.youtube.com/watch?v=il3gD7XMdmA
// typeInfer :: Expr a -> Matching (Type a)
// unify :: Type a -> Type b -> Matching (Type c)

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
  TVar(TypeVar),
  TConst(TypeConst),
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
  FunctionT(Box<Type>, Box<Type>),
  Bottom,
}

type TypeCheckResult = Result<Type, String>;

#[derive(Debug, PartialEq, Eq, Hash)]
struct PathSpecifier(Rc<String>);

#[derive(Debug, PartialEq, Clone)]
struct TypeContextEntry {
  type_var: Type,
  constant: bool
}

pub struct TypeContext {
  symbol_table: HashMap<PathSpecifier, TypeContextEntry>,
  existential_type_label_count: u64
}

impl TypeContext {
  pub fn new() -> TypeContext {
    TypeContext {
      symbol_table: HashMap::new(),
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
        Statement::Declaration(ref decl) => {
          match *decl {
            FuncSig(_) => (),
            Impl { .. } => (),
            TypeDecl(ref type_constructor, ref body) => {
              for variant in body.0.iter() {
                let (spec, type_var) = match variant {
                  &Variant::UnitStruct(ref data_constructor) => {
                    let spec = PathSpecifier(data_constructor.clone());
                    let type_var = TConst(UserT(type_constructor.clone()));
                    (spec, type_var)
                  },
                  &Variant::TupleStruct(ref data_construcor, ref args) => {
                    //TODO fix
                    let arg = args.get(0).unwrap();
                    let type_arg = self.from_anno(arg);
                    let spec = PathSpecifier(data_construcor.clone());
                    let type_var = TConst(FunctionT(
                      Box::new(type_arg),
                      Box::new(TConst(UserT(type_constructor.clone()))),
                      ));
                    (spec, type_var)

                  },
                  &Variant::Record(_, _) => unimplemented!(),
                };
                let entry = TypeContextEntry { type_var, constant: true };
                self.symbol_table.insert(spec, entry);
              }
            },
            TypeAlias { .. } => (),
            Binding {ref name, ref constant, ref expr} => {
              let spec = PathSpecifier(name.clone());
              let type_var = expr.1.as_ref()
                .map(|ty| self.from_anno(ty))
                .unwrap_or_else(|| { self.get_existential_type() });
              let entry = TypeContextEntry { type_var, constant: *constant };
              self.symbol_table.insert(spec, entry);
            },
            FuncDecl(ref signature, _) => {
              let spec = PathSpecifier(signature.name.clone());
              let type_var = self.from_signature(signature);
              let entry = TypeContextEntry { type_var, constant: true };
              self.symbol_table.insert(spec, entry);
            },
          }
        }
      }
    }
  }
  fn lookup(&mut self, binding: &Rc<String>) -> Option<TypeContextEntry> {
    let key = PathSpecifier(binding.clone());
    self.symbol_table.get(&key).map(|entry| entry.clone())
  }
  pub fn debug_symbol_table(&self) -> String  {
    format!("Symbol table:\n {:?}", self.symbol_table)
  }
  fn get_existential_type(&mut self) -> Type {
    let ret = Type::TVar(TypeVar::Exist(self.existential_type_label_count));
    self.existential_type_label_count += 1;
    ret
  }

  fn from_anno(&mut self, anno: &TypeName) -> Type {
    use self::Type::*;
    use self::TypeConst::*;

    match anno {
      &TypeName::Singleton { ref name, .. } => {
        match name.as_ref().as_ref() {
          "Int" => TConst(Integer),
          "Bool" => TConst(Boolean),
          _ => self.get_existential_type()
        }
      },
      _ => TConst(Bottom)
    }
  }

  fn from_signature(&mut self, sig: &Signature) -> Type {
    use self::Type::*;
    use self::TypeConst::*;

    let return_type = sig.type_anno.as_ref().map(|anno| self.from_anno(&anno)).unwrap_or_else(|| { self.get_existential_type() });
    if sig.params.len() == 0 {
      TConst(FunctionT(Box::new(TConst(Unit)), Box::new(return_type)))
    } else {
      let mut output_type = return_type;
      for p in sig.params.iter() {
        let p_type = p.1.as_ref().map(|anno| self.from_anno(anno)).unwrap_or_else(|| { self.get_existential_type() });
        output_type = TConst(FunctionT(Box::new(p_type), Box::new(output_type)));
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
    use self::ExpressionType::*;
    use self::Type::*;
    use self::TypeConst::*;

    Ok(match (&expr.0, &expr.1) {
      (&IntLiteral(_), anno) => {
        match *anno {
          None => TConst(Integer),
          Some(ref t) => self.from_anno(t)
        }
      }
      (&FloatLiteral(_), anno) => {
        match *anno {
          None => TConst(Float),
          Some(ref t) => self.from_anno(t),
        }
      },
      (&StringLiteral(_), anno) => {
        match *anno {
          None => TConst(StringT),
          Some(ref t) => self.from_anno(t),
        }
      },
      (&BoolLiteral(_), anno) => {
        match *anno {
          None => TConst(Boolean),
          Some(ref t) => self.from_anno(t),
        }
      },
      (&Value(ref name), ref _anno) => {
        self.lookup(name)
          .map(|entry| entry.type_var)
          .ok_or(format!("Couldn't find {}", name))?
      },
      (&BinExp(ref op, box ref lhs, box ref rhs), ref _anno) => {
        let op_type = self.infer_op(op)?;
        let lhs_type = self.infer(&lhs)?;

        match op_type {
          TConst(FunctionT(box t1, box t2)) => {
            let _ = self.unify(t1, lhs_type)?;
            let rhs_type = self.infer(&rhs)?;
            match t2 {
              TConst(FunctionT(box t3, box t_ret)) => {
                let _ = self.unify(t3, rhs_type)?;
                t_ret
              },
              _ => return Err(format!("Another bad type for operator"))
            }
          },
          _ => return Err(format!("Bad type for operator")),
        }
      },
      (&Call { ref f, ref arguments }, ref _anno) => {
        let f_type = self.infer(&*f)?;
        let arg_type = self.infer(arguments.get(0).unwrap())?; // TODO fix later
        match f_type {
          TConst(FunctionT(box t1, box ret_type)) => {
            let _ = self.unify(t1, arg_type)?;
            ret_type
          },
          _ => return Err(format!("Type error"))
        }
      },
      _ => TConst(Unit)
    })
  }

  fn infer_op(&mut self, op: &Operation) -> TypeCheckResult {
    use self::Type::*;
    use self::TypeConst::*;

    let opstr: &str = &op.0;
    if opstr == "+" {
      return Ok(
        TConst(FunctionT(
            Box::new(TVar(TypeVar::univ("a"))),
            Box::new(TConst(FunctionT(
                  Box::new(TVar(TypeVar::univ("a"))),
                  Box::new(TVar(TypeVar::univ("a")))
                  )))
            ))
        )
    }

    Ok(
      TConst(FunctionT(
        Box::new(TConst(Integer)),
        Box::new(TConst(FunctionT(
          Box::new(TConst(Integer)),
          Box::new(TConst(Integer))
          )))
        ))
      )
  }

  fn unify(&mut self, t1: Type, t2: Type) -> TypeCheckResult {
    use self::Type::*;
    use self::TypeConst::*;

    match (&t1, &t2) {
      (&TConst(ref c1), &TConst(ref c2)) if c1 == c2 => Ok(TConst(c1.clone())),
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
      assert_eq!($correct, tc.type_check(&ast).unwrap())
      }
    }
  }

  #[test]
  fn basic_inference() {
    type_test!("30", TConst(Integer));
    type_test!("1 + 2", TConst(Integer));
  }
}
