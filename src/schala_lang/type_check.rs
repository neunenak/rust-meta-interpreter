use std::collections::HashMap;
use std::rc::Rc;

use schala_lang::parsing::{AST, Statement, Declaration, Signature, Expression, ExpressionType, Operation, TypeName};

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
  Univ(String),
  Exist(u64),
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeConst {
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

    for statement in ast.0.iter() {
      match *statement {
        Statement::ExpressionStatement(_) => (),
        Statement::Declaration(ref decl) => {
          match *decl {
            FuncSig(_) => (),
            Impl { .. } => (),
            TypeDecl { .. } => (),
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
      (ref _t, &Some(ref anno)) => {
        self.from_anno(anno)// TODO make this better,
      },
      (&IntLiteral(_), _) => TConst(Integer),
      (&FloatLiteral(_), _) => TConst(Float),
      (&StringLiteral(_), _) => TConst(StringT),
      (&BoolLiteral(_), _) => TConst(Boolean),
      (&Variable(ref name), _) => {
        self.lookup(name)
          .map(|entry| entry.type_var)
          .ok_or(format!("Couldn't find {}", name))?
      },
      (&BinExp(ref op, box ref lhs, box ref rhs), _) => {
        let _f_type = self.infer_op(op);
        let _lhs_type = self.infer(&lhs);
        let _rhs_type = self.infer(&rhs);
        unimplemented!()
      },
      (&Call { ref f, ref arguments }, _) => {
        let f_type = self.infer(&*f)?;
        let arg_type = self.infer(arguments.get(0).unwrap())?; // TODO fix later
        match f_type {
          TConst(FunctionT(box t1, box ret_type)) => {
            let _ = self.unify(&t1, &arg_type)?;
            ret_type
          },
          _ => return Err(format!("Type error"))
        }
      },
      _ => TConst(Unit)
    })
  }

  fn infer_op(&mut self, _op: &Operation) -> TypeCheckResult {
    use self::Type::*;
    use self::TypeConst::*;

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

  fn unify(&mut self, t1: &Type, t2: &Type) -> TypeCheckResult {
    if t1 == t2 {
      Ok(t1.clone())
    } else {
      Err(format!("Types {:?} and {:?} don't unify", t1, t2))
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
  }
}

