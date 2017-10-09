use std::collections::HashMap;
use std::rc::Rc;

use schala_lang::parsing::{AST, Statement, Declaration, Signature, Expression, ExpressionType, Operation, TypeName};

#[derive(Debug, PartialEq, Eq, Hash)]
struct PathSpecifier(Rc<String>);

#[derive(Debug, PartialEq, Clone)]
struct TypeContextEntry {
  type_var: TypeVariable,
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
  fn get_existential_type(&mut self) -> TypeVariable {
    let ret = TypeVariable::Exist(self.existential_type_label_count);
    self.existential_type_label_count += 1;
    ret
  }

  fn from_anno(&mut self, anno: &TypeName) -> TypeVariable {
    use self::TypeVariable::*;
    use self::UVar::*;

    match anno {
      &TypeName::Singleton { ref name, .. } => {
        match name.as_ref().as_ref() {
          "Int" => Univ(Integer),
          "Bool" => Univ(Boolean),
          _ => self.get_existential_type()
        }
      },
      _ => Univ(Bottom),
    }
  }

  fn from_signature(&mut self, sig: &Signature) -> TypeVariable {
    use self::TypeVariable::Univ;
    use self::UVar::{Unit, Function};
    let return_type = sig.type_anno.as_ref().map(|anno| self.from_anno(&anno)).unwrap_or_else(|| { self.get_existential_type() });
    if sig.params.len() == 0 {
      Univ(Function(Box::new(Univ(Unit)), Box::new(return_type)))
    } else {
      let mut output_type = return_type;
      for p in sig.params.iter() {
        let p_type = p.1.as_ref().map(|anno| self.from_anno(anno)).unwrap_or_else(|| { self.get_existential_type() });
        output_type = Univ(Function(Box::new(p_type), Box::new(output_type)));
      }
      output_type
    }
  }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeVariable {
  Univ(UVar),
  Exist(u64),
}

#[derive(Debug, PartialEq, Clone)]
pub enum UVar {
  Integer,
  Float,
  String,
  Boolean,
  Unit,
  Function(Box<TypeVariable>, Box<TypeVariable>),
  Bottom,
}

type TypeCheckResult = Result<TypeVariable, String>;

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

impl TypeContext {
  pub fn type_check(&mut self, ast: &AST) -> TypeCheckResult {
    let mut last = TypeVariable::Univ(UVar::Unit);
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
    use self::TypeVariable::*;

    Ok(match (&expr.0, &expr.1) {
      (ref _t, &Some(ref anno)) => {
        //TODO make this better,
        self.from_anno(anno)
      },
      (&IntLiteral(_), _) => Univ(UVar::Integer),
      (&FloatLiteral(_), _) => Univ(UVar::Float),
      (&StringLiteral(_), _) => Univ(UVar::String),
      (&BoolLiteral(_), _) => Univ(UVar::Boolean),
      (&Variable(ref name), _) => self.lookup(name).map(|entry| entry.type_var)
        .ok_or(format!("Couldn't find {}", name))?,
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
          Univ(UVar::Function(box t1, box ret_type)) => {
            let _ = self.unify(&t1, &arg_type)?;
            ret_type
          },
          _ => return Err(format!("Type error"))
        }
      },
      _ => Univ(UVar::Unit),
    })
  }

  fn infer_op(&mut self, _op: &Operation) -> TypeCheckResult {
    use self::TypeVariable::*;
    Ok(
      Univ(UVar::Function(
        Box::new(Univ(UVar::Integer)),
        Box::new(Univ(UVar::Function(
          Box::new(Univ(UVar::Integer)),
          Box::new(Univ(UVar::Integer))
          )))
        ))
      )
  }

  fn unify(&mut self, t1: &TypeVariable, t2: &TypeVariable) -> TypeCheckResult {
    if t1 == t2 {
      Ok(t1.clone())
    } else {
      Err(format!("Types {:?} and {:?} don't unify", t1, t2))
    }
  }
}

#[cfg(test)]
mod tests {
  use super::{TypeContext, TypeVariable, UVar};
  use super::TypeVariable::*;
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
    type_test!("30", Univ(UVar::Integer))
  }
}

