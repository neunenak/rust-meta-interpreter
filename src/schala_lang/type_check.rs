use std::collections::HashMap;
use std::rc::Rc;

use schala_lang::parsing::{AST, Statement, Declaration, Expression, ExpressionType, Operation, TypeName};

#[derive(Debug, PartialEq, Eq, Hash)]
struct PathSpecifier {
  name: Rc<String>,
  kind: &'static str,
  constant: bool,
}

struct SymbolTable {
  map: HashMap<PathSpecifier, Expression>,
}

impl SymbolTable {
  fn new() -> SymbolTable {
    SymbolTable { map: HashMap::new() }
  }

  fn add_symbols(&mut self, ast: &AST) {
    use self::Declaration::*;

    for statement in ast.0.iter() {
      match statement {
        &Statement::ExpressionStatement(_) => (),
        &Statement::Declaration(ref d) => {
          match d {
            &FuncSig(_) => (),
            &FuncDecl(_, _) => (),
            &TypeDecl { .. } => (),
            &TypeAlias { .. } => (),
            &Binding {ref name, ref constant, ref expr} => {
              let spec = PathSpecifier {
                name: name.clone(),
                kind: "binding",
                constant: *constant
              };
              let binding_contents = (*expr).clone();
              self.map.insert(spec, binding_contents);
            },
            &Impl { .. } => (),
          }
        }
      }
    }
  }
  fn lookup(&mut self, binding: &Rc<String>) -> Option<SchalaType> {
    use self::SchalaType::*;
    Some(Function(Box::new(Integer), Box::new(Boolean)))
  }
}

pub struct TypeContext {
  symbol_table: SymbolTable,
}

impl TypeContext {
  pub fn new() -> TypeContext {
    TypeContext { symbol_table: SymbolTable::new() }
  }
  pub fn add_symbols(&mut self, ast: &AST) {
    self.symbol_table.add_symbols(ast)
  }
  pub fn debug_symbol_table(&self) -> String  {
    format!("Symbol table:\n {:?}", self.symbol_table.map)
  }
}

#[derive(Debug, PartialEq, Clone)]
pub enum SchalaType {
  Integer,
  Boolean,
  Unit,
  Function(Box<SchalaType>, Box<SchalaType>),
  Bottom,
}

impl SchalaType {
  fn from_anno(anno: &TypeName) -> SchalaType {
    use self::SchalaType::*;

    match anno {
      &TypeName::Singleton { ref name, .. } => {
        match name.as_ref().as_ref() {
          "Int" => Integer,
          "Bool" => Boolean,
          _ => Bottom,
        }
      },
      _ => Bottom,
    }
  }
}

type TypeCheckResult = Result<SchalaType, String>;

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
    let mut last = SchalaType::Unit;
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

    Ok(match (&expr.0, &expr.1) {
      (ref _t, &Some(ref anno)) => {
        //TODO make this better,
        SchalaType::from_anno(anno)
      },
      (&IntLiteral(_), _) => SchalaType::Integer,
      (&BoolLiteral(_), _) => SchalaType::Boolean,
      (&Variable(ref name), _) => self.symbol_table
        .lookup(name)
        .ok_or(format!("Couldn't find {}", name))?,

      (&Call { ref f, ref arguments }, _) => {
        let f_type = self.infer(&*f)?;
        let arg_type = self.infer(arguments.get(0).unwrap())?; // TODO fix later
        match f_type {
          SchalaType::Function(box t1, box ret_type) => {
            let _ = self.unify(&t1, &arg_type)?;
            ret_type
          },
          _ => return Err(format!("Type error"))
        }
      },
      _ => SchalaType::Unit,
    })
  }

  fn unify(&mut self, t1: &SchalaType, t2: &SchalaType) -> TypeCheckResult {
    if t1 == t2 {
      Ok(t1.clone())
    } else {
      Err(format!("Types {:?} and {:?} don't unify", t1, t2))
    }
  }
}

