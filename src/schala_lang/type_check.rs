use std::collections::HashMap;

use schala_lang::parsing::{AST, Statement, Declaration, Expression, ExpressionType, Operation, TypeAnno};

#[derive(Debug, PartialEq, Eq, Hash)]
struct PathSpecifier(String);

struct SymbolTable {
  map: HashMap<PathSpecifier, Expression>,
}

impl SymbolTable {
  fn new() -> SymbolTable {
    SymbolTable { map: HashMap::new() }
  }

  fn add_symbols(&mut self, ast: &AST) {
  }
}

pub struct TypeContext {
  symbol_table: SymbolTable,
}

impl TypeContext {
  pub fn new() -> TypeContext {
    TypeContext { symbol_table: SymbolTable::new() }
  }
}

pub struct SchalaType {
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
impl TypeContext {
  pub fn type_check(&mut self, ast: &AST) -> TypeCheckResult {
    use self::ExpressionType::*;

    self.symbol_table.add_symbols(ast);

    for statement in ast.0.iter() {
      match statement {
        &Statement::Declaration(ref _decl) => {
          return Err(format!("Declarations not supported"));
        },
        &Statement::ExpressionStatement(ref expr) => {
          self.expr_type_check(expr)?;
        }
      }
    }
    Ok(SchalaType { })
  }

  fn expr_type_check(&mut self, expr: &Expression) -> TypeCheckResult {
    use self::ExpressionType::*;

    match (&expr.0, &expr.1) {
      (&IntLiteral(_), &Some(ref t)) => {
        match t {
          &TypeAnno::Singleton { ref name, ref params } if **name == "Int" && params.len() == 0 => (),
          t => return Err(format!("Bad type {:?} for int literal", t)),
        }
      },
      _ => (),
    }
    Ok(SchalaType { })
  }
}
