use std::collections::HashMap;
use std::rc::Rc;
use std::fmt;
use std::fmt::Write;

use parsing;

//cf. p. 150 or so of Language Implementation Patterns
pub struct SymbolTable {
  pub values: HashMap<Rc<String>, Symbol> //TODO this will eventually have real type information
}

impl SymbolTable {
  pub fn new() -> SymbolTable {
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

impl SymbolTable {
  /* note: this adds names for *forward reference* but doesn't actually create any types. solve that problem
   * later */
  pub fn add_top_level_symbols(&mut self, ast: &parsing::AST) -> Result<(), String> {
    use self::parsing::{Statement, TypeName, Variant, TypeSingletonName, TypeBody};
    use self::parsing::Declaration::*;
    for statement in ast.0.iter() {
      if let Statement::Declaration(decl) = statement {
        match decl {
          FuncSig(signature) | FuncDecl(signature, _) => {
            self.values.insert(
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
                  self.values.insert(variant_name.clone(), Symbol { name: variant_name.clone(), spec });
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
    for (sym, ty) in &self.values {
      write!(output, "{} -> {:?}\n", sym, ty).unwrap();
    }
    output
  }
}
