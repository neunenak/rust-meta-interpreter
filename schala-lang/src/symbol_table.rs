use std::collections::HashMap;
use std::rc::Rc;
use std::fmt;
use std::fmt::Write;

use ast;
use typechecking::TypeName;

//cf. p. 150 or so of Language Implementation Patterns
pub struct SymbolTable {
  pub values: HashMap<Rc<String>, Symbol> //TODO this will eventually have real type information
}

//TODO add various types of lookups here, maybe multiple hash tables internally? also make values
//non-public
impl SymbolTable {
  pub fn new() -> SymbolTable {
    SymbolTable { values: HashMap::new() }
  }

  pub fn lookup_by_name(&self, name: &Rc<String>) -> Option<&Symbol> {
    self.values.get(name)
  }
}

#[derive(Debug)]
pub struct Symbol {
  pub name: Rc<String>,
  pub spec: SymbolSpec,
}

impl fmt::Display for Symbol {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "<Name: {}, Spec: {}>", self.name, self.spec)
  }
}

#[derive(Debug)]
pub enum SymbolSpec {
  Func(Vec<TypeName>),
  DataConstructor {
    index: usize,
    type_name: Rc<String>,
    type_args: Vec<Rc<String>>,
  },
}

impl fmt::Display for SymbolSpec {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    use self::SymbolSpec::*;
    match self {
      Func(type_names) => write!(f, "Func({:?})", type_names),
      DataConstructor { index, type_name, type_args } => write!(f, "DataConstructor({})({:?} -> {})", index, type_args, type_name),
    }
  }
}

impl SymbolTable {
  /* note: this adds names for *forward reference* but doesn't actually create any types. solve that problem
   * later */
  pub fn add_top_level_symbols(&mut self, ast: &ast::AST) -> Result<(), String> {
    use self::ast::{Statement, TypeName, Variant, TypeSingletonName, TypeBody};
    use self::ast::Declaration::*;
    for statement in ast.0.iter() {
      if let Statement::Declaration(decl) = statement {
        match decl {
          FuncSig(signature) | FuncDecl(signature, _) => {
            let mut ch: char = 'a';
            let mut types = vec![];
            for param in signature.params.iter() {
              match param {
                (_, Some(_ty)) => {
                  //TODO eventually handle this case different
                  types.push(Rc::new(format!("{}", ch)));
                  ch = ((ch as u8) + 1) as char;
                },
                (_, None) => {
                  types.push(Rc::new(format!("{}", ch)));
                  ch = ((ch as u8) + 1) as char;
                }
              }
            }
            let spec = SymbolSpec::Func(types);
            self.values.insert(
              signature.name.clone(),
              Symbol { name: signature.name.clone(), spec }
              );
          },
          //TODO figure out why _params isn't being used here
          TypeDecl { name: TypeSingletonName { name, params: _params}, body: TypeBody(variants), mutable: _mutable, } => {
            for (index, var) in variants.iter().enumerate() {
              match var {
                Variant::UnitStruct(variant_name) => {
                  let spec = SymbolSpec::DataConstructor {
                    index,
                    type_name: name.clone(),
                    type_args: vec![],
                  };
                  self.values.insert(variant_name.clone(), Symbol { name: variant_name.clone(), spec });
                },
                Variant::TupleStruct(variant_name, tuple_members) => {
                  let type_args = tuple_members.iter().map(|type_name| match type_name {
                    TypeName::Singleton(TypeSingletonName { name, ..}) => name.clone(),
                    TypeName::Tuple(_) => unimplemented!(),
                  }).collect();
                  let spec = SymbolSpec::DataConstructor {
                    index,
                    type_name: name.clone(),
                    type_args
                  };
                  let symbol = Symbol { name: variant_name.clone(), spec };
                  self.values.insert(variant_name.clone(), symbol);
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
    for (name, sym) in &self.values {
      write!(output, "{} -> {}\n", name, sym).unwrap();
    }
    output
  }
}
