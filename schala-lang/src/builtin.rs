use std::rc::Rc;
use std::collections::HashMap;
use std::fmt;

use self::Type::*; use self::TConstOld::*;


//TODO get rid of these types and replace them with the right MonoType or whatever ones later
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

#[derive(Debug, PartialEq, Clone)]
pub struct BinOp {
  sigil: Rc<String>
}

impl BinOp {
  pub fn from_sigil(sigil: &str) -> BinOp {
    BinOp { sigil: Rc::new(sigil.to_string()) }
  }
  pub fn sigil(&self) -> &Rc<String> {
    &self.sigil
  }
  pub fn get_type(&self) -> Result<Type, String> {
    let s = self.sigil.as_str();
    BINOPS.get(s).map(|x| x.0.clone()).ok_or(format!("Binop {} not found", s))
  }
  pub fn min_precedence() -> i32 {
    i32::min_value()
  }
  pub fn get_precedence(op: &str) -> i32 {
    let default = 10_000_000;
    BINOPS.get(op).map(|x| x.2.clone()).unwrap_or(default)
  }
}

#[derive(Debug, PartialEq, Clone)]
pub struct PrefixOp {
  sigil: Rc<String>
}

impl PrefixOp {
  pub fn from_sigil(sigil: &str) -> PrefixOp {
    PrefixOp { sigil: Rc::new(sigil.to_string()) }
  }
  pub fn sigil(&self) -> &Rc<String> {
    &self.sigil
  }
  pub fn is_prefix(op: &str) -> bool {
    PREFIX_OPS.get(op).is_some()
  }
  pub fn get_type(&self) -> Result<Type, String> {
    let s = self.sigil.as_str();
    PREFIX_OPS.get(s).map(|x| x.0.clone()).ok_or(format!("Prefix op {} not found", s))
  }
}
lazy_static! {
  static ref PREFIX_OPS: HashMap<&'static str, (Type, ())> = 
    hashmap! {
      "+" => (Func(bx!(Const(Int)),  bx!(Const(Int))), ()),
      "-" => (Func(bx!(Const(Int)),  bx!(Const(Int))), ()),
      "!" => (Func(bx!(Const(Bool)),  bx!(Const(Bool))), ()),
    };
}

/* the second tuple member is a placeholder for when I want to make evaluation rules tied to the
 * binop definition */
lazy_static! {
  static ref BINOPS: HashMap<&'static str, (Type, (), i32)> =
    hashmap! {
      "+" => (Func(bx!(Const(Nat)), bx!(Func(bx!(Const(Nat)), bx!(Const(Nat))))), (), 10),
      "-" => (Func(bx!(Const(Nat)), bx!(Func(bx!(Const(Nat)), bx!(Const(Nat))))), (), 10),
      "*" => (Func(bx!(Const(Nat)), bx!(Func(bx!(Const(Nat)), bx!(Const(Nat))))), (), 20),
      "/" => (Func(bx!(Const(Nat)), bx!(Func(bx!(Const(Nat)), bx!(Const(Float))))), (), 20),
      "//" => (Func(bx!(Const(Nat)), bx!(Func(bx!(Const(Nat)), bx!(Const(Nat))))), (), 20), //TODO change this to `quot`
      "%" => (Func(bx!(Const(Nat)), bx!(Func(bx!(Const(Nat)), bx!(Const(Nat))))), (), 20),
      "++" => (Func(bx!(Const(StringT)), bx!(Func(bx!(Const(StringT)), bx!(Const(StringT))))), (), 30),
      "^" => (Func(bx!(Const(Nat)), bx!(Func(bx!(Const(Nat)), bx!(Const(Nat))))), (), 20),
      "&" => (Func(bx!(Const(Nat)), bx!(Func(bx!(Const(Nat)), bx!(Const(Nat))))), (), 20),
      "|" => (Func(bx!(Const(Nat)), bx!(Func(bx!(Const(Nat)), bx!(Const(Nat))))), (), 20),
    };
}
