use std::rc::Rc;
use std::collections::HashMap;
use std::fmt;

use tokenizing::TokenType;
use self::BuiltinTypeSpecifier::*;
use self::BuiltinTConst::*;


#[derive(Debug, PartialEq, Clone)]
pub enum BuiltinTypeSpecifier {
  Const(BuiltinTConst),
  Func(Box<BuiltinTypeSpecifier>, Box<BuiltinTypeSpecifier>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum BuiltinTConst {
  Nat,
  Int,
  Float,
  StringT,
  Bool,
}

impl fmt::Display for BuiltinTypeSpecifier {
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
  pub fn from_sigil_token(tok: &TokenType) -> Option<BinOp> {
    use self::TokenType::*;
    let s = match tok {
      Operator(op) => op,
      Period => ".",
      Pipe => "|",
      Slash => "/",
      LAngleBracket => "<",
      RAngleBracket => ">",
      _ => return None
    };
    Some(BinOp::from_sigil(s))
  }
  /*
  pub fn get_type(&self) -> Result<Type, String> {
    let s = self.sigil.as_str();
    BINOPS.get(s).map(|x| x.0.clone()).ok_or(format!("Binop {} not found", s))
  }
  */
  pub fn min_precedence() -> i32 {
    i32::min_value()
  }
  pub fn get_precedence_from_token(op: &TokenType) -> Option<i32> {
    use self::TokenType::*;
    let s = match op {
      Operator(op) => op,
      Period => ".",
      Pipe => "|",
      Slash => "/",
      LAngleBracket => "<",
      RAngleBracket => ">",
      _ => return None
    };
    let default = 10_000_000;
    Some(BINOPS.get(s).map(|x| x.2.clone()).unwrap_or_else(|| {
      println!("Warning: operator {} not defined", s);
      default
    }))
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
  /*
  pub fn get_type(&self) -> Result<Type, String> {
    let s = self.sigil.as_str();
    PREFIX_OPS.get(s).map(|x| x.0.clone()).ok_or(format!("Prefix op {} not found", s))
  }
  */
}
lazy_static! {
  static ref PREFIX_OPS: HashMap<&'static str, (BuiltinTypeSpecifier, ())> = 
    hashmap! {
      "+" => (Func(bx!(Const(Int)),  bx!(Const(Int))), ()),
      "-" => (Func(bx!(Const(Int)),  bx!(Const(Int))), ()),
      "!" => (Func(bx!(Const(Bool)),  bx!(Const(Bool))), ()),
    };
}

/* the second tuple member is a placeholder for when I want to make evaluation rules tied to the
 * binop definition */
lazy_static! {
  static ref BINOPS: HashMap<&'static str, (BuiltinTypeSpecifier, (), i32)> =
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
      ">" => (Func(bx!(Const(Nat)), bx!(Func(bx!(Const(Nat)), bx!(Const(Nat))))), (), 20),
      ">=" => (Func(bx!(Const(Nat)), bx!(Func(bx!(Const(Nat)), bx!(Const(Nat))))), (), 20),
      "<" => (Func(bx!(Const(Nat)), bx!(Func(bx!(Const(Nat)), bx!(Const(Nat))))), (), 20),
      "<=" => (Func(bx!(Const(Nat)), bx!(Func(bx!(Const(Nat)), bx!(Const(Nat))))), (), 20),
      "==" => (Func(bx!(Const(Nat)), bx!(Func(bx!(Const(Nat)), bx!(Const(Nat))))), (), 20),
      "=" => (Func(bx!(Const(Nat)), bx!(Func(bx!(Const(Nat)), bx!(Const(Nat))))), (), 20),
    };
}
