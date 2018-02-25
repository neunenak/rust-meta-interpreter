use std::rc::Rc;
use std::collections::HashMap;

use schala_lang::typechecking::{Type, TypeResult, TConst};
use self::Type::*; use self::TConst::*;

#[derive(Debug, PartialEq, Clone)]
pub struct BinOp {
  sigil: Rc<String>
}

impl BinOp {
  pub fn from_sigil(sigil: Rc<String>) -> BinOp {
    BinOp { sigil }
  }
  pub fn sigil(&self) -> &Rc<String> {
    &self.sigil
  }
  pub fn get_type(&self) -> TypeResult<Type> {
    let s = self.sigil.as_str();
    BINOPS.get(s).map(|x| x.0.clone()).ok_or(format!("Binop {} not found", s))
  }
}

impl BinOp {
  pub fn min_precedence() -> i32 {
    i32::min_value()
  }
  pub fn get_precedence(op: &str) -> i32 {
    match op {
      "+" | "-" => 10,
      "*" | "/" | "%" =>  20,
      _ => 30,
    }
  }
}

#[derive(Debug, PartialEq, Clone)]
pub struct PrefixOp {
  sigil: Rc<String>
}

impl PrefixOp {
  pub fn from_sigil(sigil: Rc<String>) -> PrefixOp {
    PrefixOp { sigil }
  }
  pub fn sigil(&self) -> &Rc<String> {
    &self.sigil
  }
  pub fn is_prefix(op: &str) -> bool {
    match op {
      "+" | "-" | "!" | "~" => true,
      _ => false,
    }
  }
}


/* the second tuple member is a placeholder for when I want to make evaluation rules tied to the
 * binop definition */
lazy_static! {
  static ref BINOPS: HashMap<&'static str, (Type, (), i32)> =
    hashmap! {
      "+" => (Func(bx!(Const(Int)), bx!(Func(bx!(Const(Int)), bx!(Const(Int))))), (), 10),
      "-" => (Func(bx!(Const(Int)), bx!(Func(bx!(Const(Int)), bx!(Const(Int))))), (), 10),
      "*" => (Func(bx!(Const(Int)), bx!(Func(bx!(Const(Int)), bx!(Const(Int))))), (), 20),
      "/" => (Func(bx!(Const(Int)), bx!(Func(bx!(Const(Int)), bx!(Const(Int))))), (), 20),
      "%" => (Func(bx!(Const(Int)), bx!(Func(bx!(Const(Int)), bx!(Const(Int))))), (), 20),
      "++" => (Func(bx!(Const(StringT)), bx!(Func(bx!(Const(StringT)), bx!(Const(StringT))))), (), 30),
    };
}
