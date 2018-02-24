use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub struct BinOp {
  pub sigil: Rc<String>
}

#[derive(Debug, PartialEq, Clone)]
pub struct PrefixOp {
  pub sigil: Rc<String>
}

impl BinOp {
  pub fn from_sigil(sigil: Rc<String>) -> BinOp {
    BinOp { sigil }
  }
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

impl PrefixOp {
  pub fn from_sigil(sigil: Rc<String>) -> PrefixOp {
    PrefixOp { sigil }
  }
  pub fn is_prefix(op: &str) -> bool {
    match op {
      "+" | "-" | "!" | "~" => true,
      _ => false,
    }
  }
}
