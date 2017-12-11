use itertools::Itertools;
use schala_lib::{ProgrammingLanguageInterface, EvalOptions, ReplOutput};
use std::iter::Peekable;
use std::vec::IntoIter;
use std::str::Chars;
use std::collections::HashMap;

pub struct EvaluatorState {
  vars: HashMap<String, Sexp>
}

impl EvaluatorState {
  fn new() -> EvaluatorState {
    EvaluatorState {
      vars: HashMap::new(),
    }
  }
  fn set_var(&mut self, var: String, value: Sexp) {
    self.vars.insert(var, value);
  }
  fn get_var(&self, var: &str) -> Option<&Sexp> {
    self.vars.get(var)
  }
  fn push_env(&mut self) {

  }
  fn pop_env(&mut self) {

  }
}

pub struct Rukka {
  state: EvaluatorState
}

impl Rukka {
  pub fn new() -> Rukka { Rukka { state: EvaluatorState::new() } }
}

impl ProgrammingLanguageInterface for Rukka {
  fn get_language_name(&self) -> String {
    "Rukka".to_string()
  }

  fn get_source_file_suffix(&self) -> String {
    format!("rukka")
  }

  fn evaluate_in_repl(&mut self, input: &str, _eval_options: &EvalOptions) -> ReplOutput {
    let mut output = ReplOutput::default();
    let sexps = match read(input) {
      Err(err) => {
        output.add_output(format!("Error: {}", err));
        return output;
      },
      Ok(sexps) => sexps
    };

    let output_str: String = sexps.into_iter().enumerate().map(|(i, sexp)| {
      match self.state.eval(sexp) {
        Ok(result) => format!("{}: {}", i, result.print()),
        Err(err) => format!("{} Error: {}", i, err),
      }
    }).intersperse(format!("\n")).collect();
    output.add_output(output_str);
    output
  }
}

impl EvaluatorState {
  fn eval(&mut self, expr: Sexp) -> Result<Sexp, String> {
    use self::Sexp::*;
    println!("Evaling {:?}", expr);
    Ok(match expr {
      SymbolAtom(ref sym) => match self.get_var(sym) {
        Some(ref sexp) => {
          let q: &Sexp = sexp; //WTF? if I delete this line, the copy doesn't work??
          q.clone() //TODO make this not involve a clone
        },
        None => return Err(format!("Variable {} not bound", sym)),
      },
      expr @ FnLiteral { .. } => expr,
      expr @ StringAtom(_) => expr,
      expr @ NumberAtom(_) => expr,
      True => True,
      False => False,
      Cons(box operator, box operands) => match operator {
        SymbolAtom(ref sym) if match &sym[..] {
          "quote" | "eq?" | "cons" | "car" | "cdr" | "atom?" | "define" | "lambda" | "if" | "cond" => true, _ => false
          } => self.eval_special_form(sym, operands)?,
        _ => {
          let evaled = self.eval(operator)?;
          self.apply(evaled, operands)?
        }
      },
      Nil => Nil,
    })
  }
  fn eval_special_form(&mut self, form: &str, operands: Sexp) -> Result<Sexp, String> {
    use self::Sexp::*;
    Ok(match form {
      "quote" => operands, //TODO Broken
      "eq?" => match operands {
        Cons(box lhs, box Cons(box rhs, _)) => {
          match lhs == rhs {
            true => True,
            false => False,
          }
        },
        _ => True,
      },
      "cons" => match operands {
        Cons(box cadr, box Cons(box caddr, box Nil)) => {
          let newl = self.eval(cadr)?;
          let newr = self.eval(caddr)?;
          Cons(Box::new(newl), Box::new(newr))
        },
        _ => return Err(format!("Bad arguments for cons")),
      },
      "car" => match operands {
        Cons(box car, _) => car,
        _ => return Err(format!("called car with a non-pair argument")),
      },
      "cdr" => match operands {
        Cons(_, box cdr) => cdr,
        _ => return Err(format!("called cdr with a non-pair argument")),
      },
      "atom?" => match operands {
        Cons(_, _) => False,
        _ => True,
      },
      "define" => match operands {
        Cons(box SymbolAtom(sym), box Cons(box expr, box Nil)) => {
          let evaluated = self.eval(expr)?;
          self.set_var(sym, evaluated);
          Nil
        },
        _ => return Err(format!("Bad assignment")),
      }
      "lambda" => match operands {
        Cons(box paramlist, box Cons(box formalexp, box Nil)) => {
          unimplemented!() //needs to return an abstract object
        },
        _ => return Err(format!("Bad lambda expression")),
      },
      "if" => match operands {
        Cons(box test, box body) => {
          let truth_value = test.truthy();
          match (truth_value, body) {
            (true, Cons(box consequent, _)) => consequent,
            (false, Cons(_, box Cons(box alternative, _))) => alternative,
            _ => return Err(format!("Bad if expression"))
          }
        },
        _ => return Err(format!("Bad if expression"))
      },
      _ => unimplemented!(),
    })
  }

  fn apply(&mut self, function: Sexp, operands: Sexp) -> Result<Sexp, String> {
    Err(format!("Not implemented"))
  }
}

fn read(input: &str) -> Result<Vec<Sexp>, String> {
  let mut chars: Peekable<Chars> = input.chars().peekable();
  let mut tokens = tokenize(&mut chars).into_iter().peekable();
  let mut sexps = Vec::new();
  while let Some(_) = tokens.peek() {
    sexps.push(parse(&mut tokens)?);
  }
  Ok(sexps)
}

#[derive(Debug)]
enum Token {
  LParen,
  RParen,
  Quote,
  Word(String),
  StringLiteral(String),
  NumLiteral(u64),
}

//TODO make this notion of Eq more sophisticated
#[derive(Debug, PartialEq, Clone)]
enum Sexp {
  SymbolAtom(String),
  StringAtom(String),
  NumberAtom(u64),
  True,
  False,
  Cons(Box<Sexp>, Box<Sexp>),
  Nil,
  FnLiteral {
    formal_params: Vec<String>,
    body: Box<Sexp>
  }
}

impl Sexp {
  fn print(&self) -> String {
    use self::Sexp::*;
    match self {
      &True => format!("#t"),
      &False => format!("#f"),
      &SymbolAtom(ref sym) => format!("{}", sym),
      &StringAtom(ref s) => format!("\"{}\"", s),
      &NumberAtom(ref n) => format!("{}", n),
      &Cons(ref car, ref cdr) => format!("({} . {})", car.print(), cdr.print()),
      &Nil => format!("()"),
      &FnLiteral { ref formal_params, .. } => format!("<lambda {:?}>", formal_params),
    }
  }

  fn truthy(&self) -> bool {
    use self::Sexp::*;
    match self {
      &False => false,
      _ => true
    }
  }
}

fn tokenize(input: &mut Peekable<Chars>) -> Vec<Token> {
  use self::Token::*;
  let mut tokens = Vec::new();
  loop {
    match input.next() {
      None => break,
      Some('(') => tokens.push(LParen),
      Some(')') => tokens.push(RParen),
      Some('\'') => tokens.push(Quote),
      Some(c) if c.is_whitespace() => continue,
      Some(c) if c.is_numeric() => {
        let tok: String = input.peeking_take_while(|next| next.is_numeric()).collect();
        let n: u64 = format!("{}{}", c, tok).parse().unwrap();
        tokens.push(NumLiteral(n));
      },
      Some('"') => {
        let string: String = input.scan(false, |escape, cur_char| {
          let seen_escape = *escape;
          *escape = cur_char == '\\' && !seen_escape;
          match (cur_char, seen_escape) {
            ('"', false) => None,
            ('\\', false) => Some(None),
            (c, _) => Some(Some(c))
          }
        }).filter_map(|x| x).collect();
        tokens.push(StringLiteral(string));
      }
      Some(c) => {
        let sym: String = input.peeking_take_while(|next| {
          match *next {
            '(' | ')' => false,
            c if c.is_whitespace() => false,
            _ => true
          }
        }).collect();
        tokens.push(Word(format!("{}{}", c, sym)));
      }
    }
  }
  tokens
}

fn parse(tokens: &mut Peekable<IntoIter<Token>>) -> Result<Sexp, String> {
  use self::Token::*;
  use self::Sexp::*;
  match tokens.next() {
    Some(Word(ref s)) if s == "#f" => Ok(False),
    Some(Word(ref s)) if s == "#t" => Ok(True),
    Some(Word(s)) => Ok(SymbolAtom(s)),
    Some(StringLiteral(s)) => Ok(StringAtom(s)),
    Some(LParen) => parse_sexp(tokens),
    Some(RParen) => Err(format!("Unexpected ')'")),
    Some(Quote) => {
      let quoted = parse(tokens)?;
      Ok(Cons(Box::new(SymbolAtom(format!("quote"))), Box::new(quoted)))
    },
    Some(NumLiteral(n)) => Ok(NumberAtom(n)),
    None => Err(format!("Unexpected end of input")),
  }
}

fn parse_sexp(tokens: &mut Peekable<IntoIter<Token>>) -> Result<Sexp, String> {
  use self::Token::*;
  use self::Sexp::*;
  let mut cell = Nil;
  {
    let mut cell_ptr = &mut cell;
    loop {
      match tokens.peek() {
        None => return Err(format!("Unexpected end of input")),
        Some(&RParen) => {
          tokens.next();
          break;
        },
        _ => {
          let current = parse(tokens)?;
          let new_cdr = Cons(Box::new(current), Box::new(Nil));
          match cell_ptr {
            &mut Cons(_, ref mut cdr) => **cdr = new_cdr,
            &mut Nil => *cell_ptr  = new_cdr,
            _ => unreachable!()
          };

          let old_ptr = cell_ptr;
          let new_ptr: &mut Sexp = match old_ptr { &mut Cons(_, ref mut cdr) => cdr, _ => unreachable!() } as &mut Sexp;
          cell_ptr = new_ptr;
        }
      }
    }
  }
  Ok(cell)
}

