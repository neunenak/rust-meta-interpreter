use std::cell::RefCell;
use std::rc::Rc;
use std::fmt::Write;
use std::io;

use itertools::Itertools;

use util::StateStack;
use reduced_ast::{ReducedAST, Stmt, Expr, Lit, Func};
use symbol_table::{SymbolSpec, Symbol, SymbolTable};

pub struct State<'a> {
  values: StateStack<'a, Rc<String>, ValueEntry>,
  symbol_table_handle: Rc<RefCell<SymbolTable>>,
}

macro_rules! builtin_binding {
  ($name:expr, $values:expr) => {
    $values.insert(Rc::new(format!($name)), ValueEntry::Binding { constant: true, val: Expr::Func(Func::BuiltIn(Rc::new(format!($name)))) });
  }
}

impl<'a> State<'a> {
  pub fn new(symbol_table_handle: Rc<RefCell<SymbolTable>>) -> State<'a> {
    let mut values = StateStack::new(Some(format!("global")));
    builtin_binding!("print", values);
    builtin_binding!("println", values);
    builtin_binding!("getline", values);
    State { values, symbol_table_handle }
  }

  pub fn debug_print(&self) -> String {
    format!("Values: {:?}", self.values)
  }
}

#[derive(Debug)]
enum ValueEntry {
  Binding {
    constant: bool,
    val: /*FullyEvaluatedExpr*/ Expr,
  }
}

type EvalResult<T> = Result<T, String>;


impl Expr {
  fn to_repl(&self) -> String {
    use self::Lit::*;
    use self::Func::*;
    fn paren_wrapped_vec(exprs: &Vec<Expr>) -> String {
      let mut buf = String::new();
      write!(buf, "(").unwrap();
      for term in exprs.iter().map(|e| Some(e)).intersperse(None) {
        match term {
          Some(e) => write!(buf, "{}", e.to_repl()).unwrap(),
          None => write!(buf, ", ").unwrap(),
        };
      }
      write!(buf, ")").unwrap();
      buf
    }

    match self {
      Expr::Lit(ref l) => match l {
        Nat(n) => format!("{}", n),
        Int(i) => format!("{}", i),
        Float(f) => format!("{}", f),
        Bool(b) => format!("{}", b),
        StringLit(s) => format!("\"{}\"", s),
        Custom(name, args) if args.len() == 0 => format!("{}", name),
        Custom(name, args) => format!("{}{}", name, paren_wrapped_vec(args)),
      },
      Expr::Func(f) => match f {
        BuiltIn(name) => format!("<built-in function {}>", name),
        UserDefined { name: None, .. } => format!("<function>"),
        UserDefined { name: Some(name), .. } => format!("<function {}>", name),
      },
      Expr::Tuple(exprs) => paren_wrapped_vec(exprs),
      _ => format!("{:?}", self),
    }
  }
}

impl<'a> State<'a> {
  pub fn evaluate(&mut self, ast: ReducedAST, repl: bool) -> Vec<Result<String, String>> {
    let mut acc = vec![];

    // handle prebindings
    for statement in ast.0.iter() {
      self.prebinding(statement);
    }

    for statement in ast.0 {
      match self.statement(statement) {
        Ok(Some(ref output)) if repl => acc.push(Ok(output.to_repl())),
        Ok(_) => (),
        Err(error) => {
          acc.push(Err(format!("Runtime error: {}", error)));
          return acc;
        },
      }
    }
    acc
  }

  fn prebinding(&mut self, stmt: &Stmt) {
    match stmt {
      Stmt::PreBinding { name, func } => {
        let v_entry = ValueEntry::Binding { constant: true, val: Expr::Func(func.clone()) };
        self.values.insert(name.clone(), v_entry);
      },
      Stmt::Expr(_expr) => {
        //TODO have this support things like nested function defs

      },
      _ => ()
    }
  }

  fn statement(&mut self, stmt: Stmt) -> EvalResult<Option<Expr>> {
    match stmt {
      Stmt::Binding { name, constant, expr } => {
        let val = self.expression(expr)?;
        self.values.insert(name.clone(), ValueEntry::Binding { constant, val });
        Ok(None)
      },
      Stmt::Expr(expr) => Ok(Some(self.expression(expr)?)),
      Stmt::PreBinding {..} | Stmt::Noop => Ok(None),
    }
  }

  fn block(&mut self, stmts: Vec<Stmt>) -> EvalResult<Expr> {
    let mut ret = None;
    for stmt in stmts {
      ret = self.statement(stmt)?;
    }
    Ok(ret.unwrap_or(Expr::Unit))
  }

  fn expression(&mut self, expr: Expr) -> EvalResult<Expr> {
    use self::Expr::*;
    match expr {
      literal @ Lit(_) => Ok(literal),
      Call { box f, args } => {
        if let Val(name) = f {
          self.apply_data_constructor(name, args)
        } else {
          match self.expression(f)? {
            Func(f) => self.apply_function(f, args),
            other => return Err(format!("Tried to call {:?} which is not a function or data constructor", other)),
          }
        }
      },
      Val(v) => self.value(v),
      func @ Func(_) => Ok(func),
      Tuple(exprs) => Ok(Tuple(exprs.into_iter().map(|expr| self.expression(expr)).collect::<Result<Vec<Expr>,_>>()?)),
      Conditional { box cond, then_clause, else_clause } => self.conditional(cond, then_clause, else_clause),
      Assign { box val, box expr } => {
        let name = match val  {
          Expr::Val(name) => name,
          _ => return Err(format!("Trying to assign to a non-value")),
        };

        let constant = match self.values.lookup(&name) {
          None => return Err(format!("{} is undefined", name)),
          Some(ValueEntry::Binding { constant, .. }) => constant.clone(),
        };
        if constant {
          return Err(format!("trying to update {}, a non-mutable binding", name));
        }
        let val = self.expression(expr)?;
        self.values.insert(name.clone(), ValueEntry::Binding { constant: false, val });
        Ok(Expr::Unit)
      },
      e => Err(format!("Expr {:?} eval not implemented", e))
    }
  }

  fn apply_data_constructor(&mut self, name: Rc<String>, args: Vec<Expr>) -> EvalResult<Expr> {
    let symbol_table = self.symbol_table_handle.borrow();
    match symbol_table.values.get(&name) {
      Some(Symbol { spec: SymbolSpec::DataConstructor { type_name, type_args }, name }) => {
        if args.len() != type_args.len() {
          return Err(format!("Data constructor {} requires {} args", name, type_args.len()));
        }
        Ok(Expr::Lit(self::Lit::Custom(name.clone(), vec![])))
      },
      _ => return Err(format!("Bad symbol {}", name))
    }
  }

  fn apply_function(&mut self, f: Func, args: Vec<Expr>) -> EvalResult<Expr> {
    match f {
      Func::BuiltIn(sigil) => self.apply_builtin(sigil, args),
      Func::UserDefined { params, body, name } => {

        if params.len() != args.len() {
          return Err(format!("calling a {}-argument function with {} args", params.len(), args.len()))
        }
        let mut func_state = State {
          values: self.values.new_frame(name.map(|n| format!("{}", n))),
          symbol_table_handle: self.symbol_table_handle.clone(),
        };
        for (param, val) in params.into_iter().zip(args.into_iter()) {
          let val = func_state.expression(val)?;
          func_state.values.insert(param, ValueEntry::Binding { constant: true, val });
        }
        // TODO figure out function return semantics
        func_state.block(body)
      }
    }
  }

  fn apply_builtin(&mut self, name: Rc<String>, args: Vec<Expr>) -> EvalResult<Expr> {
    use self::Expr::*;
    use self::Lit::*;
    let evaled_args: Result<Vec<Expr>, String> = args.into_iter().map(|arg| self.expression(arg)).collect();
    let evaled_args = evaled_args?;

    Ok(match (name.as_str(), evaled_args.as_slice()) {
      /* binops */
      ("+", &[Lit(Nat(l)), Lit(Nat(r))]) => Lit(Nat(l + r)),
      ("++", &[Lit(StringLit(ref s1)), Lit(StringLit(ref s2))]) => Lit(StringLit(Rc::new(format!("{}{}", s1, s2)))),
      ("-", &[Lit(Nat(l)), Lit(Nat(r))]) => Lit(Nat(l - r)),
      ("*", &[Lit(Nat(l)), Lit(Nat(r))]) => Lit(Nat(l * r)),
      ("/", &[Lit(Nat(l)), Lit(Nat(r))]) => Lit(Float((l as f64)/ (r as f64))),
      ("//", &[Lit(Nat(l)), Lit(Nat(r))]) => if r == 0 {
        return Err(format!("divide by zero"));
      } else {
        Lit(Nat(l / r))
      },
      ("%", &[Lit(Nat(l)), Lit(Nat(r))]) => Lit(Nat(l % r)),
      ("^", &[Lit(Nat(l)), Lit(Nat(r))]) => Lit(Nat(l ^ r)),
      ("&", &[Lit(Nat(l)), Lit(Nat(r))]) => Lit(Nat(l & r)),
      ("|", &[Lit(Nat(l)), Lit(Nat(r))]) => Lit(Nat(l | r)),

      ("==", &[Lit(Nat(l)), Lit(Nat(r))]) => Lit(Bool(l == r)),
      ("==", &[Lit(Int(l)), Lit(Int(r))]) => Lit(Bool(l == r)),
      ("==", &[Lit(Float(l)), Lit(Float(r))]) => Lit(Bool(l == r)),
      ("==", &[Lit(Bool(l)), Lit(Bool(r))]) => Lit(Bool(l == r)),
      ("==", &[Lit(StringLit(ref l)), Lit(StringLit(ref r))]) => Lit(Bool(l == r)),

      /* prefix ops */
      ("!", &[Lit(Bool(true))]) => Lit(Bool(false)),
      ("!", &[Lit(Bool(false))]) => Lit(Bool(true)),
      ("-", &[Lit(Nat(n))]) => Lit(Int(-1*(n as i64))),
      ("-", &[Lit(Int(n))]) => Lit(Int(-1*(n as i64))),
      ("+", &[Lit(Int(n))]) => Lit(Int(n)),
      ("+", &[Lit(Nat(n))]) => Lit(Nat(n)),


      /* builtin functions */
      ("print", &[ref anything]) => {
        print!("{}", anything.to_repl());
        Expr::Unit
      },
      ("println", &[ref anything]) => {
        println!("{}", anything.to_repl());
        Expr::Unit
      },
      ("getline", &[]) => {
        let mut buf = String::new();
        io::stdin().read_line(&mut buf).expect("Error readling line in 'getline'");
        Lit(StringLit(Rc::new(buf.trim().to_string())))
      },
      (x, args) => return Err(format!("bad or unimplemented builtin {:?} | {:?}", x, args)),
    })
  }

  fn conditional(&mut self, cond: Expr, then_clause: Vec<Stmt>, else_clause: Vec<Stmt>) -> EvalResult<Expr> {
    let cond = self.expression(cond)?;
    Ok(match cond {
      Expr::Lit(Lit::Bool(true)) =>  self.block(then_clause)?,
      Expr::Lit(Lit::Bool(false)) => self.block(else_clause)?,
      _ => return Err(format!("Conditional with non-boolean condition"))
    })
  }

  fn value(&mut self, name: Rc<String>) -> EvalResult<Expr> {
    use self::ValueEntry::*;
    use self::Func::*;
    //TODO add a layer of indirection here to talk to the symbol table first, and only then look up
    //in the values table

    let symbol_table = self.symbol_table_handle.borrow();
    Ok(match symbol_table.values.get(&name) {
      Some(Symbol { name, spec }) => match spec {
        SymbolSpec::DataConstructor { type_name, type_args } => {
          if type_args.len() == 0 {
            Expr::Lit(Lit::Custom(name.clone(), vec![]))
          } else {
            return Err(format!("This data constructor thing not done"))
          }
        },
        SymbolSpec::Func(_) => match self.values.lookup(&name) {
          Some(Binding { val: Expr::Func(UserDefined { name, params, body }), .. }) => {
            Expr::Func(UserDefined { name: name.clone(), params: params.clone(), body: body.clone() })
          },
          _ => unreachable!(),
        },
      },
      /* see if it's an ordinary variable TODO make variables go in symbol table */
      None => match self.values.lookup(&name) {
        Some(Binding { val, .. }) => val.clone(),
        None => return Err(format!("Couldn't find value {}", name)),
      }
    })
  }
}

#[cfg(test)]
mod eval_tests {
  use std::cell::RefCell;
  use std::rc::Rc;
  use symbol_table::SymbolTable;
  use tokenizing::tokenize;
  use parsing::parse;
  use eval::State;

  macro_rules! fresh_env {
    ($string:expr, $correct:expr) => {
      let symbol_table = Rc::new(RefCell::new(SymbolTable::new()));
      let mut state = State::new(symbol_table);
      let all_output = state.evaluate(parse(tokenize($string)).0.unwrap().reduce(), true);
      let ref output = all_output.last().unwrap();
      assert_eq!(**output, Ok($correct.to_string()));
    }
  }

  #[test]
  fn test_basic_eval() {
    fresh_env!("1 + 2", "3");
    fresh_env!("var a = 1; a = 2", "Unit");
    fresh_env!("var a = 1; a = 2; a", "2");
    fresh_env!(r#"("a", 1 + 2)"#, r#"("a", 3)"#);
  }

  #[test]
  fn function_eval() {
    fresh_env!("fn oi(x) { x + 1 }; oi(4)", "5");
    fresh_env!("fn oi(x) { x + 1 }; oi(1+2)", "4");
  }

  #[test]
  fn scopes() {
    let scope_ok = r#"
    const a = 20
    fn haha() {
      const a = 10
      a
    }
    haha()
    "#;
    fresh_env!(scope_ok, "10");
    let scope_ok = r#"
    const a = 20
    fn haha() {
      const a = 10
      a
    }
    a
    "#;
    fresh_env!(scope_ok, "20");
  }
}
