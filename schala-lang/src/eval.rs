use std::rc::Rc;
use std::fmt::Write;
use std::io;

use itertools::Itertools;

use util::StateStack;
use ast_reducing::{ReducedAST, Stmt, Expr, Lit, Func};

pub struct State<'a> {
  values: StateStack<'a, Rc<String>, ValueEntry>
}

macro_rules! builtin_binding {
  ($name:expr, $values:expr) => {
    $values.insert(Rc::new(format!($name)), ValueEntry::Binding { constant: true, val: Expr::Func(Func::BuiltIn(Rc::new(format!($name)))) });
  }
}

impl<'a> State<'a> {
  pub fn new() -> State<'a> {
    let mut values = StateStack::new(Some(format!("global")));
    builtin_binding!("print", values);
    builtin_binding!("println", values);
    builtin_binding!("getline", values);
    State { values }
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


/*
#[derive(Debug, PartialEq, Clone)]
enum FullyEvaluatedExpr {
  UnsignedInt(u64),
  SignedInt(i64),
  Float(f64),
  Str(String),
  Bool(bool),
  FuncLit(Rc<String>),
  Custom {
    string_rep: Rc<String>,
  },
  Tuple(Vec<FullyEvaluatedExpr>),
  List(Vec<FullyEvaluatedExpr>)
}

impl FullyEvaluatedExpr {
  fn to_string(&self) -> String {
    use self::FullyEvaluatedExpr::*;
    match self {
      &UnsignedInt(ref n) => format!("{}", n),
      &SignedInt(ref n) => format!("{}", n),
      &Float(ref f) => format!("{}", f),
      &Str(ref s) => format!("\"{}\"", s),
      &Bool(ref b) => format!("{}", b),
      &Custom { ref string_rep } => format!("{}", string_rep),
      &Tuple(ref items) => {
        let mut buf = String::new();
        write!(buf, "(").unwrap();
        for term in items.iter().map(|e| Some(e)).intersperse(None) {
          match term {
            Some(e) => write!(buf, "{}", e.to_string()).unwrap(),
            None => write!(buf, ", ").unwrap(),
          };
        }
        write!(buf, ")").unwrap();
        buf
      },
      &FuncLit(ref name) => format!("<function {}>", name),
      &List(ref items) => {
        let mut buf = String::new();
        write!(buf, "[").unwrap();
        for term in items.iter().map(|e| Some(e)).intersperse(None) {
          match term {
            Some(e) => write!(buf, "{}", e.to_string()).unwrap(),
            None => write!(buf, ", ").unwrap()
          }
        }
        write!(buf, "]").unwrap();
        buf
      }
    }
  }
}
*/

  /*

  pub fn new_with_parent(parent: &'a State<'a>) -> State<'a> {
    State { parent_frame: Some(parent), values: HashMap::new() }
  }

  pub fn evaluate(&mut self, ast: AST) -> Vec<Result<String, String>> {
    let mut acc = vec![];
    for statement in ast.0 {
      match self.eval_statement(statement) {
        Ok(output) => {
          if let Some(fully_evaluated) = output {
            acc.push(Ok(fully_evaluated.to_string()));
          }
        },
        Err(error) => {
          acc.push(Err(format!("Eval error: {}", error)));
          return acc;
        },
      }
    }
    acc
  }
}

impl<'a> State<'a> {
  fn eval_statement(&mut self, statement: Statement) -> EvalResult<Option<FullyEvaluatedExpr>> {
    Ok(match statement {
      Statement::ExpressionStatement(expr) => Some(self.eval_expr(expr)?),
      Statement::Declaration(decl) => { self.eval_decl(decl)?; None }
    })
  }

  fn eval_decl(&mut self, decl: Declaration) -> EvalResult<()> {
    use self::Declaration::*;
    use self::Variant::*;

    match decl {
      FuncDecl(signature, statements) => {
        let name = signature.name;
        let param_names: Vec<Rc<String>> = signature.params.iter().map(|fp| fp.0.clone()).collect();
        self.insert(name, ValueEntry::Function { body: statements.clone(), param_names });
      },
      TypeDecl(_name, body) => {
        for variant in body.0.iter() {
          match variant {
            &UnitStruct(ref name) => self.insert(name.clone(),
              ValueEntry::Binding { val: FullyEvaluatedExpr::Custom { string_rep: name.clone() } }),
            &TupleStruct(ref _name, ref _args) =>  unimplemented!(),
            &Record(ref _name, ref _fields) => unimplemented!(),
          };
        }
      },
      Binding { name, expr, ..} => {
        let val = self.eval_expr(expr)?;
        self.insert(name.clone(), ValueEntry::Binding { val });
      },
      _ => return Err(format!("Declaration evaluation not yet implemented"))
    }
    Ok(())
  }

  fn eval_expr(&mut self, expr: Expression) -> EvalResult<FullyEvaluatedExpr> {
    use self::ExpressionType::*;
    use self::FullyEvaluatedExpr::*;

    let expr_type = expr.0;
    match expr_type {
      IntLiteral(n) => Ok(UnsignedInt(n)),
      FloatLiteral(f) => Ok(Float(f)),
      StringLiteral(s) => Ok(Str(s.to_string())),
      BoolLiteral(b) => Ok(Bool(b)),
      PrefixExp(op, expr) => self.eval_prefix_exp(op, expr),
      BinExp(op, lhs, rhs) => self.eval_binexp(op, lhs, rhs),
      Value(name) => self.eval_value(name),
      TupleLiteral(expressions) => {
        let mut evals = Vec::new();
        for expr in expressions {
          match self.eval_expr(expr) {
            Ok(fully_evaluated) => evals.push(fully_evaluated),
            error => return error,
          }
        }
        Ok(Tuple(evals))
      }
      Call { f, arguments } => {
        let mut evaled_arguments = Vec::new();
        for arg in arguments.into_iter() {
          evaled_arguments.push(self.eval_expr(arg)?);
        }
        self.eval_application(*f, evaled_arguments)
      },
      Index { box indexee, indexers } => {
        let evaled = self.eval_expr(indexee)?;
        match evaled {
          Tuple(mut exprs) => {
            let len = indexers.len();
            if len == 1 {
              let idx = indexers.into_iter().nth(0).unwrap();
              match self.eval_expr(idx)? {
                UnsignedInt(n) if (n as usize) < exprs.len() => Ok(exprs.drain(n as usize..).next().unwrap()),
                UnsignedInt(n) => Err(format!("Index {} out of range", n)),
                other => Err(format!("{:?} is not an unsigned integer", other)),
              }
            } else {
              Err(format!("Tuple index must be one integer"))
            }
          },
          _ => Err(format!("Bad index expression"))
        }
      },
      ListLiteral(items) => Ok(List(items.into_iter().map(|item| self.eval_expr(item)).collect::<Result<Vec<_>,_>>()?)),
      x => Err(format!("Unimplemented thing {:?}", x)),
    }
  }

  fn eval_application(&mut self, f: Expression, arguments: Vec<FullyEvaluatedExpr>) -> EvalResult<FullyEvaluatedExpr> {
    use self::ExpressionType::*;
    match f {
      Expression(Value(ref identifier), _) if self.is_builtin(identifier) =>  self.eval_builtin(identifier, arguments),
      Expression(Value(identifier), _) => {
        match self.lookup(&identifier) {
          Some(&ValueEntry::Function { ref body, ref param_names }) => {
            if arguments.len() != param_names.len() {
              return Err(format!("Wrong number of arguments for the function"));
            }
            let mut new_state = State::new_with_parent(self);
            let sub_ast = body.clone();
            for (param, val) in param_names.iter().zip(arguments.into_iter()) {
              new_state.insert(param.clone(), ValueEntry::Binding { val });
            }
            let mut ret: Option<FullyEvaluatedExpr> = None;
            for statement in sub_ast.into_iter() {
              ret = new_state.eval_statement(statement)?;
            }
            Ok(ret.unwrap_or(FullyEvaluatedExpr::Custom { string_rep: Rc::new("()".to_string()) }))
          },
          _ => Err(format!("Function {} not found", identifier)),
        }
      },
      x => Err(format!("Trying to apply {:?} which is not a function", x)),
    }
  }
  fn is_builtin(&self, name: &Rc<String>) -> bool {
    match &name.as_ref()[..] {
      "print" | "println" => true,
      _ => false
    }
  }
  fn eval_builtin(&mut self, name: &Rc<String>, args: Vec<FullyEvaluatedExpr>) -> EvalResult<FullyEvaluatedExpr> {
    use self::FullyEvaluatedExpr::*;
    match &name.as_ref()[..] {
      "print" => {
        for arg in args {
          print!("{}", arg.to_string());
        }
        Ok(Tuple(vec![]))
      },
      "println" => {
        for arg in args {
          println!("{}", arg.to_string());
        }
        Ok(Tuple(vec![]))
      },
      _ => unreachable!()
    }
  }
  fn eval_value(&mut self, name: Rc<String>) -> EvalResult<FullyEvaluatedExpr> {
    use self::ValueEntry::*;
    match self.lookup(&name) {
      None => return Err(format!("Value {} not found", *name)),
      Some(lookup) => match lookup {
        &Binding { ref val } => Ok(val.clone()),
        &Function { .. } =>  Ok(FullyEvaluatedExpr::FuncLit(name.clone()))
      }
    }
  }

}
*/

/* BELOW HERE NEW STUFF */

impl Expr {
  fn to_repl(&self) -> String {
    use self::Lit::*;
    use self::Func::*;
    match self {
      Expr::Lit(ref l) => match l {
        Nat(n) => format!("{}", n),
        Int(i) => format!("{}", i),
        Float(f) => format!("{}", f),
        Bool(b) => format!("{}", b),
        StringLit(s) => format!("{}", s),
      },
      Expr::Func(f) => match f {
        BuiltIn(name) => format!("<built-in function {}>", name),
        UserDefined { name: None, .. } => format!("<function>"),
        UserDefined { name: Some(name), .. } => format!("<function {}>", name),
      },
      _ => format!("{:?}", self),
    }
  }
}

impl<'a> State<'a> {
  pub fn evaluate(&mut self, ast: ReducedAST, repl: bool) -> Vec<Result<String, String>> {
    let mut acc = vec![];
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

  fn statement(&mut self, stmt: Stmt) -> EvalResult<Option<Expr>> {
    match stmt {
      Stmt::Binding { name, constant, expr } => {
        let val = self.expression(expr)?;
        self.values.insert(name.clone(), ValueEntry::Binding { constant, val });
        Ok(None)
      },
      Stmt::Expr(expr) => Ok(Some(self.expression(expr)?)),
    }
  }

  fn expression(&mut self, expr: Expr) -> EvalResult<Expr> {
    use self::Expr::*;
    match expr {
      literal @ Lit(_) => Ok(literal),
      Call { box f, args } => {
        let f = match self.expression(f)? {
          Func(f) => f,
          other => return Err(format!("Tried to call {:?} which is not a function", other)),
        };
        self.apply_function(f, args)
      },
      Val(v) => self.value(v),
      func @ Func(_) => Ok(func),
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

  fn apply_function(&mut self, f: Func, args: Vec<Expr>) -> EvalResult<Expr> {
    match f {
      Func::BuiltIn(sigil) => self.apply_builtin(sigil, args),
      Func::UserDefined { params, body, name } => {

        if params.len() != args.len() {
          return Err(format!("calling a {}-argument function with {} args", params.len(), args.len()))
        }
        let mut func_state = State { values: self.values.new_frame(name.map(|n| format!("{}", n))) };
        for (param, val) in params.into_iter().zip(args.into_iter()) {
          func_state.values.insert(param, ValueEntry::Binding { constant: true, val });
        }
        // TODO figure out function return semantics
        let mut ret = None;
        for stmt in body {
          ret = func_state.statement(stmt)?;
        }
        Ok(ret.unwrap_or(Expr::Unit))
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
        Lit(StringLit(Rc::new(buf)))
      },
      (x, args) => return Err(format!("bad or unimplemented builtin {:?} | {:?}", x, args)),
    })
  }

  fn value(&mut self, name: Rc<String>) -> EvalResult<Expr> {
    use self::ValueEntry::*;
    match self.values.lookup(&name) {
      None => return Err(format!("Value {} not found", *name)),
      Some(lookup) => match lookup {
        Binding { val, .. } => Ok(
          if let Expr::Func(Func::UserDefined { name: None, params, body }) = val {
            Expr::Func(Func::UserDefined { name: Some(name.clone()), params: params.clone(), body: body.clone() }) //TODO here is unnecessary cloning
          } else {
            val.clone()
          }
        )
      }
    }
  }
}

#[cfg(test)]
mod eval_tests {
  use tokenizing::tokenize;
  use parsing::parse;
  use eval::State;

  macro_rules! fresh_env {
    ($string:expr, $correct:expr) => {
      let mut state = State::new();
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
  }
}
