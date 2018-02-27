use schala_lang::parsing::{AST, Statement, Declaration, Expression, Variant, ExpressionType};
use schala_lang::builtin::{BinOp, PrefixOp};
use std::collections::HashMap;
use std::rc::Rc;

pub struct State<'a> {
  parent_frame: Option<&'a State<'a>>,
  values: HashMap<Rc<String>, ValueEntry>,
}

#[derive(Debug)]
enum ValueEntry {
  Binding {
    val: FullyEvaluatedExpr,
  },
  Function {
    param_names: Vec<Rc<String>>,
    body: Vec<Statement>,
  }
}

type EvalResult<T> = Result<T, String>;

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
      &Tuple(ref _items) => format!("(tuple to be defined later)"),
      &FuncLit(ref name) => format!("<function {}>", name),
    }
  }
}

impl<'a> State<'a> {
  pub fn new() -> State<'a> {
    State { parent_frame: None, values: HashMap::new() }
  }

  pub fn new_with_parent(parent: &'a State<'a>) -> State<'a> {
    State { parent_frame: Some(parent), values: HashMap::new() }
  }

  pub fn evaluate(&mut self, ast: AST) -> Vec<String> {
    let mut acc = vec![];
    for statement in ast.0 {
      match self.eval_statement(statement) {
        Ok(output) => {
          if let Some(fully_evaluated) = output {
            acc.push(fully_evaluated.to_string());
          }
        },
        Err(error) => {
          acc.push(format!("Eval error: {}", error));
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
        self.values.insert(name, ValueEntry::Function { body: statements.clone(), param_names });
      },
      TypeDecl(_name, body) => {
        for variant in body.0.iter() {
          match variant {
            &UnitStruct(ref name) => self.values.insert(name.clone(),
              ValueEntry::Binding { val: FullyEvaluatedExpr::Custom { string_rep: name.clone() } }),
            &TupleStruct(ref _name, ref _args) =>  unimplemented!(),
            &Record(ref _name, ref _fields) => unimplemented!(),
          };
        }
      },
      Binding { name, constant, expr } => {
        let val = self.eval_expr(expr)?;
        self.values.insert(name.clone(), ValueEntry::Binding { val });
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
      x => Err(format!("Unimplemented thing {:?}", x)),
    }
  }

  fn eval_application(&mut self, f: Expression, arguments: Vec<FullyEvaluatedExpr>) -> EvalResult<FullyEvaluatedExpr> {
    use self::ExpressionType::*;
    match f {
      Expression(Value(identifier), _) => {
        match self.values.get(&identifier) {
          Some(&ValueEntry::Function { ref body, ref param_names }) => {
            if arguments.len() != param_names.len() {
              return Err(format!("Wrong number of arguments for the function"));
            }
            let mut new_state = State::new_with_parent(self);
            let sub_ast = body.clone();
            for (param, val) in param_names.iter().zip(arguments.into_iter()) {
              new_state.values.insert(param.clone(), ValueEntry::Binding { val });
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

  fn eval_value(&mut self, name: Rc<String>) -> EvalResult<FullyEvaluatedExpr> {
    use self::ValueEntry::*;
    match self.values.get(&name) {
      None => return Err(format!("Value {} not found", *name)),
      Some(lookup) => match lookup {
        &Binding { ref val } => Ok(val.clone()),
        &Function { .. } =>  Ok(FullyEvaluatedExpr::FuncLit(name.clone()))
      }
    }
  }

  fn eval_binexp(&mut self, op: BinOp, lhs: Box<Expression>, rhs: Box<Expression>) -> EvalResult<FullyEvaluatedExpr> {
    use self::FullyEvaluatedExpr::*;
    let evaled_lhs = self.eval_expr(*lhs)?;
    let evaled_rhs = self.eval_expr(*rhs)?;
    let sigil = op.sigil();
    //let sigil: &str = op.sigil().as_ref().as_str();
    Ok(match (sigil.as_str(), evaled_lhs, evaled_rhs) {
      ("+", UnsignedInt(l), UnsignedInt(r)) => UnsignedInt(l + r),
      ("++", Str(s1), Str(s2)) => Str(format!("{}{}", s1, s2)),
      ("-", UnsignedInt(l), UnsignedInt(r)) => UnsignedInt(l - r),
      ("*", UnsignedInt(l), UnsignedInt(r)) => UnsignedInt(l * r),
      ("/", UnsignedInt(l), UnsignedInt(r)) => Float((l as f64)/ (r as f64)),
      ("//", UnsignedInt(l), UnsignedInt(r)) => if r == 0 {
        return Err(format!("Runtime error: divide by zero"));
      } else {
        UnsignedInt(l / r)
      },
      ("%", UnsignedInt(l), UnsignedInt(r)) => UnsignedInt(l % r),
      ("^", UnsignedInt(l), UnsignedInt(r)) => UnsignedInt(l ^ r),
      ("&", UnsignedInt(l), UnsignedInt(r)) => UnsignedInt(l & r),
      ("|", UnsignedInt(l), UnsignedInt(r)) => UnsignedInt(l | r),
      _ => return Err(format!("Runtime error: not yet implemented")),
    })
  }

  fn eval_prefix_exp(&mut self, op: PrefixOp, expr: Box<Expression>) -> EvalResult<FullyEvaluatedExpr> {
    use self::FullyEvaluatedExpr::*;
    let evaled_expr = self.eval_expr(*expr)?;
    let sigil = op.sigil();

    Ok(match (sigil.as_str(), evaled_expr) {
      ("!", Bool(true)) => Bool(false),
      ("!", Bool(false)) => Bool(true),
      ("-", UnsignedInt(n)) => SignedInt(-1*(n as i64)),
      ("-", SignedInt(n)) => SignedInt(-1*(n as i64)),
      ("+", SignedInt(n)) => SignedInt(n),
      ("+", UnsignedInt(n)) => UnsignedInt(n),
      _ => return Err(format!("Runtime error: not yet implemented")),
    })
  }
}
