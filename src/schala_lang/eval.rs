use schala_lang::parsing::{AST, Statement, Declaration, Expression, Variant, ExpressionType};
use schala_lang::builtin::{BinOp, PrefixOp};
use std::collections::HashMap;
use std::rc::Rc;

pub struct State {
  values: HashMap<Rc<String>, ValueEntry>,
}

enum ValueEntry {
  Binding {
    val: FullyEvaluatedExpr,
  },
  Function {
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

impl State {
  pub fn new() -> State {
    State { values: HashMap::new() }
  }

  pub fn evaluate(&mut self, ast: AST) -> Vec<String> {
    let mut acc = vec![];
    for statement in ast.0 {
      match self.eval_statement(statement) {
        Ok(output) => {
          if let Some(s) = output {
            acc.push(s);
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

impl State {
  fn eval_statement(&mut self, statement: Statement) -> EvalResult<Option<String>> {
    use self::FullyEvaluatedExpr::*;
    match statement {
      Statement::ExpressionStatement(expr) => {
        self.eval_expr(expr).map( |eval| {
          match eval {
            UnsignedInt(n) => Some(format!("{}", n)),
            SignedInt(n) => Some(format!("{}", n)),
            Float(f) => Some(format!("{}", f)),
            Str(s) => Some(format!("\"{}\"", s)),
            Bool(b) => Some(format!("{}", b)),
            Custom { string_rep } => Some(format!("{}", string_rep)),
            Tuple(_items) => Some(format!("(tuple to be defined later)")),
            FuncLit(name) => Some(format!("<function {}>", name)),
          }
        })
      },
      Statement::Declaration(decl) => {
        self.eval_decl(decl).map(|_| None)
      }
    }
  }

  fn eval_decl(&mut self, decl: Declaration) -> EvalResult<()> {
    use self::Declaration::*;
    use self::Variant::*;

    match decl {
      FuncDecl(signature, statements) => {
        let name = signature.name;
        self.values.insert(name, ValueEntry::Function { body: statements.clone() });
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
      Value(name, _) => self.eval_value(name),
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
      Call { f, arguments } => self.eval_application(*f, arguments),
      x => Err(format!("Unimplemented thing {:?}", x)),
    }
  }

  fn eval_application(&mut self, f: Expression, _arguments: Vec<Expression>) -> EvalResult<FullyEvaluatedExpr> {
    use self::ExpressionType::*;
    match f {
      Expression(Value(identifier, _), _) => {
        match self.values.get(&identifier) {
          Some(&ValueEntry::Function { ref body }) => {
            let new_state = State::new();
            let sub_ast = AST(body.clone());
            println!("LOL ALL FUNCTIONS EVALUATE TO 2!");
            Ok(FullyEvaluatedExpr::UnsignedInt(2))
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
      Some(lookup) => {
        match lookup {
          &Binding { ref val } => Ok(val.clone()),
          &Function { .. } =>  Ok(FullyEvaluatedExpr::FuncLit(name.clone()))
        }
      }
    }
  }

  fn eval_binexp(&mut self, op: BinOp, lhs: Box<Expression>, rhs: Box<Expression>) -> EvalResult<FullyEvaluatedExpr> {
    use self::FullyEvaluatedExpr::*;
    let evaled_lhs = self.eval_expr(*lhs)?;
    let evaled_rhs = self.eval_expr(*rhs)?;
    let sigil: &str = op.sigil.as_ref().as_str();
    Ok(match (sigil, evaled_lhs, evaled_rhs) {
      ("+", UnsignedInt(l), UnsignedInt(r)) => UnsignedInt(l + r),
      ("++", Str(s1), Str(s2)) => Str(format!("{}{}", s1, s2)),
      ("-", UnsignedInt(l), UnsignedInt(r)) => UnsignedInt(l - r),
      ("*", UnsignedInt(l), UnsignedInt(r)) => UnsignedInt(l * r),
      ("/", UnsignedInt(l), UnsignedInt(r)) => UnsignedInt(l / r),
      ("%", UnsignedInt(l), UnsignedInt(r)) => UnsignedInt(l % r),
      _ => return Err(format!("Runtime error: not yet implemented")),
    })
  }

  fn eval_prefix_exp(&mut self, op: PrefixOp, expr: Box<Expression>) -> EvalResult<FullyEvaluatedExpr> {
    use self::FullyEvaluatedExpr::*;
    let evaled_expr = self.eval_expr(*expr)?;

    let sigil: &str = op.sigil.as_ref().as_str();

    Ok(match (sigil, evaled_expr) {
      ("!", Bool(true)) => Bool(false),
      ("!", Bool(false)) => Bool(true),
      ("-", UnsignedInt(n)) => SignedInt(-1*(n as i64)),
      ("-", SignedInt(n)) => SignedInt(-1*(n as i64)),
      _ => return Err(format!("Runtime error: not yet implemented")),
    })
  }
}
