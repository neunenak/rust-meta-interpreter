extern crate take_mut;

use std::collections::HashMap;
use std::collections::VecDeque;
use parser::{AST, Statement, Expression, Function, Callable, BinOp};
use std::rc::Rc;
use std::io::{Write, Stdout, BufWriter};
use std::convert::From;

use parser::Expression::*;
use parser::Statement::*;

type Reduction<T> = (T, Option<SideEffect>);

#[derive(Debug, Clone)]
enum ReducedValue {
    StringLiteral(Rc<String>),
    ListLiteral(VecDeque<Expression>),
    StructLiteral(VecDeque<(Rc<String>, Expression)>),
    Number(f64),
    Lambda(Function),
}

impl From<ReducedValue> for Expression {
    fn from(rv: ReducedValue) -> Expression {
        match rv {
            ReducedValue::Number(n) => Expression::Number(n),
            ReducedValue::StringLiteral(n) => Expression::StringLiteral(n),
            ReducedValue::Lambda(f) => Expression::Lambda(f),
            ReducedValue::ListLiteral(items) => Expression::ListLiteral(items),
            ReducedValue::StructLiteral(items) => Expression::StructLiteral(items),
        }
    }
}

impl From<Expression> for ReducedValue {
    fn from(rv: Expression) -> ReducedValue {
        match rv {
            Expression::Number(n) => ReducedValue::Number(n),
            Expression::StringLiteral(n) => ReducedValue::StringLiteral(n),
            Expression::Lambda(f) => ReducedValue::Lambda(f),
            Expression::ListLiteral(items) => ReducedValue::ListLiteral(items),
            Expression::StructLiteral(items) => ReducedValue::StructLiteral(items),
            _ => panic!("trying to store a non-fully-reduced variable"),
        }
    }
}

fn get_indexer(f: f64) -> Option<usize> {
    if f.fract() == 0.0 {
        if f.trunc() >= 0.0 {
            return Some(f.trunc() as usize);
        }
    }
    None
}

#[derive(Debug)]
enum SideEffect {
    Print(String),
    AddBinding(Rc<String>, ReducedValue),
}

pub struct Evaluator<'a> {
    parent: Option<&'a Evaluator<'a>>,
    variables: HashMap<String, ReducedValue>,
    stdout: BufWriter<Stdout>,
    pub trace_evaluation: bool,
}

impl<'a> Evaluator<'a> {
    pub fn new(parent: Option<&'a Evaluator>) -> Evaluator<'a> {
        Evaluator {
            variables: HashMap::new(),
            parent: parent,
            stdout: BufWriter::new(::std::io::stdout()),
            trace_evaluation: parent.map_or(false, |e| e.trace_evaluation),
        }
    }

    pub fn run(&mut self, ast: AST) -> Vec<String> {
        ast.into_iter()
            .map(|astnode| format!("{}", self.reduction_loop(astnode)))
            .collect()

    }

    fn add_binding(&mut self, var: String, value: ReducedValue) {
        self.variables.insert(var, value);
    }

    fn lookup_binding(&self, var: &str) -> Option<ReducedValue> {
        match self.variables.get(var) {
            Some(expr) => Some(expr.clone()),
            None => match self.parent {
                Some(env) => env.lookup_binding(var),
                None => None
            }
        }
    }
}

trait Evaluable {
    fn is_reducible(&self) -> bool;
}

impl Evaluable for Statement {
    fn is_reducible(&self) -> bool {
        match self {
            &ExprNode(ref expr) => expr.is_reducible(),
            &FuncDefNode(_) => true,
        }
    }
}

impl Evaluable for Expression {
    fn is_reducible(&self) -> bool {
        match *self {
            Null => false,
            StringLiteral(_) => false,
            Lambda(_) => false,
            Number(_) => false,
            ListLiteral(ref items) => {
                items.iter().any(|x| x.is_reducible())
            }
            StructLiteral(ref items) => {
                items.iter().any(|pair| pair.1.is_reducible())
            }
            _ => true,
        }
    }
}

impl Expression {
    fn is_truthy(&self) -> bool {
        match *self {
            Null => false,
            StringLiteral(ref s) if **s == "" => false,
            Number(n) if n == 0.0 => false,
            _ => true,
        }
    }
}

fn is_assignment(op: &BinOp) -> bool {
    use self::BinOp::*;
    match *op {
        Assign | AddAssign | SubAssign |
        MulAssign | DivAssign => true,
        _ => false,
    }
}

impl<'a> Evaluator<'a> {
    fn reduction_loop(&mut self, mut node: Statement) -> Statement {
        loop {
            node = self.step(node);
            if !node.is_reducible() {
                break;
            }
        }
        node
    }

    fn step(&mut self, node: Statement) -> Statement {
        let mut trace = String::new();
        if self.trace_evaluation {
            trace.push_str(&format!("Step: {:?}", node));
        }

        let (new_node, side_effect) = self.reduce_astnode(node);

        if self.trace_evaluation {
            trace.push_str(&format!(" ➜ {:?}", new_node));
        }
        if let Some(s) = side_effect {
            if self.trace_evaluation {
                trace.push_str(&format!(" | side-effect: {:?}", s));
            }
            self.perform_side_effect(s);
        }
        if self.trace_evaluation {
            println!("{}", trace);
        }
        new_node
    }

    fn perform_side_effect(&mut self, side_effect: SideEffect) {
        use self::SideEffect::*;
        match side_effect {
            Print(s) => {
                write!(self.stdout, "{}\n", s).unwrap();
                match self.stdout.flush() {
                    Ok(_) => (),
                    Err(_) => println!("Could not flush stdout"),
                };
            }
            AddBinding(var, value) => {
                self.add_binding((*var).clone(), value);
            },
        }
    }

    fn reduce_astnode(&mut self, node: Statement) -> Reduction<Statement> {
        match node {
            ExprNode(expr) => {
                if expr.is_reducible() {
                    let (new_expr, side_effect) = self.reduce_expr(expr);
                    (ExprNode(new_expr), side_effect)
                } else {
                    (ExprNode(expr), None)
                }
            }
            FuncDefNode(func) => {
                let name = func.prototype.name.clone();
                let reduced_value = ReducedValue::Lambda(func.clone());
                let binding = Some(SideEffect::AddBinding(name, reduced_value));
                (ExprNode(Expression::Lambda(func)), binding)
            }
        }
    }

    //TODO I probably want another Expression variant that holds a ReducedValue
    fn reduce_expr(&mut self, expression: Expression) -> Reduction<Expression> {
        match expression {
            Null => (Null, None),
            e @ StringLiteral(_) => (e, None),
            e @ Number(_) => (e, None),
            e @ Lambda(_) => (e, None),
            Variable(ref var) => {
                match self.lookup_binding(var).map(|x| x.into()) {
                    None => (Null, None),
                    Some(expr) => (expr, None),
                }
            }
            BinExp(op, mut left, mut right) => {
                if right.is_reducible() {
                    let mut side_effect = None;
                    take_mut::take(right.as_mut(), |expr| { let (a, b) = self.reduce_expr(expr); side_effect = b; a});
                    return (BinExp(op, left, right), side_effect);
                }

                if let BinOp::Assign = op {
                    return match *left {
                        Variable(var) => {
                            let reduced_value: ReducedValue = ReducedValue::from(*right);
                            let binding = SideEffect::AddBinding(var, reduced_value);
                            (Null, Some(binding))
                        },
                        _ => (Null, None)
                    };
                }

                if is_assignment(&op) {
                    use self::BinOp::*;
                    let new_op = match op {
                        AddAssign => Add,
                        SubAssign => Sub,
                        MulAssign => Mul,
                        DivAssign => Div,
                        _ => unreachable!(),
                    };

                    let reduction =
                        BinExp(BinOp::Assign,
                               Box::new(*left.clone()),
                               Box::new(BinExp(new_op, left, right))
                              );

                    return (reduction, None);
                }

                if left.is_reducible() {
                    let mut side_effect = None;
                    take_mut::take(left.as_mut(), |expr| { let (a, b) = self.reduce_expr(expr); side_effect = b; a});
                    (BinExp(op, left, right), side_effect)
                } else {
                    (self.reduce_binop(op, *left, *right), None) //can assume both arguments are maximally reduced
                }
            }
            Call(callable, mut args) => {
                let mut f = true;
                for arg in args.iter_mut() {
                    if arg.is_reducible() {
                        take_mut::take(arg, |arg| self.reduce_expr(arg).0);
                        f = false;
                        break;
                    }
                }
                if f {
                    self.reduce_call(callable, args)
                } else {
                    (Call(callable, args), None)
                }
            }
            While(test, body) => {
                let mut block = VecDeque::from(body.clone());
                block.push_back(While(test.clone(), body.clone()));
                let reduction = Conditional(test, Box::new(Block(block)), None); 
                (reduction, None)
            }
            Conditional(box test, then_block, else_block) => {
                if test.is_reducible() {
                    let (new_test, new_effect) = self.reduce_expr(test);
                    (Conditional(Box::new(new_test), then_block, else_block), new_effect)
                } else {
                    if test.is_truthy() {
                        (*then_block, None)
                    } else {
                        match else_block {
                            Some(box expr) => (expr, None),
                            None => (Null, None),
                        }
                    }
                }
            }
            Block(mut exprs) => {
                let first = exprs.pop_front();
                match first {
                    None => (Null, None),
                    Some(expr) => {
                        if exprs.len() == 0 {
                            (expr, None)
                        } else {
                            if expr.is_reducible() {
                                let (new, side_effect) = self.reduce_expr(expr);
                                exprs.push_front(new);
                                (Block(exprs), side_effect)
                            } else {
                                (Block(exprs), None)
                            }
                        }
                    }
                }
            }
            Index(mut expr, mut index_expr) => {
                if index_expr.is_reducible() {
                    let mut side_effect = None;
                    take_mut::take(index_expr.as_mut(), |expr| { let (a, b) = self.reduce_expr(expr); side_effect = b; a});
                    return (Index(expr, index_expr), side_effect)
                }

                if expr.is_reducible() {
                    let mut side_effect = None;
                    take_mut::take(expr.as_mut(), |expr| { let (a, b) = self.reduce_expr(expr); side_effect = b; a});
                    return (Index(expr, index_expr), side_effect);
                }

                match (*expr, *index_expr) {
                    (ListLiteral(list_items), Number(n)) => {
                        let indexed_expr = get_indexer(n).and_then(|i| list_items.get(i));
                        if let Some(e) = indexed_expr {
                            (e.clone(), None)
                        } else {
                            (Null, None)
                        }
                    }
                    (StructLiteral(items), StringLiteral(s)) => {
                        for item in items {
                            if s == item.0 {
                                return (item.1.clone(), None); //TODO this is hella inefficient
                            }
                        }
                        (Null, None)
                    },
                    _ => (Null, None)
                }
            }
            ListLiteral(mut exprs) => {
                let mut side_effect = None;
                for expr in exprs.iter_mut() {
                    if expr.is_reducible() {
                        take_mut::take(expr, |expr| {
                            let (a, b) = self.reduce_expr(expr);
                            side_effect = b;
                            a
                        });
                        break;
                    }
                }
                (ListLiteral(exprs), side_effect)
            },

            StructLiteral(mut items) => {
                let mut side_effect = None;
                for pair in items.iter_mut() {
                    if pair.1.is_reducible() {
                        take_mut::take(pair, |pair| {
                            let (name, expr) = pair;
                            let (a, b) = self.reduce_expr(expr);
                            side_effect = b;
                            (name, a)
                        });
                        break;
                    }
                }

                (StructLiteral(items), side_effect)
            }
        }
    }

    fn reduce_binop(&mut self, op: BinOp, left: Expression, right: Expression) -> Expression {
        use self::BinOp::*;
        let truthy = Number(1.0);
        let falsy = Null;
        match (op, left, right) {
            (Add, Number(l), Number(r)) => Number(l + r),
            (Add, StringLiteral(s1), StringLiteral(s2)) => StringLiteral(Rc::new(format!("{}{}", *s1, *s2))),
            (Add, StringLiteral(s1), Number(r)) => StringLiteral(Rc::new(format!("{}{}", *s1, r))),
            (Add, Number(l), StringLiteral(s1)) => StringLiteral(Rc::new(format!("{}{}", l, *s1))),
            (Sub, Number(l), Number(r)) => Number(l - r),
            (Mul, Number(l), Number(r)) => Number(l * r),
            (Div, Number(l), Number(r)) if r != 0.0 => Number(l / r),
            (Mod, Number(l), Number(r)) => Number(l % r),
            (Less, Number(l), Number(r)) => if l < r { truthy } else { falsy },
            (LessEq, Number(l), Number(r)) => if l <= r { truthy } else { falsy },
            (Greater, Number(l), Number(r)) => if l > r { truthy } else { falsy },
            (GreaterEq, Number(l), Number(r)) => if l >= r { truthy } else { falsy },
            (Equal, Number(l), Number(r)) => if l == r { truthy } else { falsy },
            (Equal, Null, Null) => truthy,
            (Equal, StringLiteral(s1), StringLiteral(s2)) => if s1 == s2 { truthy } else { falsy },
            (Equal, _, _) => falsy,
            _ => falsy,
        }
    }

    fn reduce_call(&mut self, callable: Callable, arguments: Vec<Expression>) -> Reduction<Expression> {
        if let Some(res) = handle_builtin(&callable, &arguments) {
            return res;
        }

        let function = match callable {
            Callable::Lambda(func) => func.clone(),
            Callable::NamedFunction(name) => {
                match self.lookup_binding(&*name) {
                    Some(ReducedValue::Lambda(func)) => func,
                    _ => return (Null, None),
                }
            }
        };
        if function.prototype.parameters.len() != arguments.len() {
            return (Null, None);
        }

        let mut evaluator = Evaluator::new(Some(self));
        for (binding, expr) in function.prototype.parameters.iter().zip(arguments.iter()) {
            evaluator.add_binding((**binding).clone(), expr.clone().into());
        }

        let nodes = function.body.iter().map(|node| node.clone());
        let mut retval = ExprNode(Null);
        for n in nodes {
            retval = evaluator.reduction_loop(n);
        }

        match retval {
            ExprNode(expr) => (expr, None),
            FuncDefNode(_) => panic!("This should never happen! A maximally-reduced node\
            should never be a function definition!")
        }
    }
}

fn handle_builtin(callable: &Callable, arguments: &Vec<Expression>) -> Option<Reduction<Expression>> {
    let name: &str = match *callable {
        Callable::NamedFunction(ref name) => *&name,
        _ => return None,
    };

    match name {
        "print" => {
            let mut s = String::new();
            for arg in arguments {
                s.push_str(&format!("{} ", arg));
            }
            return Some((Null, Some(SideEffect::Print(s))));
        },
        _ => None
    }
}
