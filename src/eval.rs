extern crate take_mut;

use std::collections::HashMap;
use std::collections::VecDeque;
use parser::{AST, Statement, Expression, Function};
use std::rc::Rc;
use std::io::{Write, Stdout, BufWriter};

use parser::Expression::*;
use parser::Statement::*;

type Reduction<T> = (T, Option<SideEffect>);

#[derive(Debug)]
enum SideEffect {
    Print(String),
    AddBinding(Rc<String>, Expression),
    AddFunctionBinding(Function),
}

pub struct Evaluator<'a> {
    parent: Option<&'a Evaluator<'a>>,
    functions: HashMap<String, Function>,
    variables: HashMap<String, Expression>,
    stdout: BufWriter<Stdout>,
    pub trace_evaluation: bool,
}

impl<'a> Evaluator<'a> {
    pub fn new_with_opts(parent: Option<&'a Evaluator>, trace_evaluation: bool) -> Evaluator<'a> {
        let mut e = Evaluator::new(parent);
        e.trace_evaluation = trace_evaluation;
        e
    }
    pub fn new(parent: Option<&'a Evaluator>) -> Evaluator<'a> {
        Evaluator {
            functions: HashMap::new(),
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

    fn add_binding(&mut self, var: String, value: Expression) {
        self.variables.insert(var, value);
    }

    fn lookup_binding(&self, var: &str) -> Option<Expression> {
        match self.variables.get(var) {
            Some(expr) => Some(expr.clone()),
            None => match self.parent {
                Some(env) => env.lookup_binding(var),
                None => None
            }
        }
    }

    fn add_function(&mut self, name: String, function: Function) {
        self.functions.insert(name, function);
    }

    fn lookup_function(&self, name: &str) -> Option<Function> {
        match self.functions.get(name) {
            Some(func) => Some(func.clone()),
            None => match self.parent {
                Some(env) => env.lookup_function(name),
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
            _ => true,
        }
    }
}

impl Expression {
    fn is_truthy(&self) -> bool {
        match *self {
            Null => false,
            StringLiteral(ref s) if **s == "" => false,
            Number(0.0) => false,
            _ => true,
        }
    }
}

fn is_assignment(op: &str) -> bool {
    match op {
        "=" | "+=" | "-=" |
        "*=" | "/=" => true,
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
            }
            AddBinding(var, value) => {
                self.add_binding((*var).clone(), value);
            },
            AddFunctionBinding(function) => {
                self.add_function((*function.prototype.name).clone(), function);
            }
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
                let binding = Some(SideEffect::AddFunctionBinding(func.clone()));
                (ExprNode(Expression::Lambda(func)), binding)
            }
        }
    }

    fn reduce_expr(&mut self, expression: Expression) -> Reduction<Expression> {
        match expression {
            Null => (Null, None),
            e @ StringLiteral(_) => (e, None),
            e @ Number(_) => (e, None),
            e @ Lambda(_) => (e, None),
            Variable(ref var) => {
                match self.lookup_binding(var) {
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

                if *op == "=" {
                    return match *left {
                        Variable(var) => {
                            let binding = SideEffect::AddBinding(var, *right);
                            (Null, Some(binding))
                        },
                        _ => (Null, None)
                    };
                }

                if is_assignment(&*op) {
                    let new_op = Rc::new(String::from(match &op[..] {
                        "+=" => "+", 
                        "-=" => "-",
                        "*=" => "*",
                        "/=" => "/",
                        _ => unreachable!(),
                    }));

                    let reduction =
                        BinExp(Rc::new(String::from("=")),
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
            Call(name, mut args) => {
                let mut f = true;
                for arg in args.iter_mut() {
                    if arg.is_reducible() {
                        take_mut::take(arg, |arg| self.reduce_expr(arg).0);
                        f = false;
                        break;
                    }
                }
                if f {
                    self.reduce_call(name, args)
                } else {
                    (Call(name, args), None)
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
        }
    }

    fn reduce_binop(&mut self, op: Rc<String>, left: Expression, right: Expression) -> Expression {
        let truthy = Number(1.0);
        let falsy = Null;
        match (&op[..], left, right) {
            ("+", Number(l), Number(r)) => Number(l + r),
            ("+", StringLiteral(s1), StringLiteral(s2)) => StringLiteral(Rc::new(format!("{}{}", *s1, *s2))),
            ("+", StringLiteral(s1), Number(r)) => StringLiteral(Rc::new(format!("{}{}", *s1, r))),
            ("+", Number(l), StringLiteral(s1)) => StringLiteral(Rc::new(format!("{}{}", l, *s1))),
            ("-", Number(l), Number(r)) => Number(l - r),
            ("*", Number(l), Number(r)) => Number(l * r),
            ("/", Number(l), Number(r)) if r != 0.0 => Number(l / r), 
            ("%", Number(l), Number(r)) => Number(l % r),
            ("<", Number(l), Number(r)) => if l < r { truthy } else { falsy },
            ("<=", Number(l), Number(r)) => if l <= r { truthy } else { falsy },
            (">", Number(l), Number(r)) => if l > r { truthy } else { falsy },
            (">=", Number(l), Number(r)) => if l >= r { truthy } else { falsy },
            ("==", Number(l), Number(r)) => if l == r { truthy } else { falsy },
            ("==", Null, Null) => truthy,
            ("==", StringLiteral(s1), StringLiteral(s2)) => if s1 == s2 { truthy } else { falsy },
            ("==", _, _) => falsy,
            _ => falsy,
        }
    }

    fn reduce_call(&mut self, name: Rc<String>, arguments: Vec<Expression>) -> Reduction<Expression> {
        if let Some(res) = handle_builtin(&*name, &arguments) {
            return res;
        }

        let function = match self.lookup_function(&*name) {
            Some(func) => func,
            None => return (Null, None),
        };

        if function.prototype.parameters.len() != arguments.len() {
            return (Null, None);
        }

        let mut evaluator = Evaluator::new(Some(self));
        for (binding, expr) in function.prototype.parameters.iter().zip(arguments.iter()) {
            evaluator.add_binding((**binding).clone(), expr.clone());
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

fn handle_builtin(name: &str, arguments: &Vec<Expression>) -> Option<Reduction<Expression>> {
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
