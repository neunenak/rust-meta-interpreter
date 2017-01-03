extern crate take_mut;

use std::collections::HashMap;
use parser::{AST, ASTNode, Expression, Function};

type Reduction<T> = (T, Option<SideEffect>);

#[derive(Debug)]
enum SideEffect {
    Print(String),
    AddBinding(String, Expression),
    AddFunctionBinding(Function),
}

pub struct Evaluator<'a> {
    parent: Option<&'a Evaluator<'a>>,
    functions: HashMap<String, Function>,
    variables: HashMap<String, Expression>,
}

impl<'a> Evaluator<'a> {
    pub fn new(parent: Option<&'a Evaluator>) -> Evaluator<'a> {
        Evaluator {
            functions: HashMap::new(),
            variables: HashMap::new(),
            parent: parent,
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

    fn lookup_binding(&self, var: String) -> Option<Expression> {
        match self.variables.get(&var) {
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

    fn lookup_function(&self, name: String) -> Option<Function> {
        match self.functions.get(&name) {
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

impl Evaluable for ASTNode {
    fn is_reducible(&self) -> bool {
        use parser::ASTNode::*;
        match self {
            &ExprNode(ref expr) => expr.is_reducible(),
            &FuncDefNode(_) => true,
        }
    }
}

impl Evaluable for Expression {
    fn is_reducible(&self) -> bool {
        use parser::Expression::*;
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
        use parser::Expression::*;
        match *self {
            Null => false,
            StringLiteral(ref s) if s == "" => false,
            Number(0.0) => false,
            _ => true,
        }
    }
}

impl<'a> Evaluator<'a> {
    fn reduction_loop(&mut self, mut node: ASTNode) -> ASTNode {
        loop {
            node = self.step(node);
            if !node.is_reducible() {
                break;
            }
        }
        node
    }

    fn step(&mut self, node: ASTNode) -> ASTNode {
        let (new_node, side_effect) = self.reduce_astnode(node);
        if let Some(s) = side_effect {
            self.perform_side_effect(s);
        }
        new_node
    }

    fn perform_side_effect(&mut self, side_effect: SideEffect) {
        use self::SideEffect::*;
        match side_effect {
            Print(s) => println!("{}", s),
            AddBinding(var, value) => {
                self.add_binding(var, value);
            },
            AddFunctionBinding(function) => {
                self.add_function(function.prototype.name.clone(), function);
            }
        }
    }

    fn reduce_astnode(&mut self, node: ASTNode) -> Reduction<ASTNode> {
        use parser::ASTNode::*;
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
        use parser::Expression::*;
        match expression {
            Null => (Null, None),
            e @ StringLiteral(_) => (e, None),
            e @ Number(_) => (e, None),
            e @ Lambda(_) => (e, None),
            Variable(var) => {
                match self.lookup_binding(var) {
                    None => (Null, None),
                    Some(expr) => (expr, None),
                }
            }
            BinExp(op, box left, box right) => {
                if right.is_reducible() {
                    let new = self.reduce_expr(right);
                    return (BinExp(op, Box::new(left), Box::new(new.0)), new.1);
                }

                // special case for variable assignment
                if op == "=" {
                    match left {
                        Variable(var) => {
                            let binding = SideEffect::AddBinding(var, right);
                            return (Null, Some(binding));
                        }
                        _ => return (Null, None),
                    }
                }

                if left.is_reducible() {
                    let new = self.reduce_expr(left);
                    (BinExp(op, Box::new(new.0), Box::new(right)), new.1)
                } else {
                    (self.reduce_binop(op, left, right), None) //can assume both arguments are maximally reduced
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

    fn reduce_binop(&mut self, op: String, left: Expression, right: Expression) -> Expression {
        use parser::Expression::*;
        let truthy = Number(1.0);
        let falsy = Null;
        match (&op[..], left, right) {
            ("+", Number(l), Number(r)) => Number(l + r),
            ("+", StringLiteral(s1), StringLiteral(s2)) => StringLiteral(format!("{}{}", s1, s2)),
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

    fn reduce_call(&mut self, name: String, arguments: Vec<Expression>) -> Reduction<Expression> {
        use parser::Expression::*;
        use parser::ASTNode::*;

        // ugly hack for now
        if name == "print" {
            let mut s = String::new();
            for arg in arguments {
                s.push_str(&format!("{}\n", arg));
            }
            return (Null, Some(SideEffect::Print(s)));
        }

        let function = match self.lookup_function(name) {
            Some(func) => func,
            None => return (Null, None),
        };

        if function.prototype.parameters.len() != arguments.len() {
            return (Null, None);
        }

        let mut evaluator = Evaluator::new(Some(self));
        for (binding, expr) in function.prototype.parameters.iter().zip(arguments.iter()) {
            evaluator.add_binding(binding.clone(), expr.clone());
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
