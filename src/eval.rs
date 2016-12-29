extern crate take_mut;

use std::collections::HashMap;
use parser::{AST, ASTNode, Expression, Function};

type Reduction<T> = (T, Option<SideEffect>);

#[derive(Debug)]
enum SideEffect {
    Print(String),
    Bundle(Vec<SideEffect>),
    AddBinding(String, Expression),
}

struct EnvFrame {
    functions: HashMap<String, Function>,
    variables: HashMap<String, Expression>,
}

impl EnvFrame {
    fn new() -> EnvFrame {
        EnvFrame {
            functions: HashMap::new(),
            variables: HashMap::new(),
        }
    }
}

pub struct Evaluator {
    frames: Vec<EnvFrame>,
}

impl Evaluator {
    pub fn new() -> Evaluator {
        Evaluator {
            frames: vec!(EnvFrame::new()),
        }
    }

    pub fn run(&mut self, ast: AST) -> Vec<String> {
        ast.into_iter()
            .map(|astnode| self.reduce(astnode))
            .collect()
    }

    fn add_binding(&mut self, var: String, value: Expression) {
        match self.frames.last_mut() {
            Some(frame) => frame.variables.insert(var, value),
            None => panic!("Evaluator should ensure that frames always has at least one element"),
        };
    }

    fn lookup_binding(&self, var: String) -> Option<Expression> {
        for frame in self.frames.iter().rev() {
            match frame.variables.get(&var) {
                None => (),
                Some(expr) => return Some(expr.clone()),
            }
        }
        None
    }

    fn add_function(&mut self, name: String, function: Function) {
        match self.frames.last_mut() {
            Some(frame) => frame.functions.insert(name, function),
            None => panic!("Evaluator should ensure that frames always has at least one element"),
        };
    }

    fn lookup_function(&self, name: String) -> Option<Function> {
        for frame in self.frames.iter().rev() {
            match frame.functions.get(&name) {
                None => (),
                Some(function) => return Some(function.clone()),
            }
        }
        None
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
            &FuncNode(_) => true,
        }
    }
}

impl Evaluable for Expression {
    fn is_reducible(&self) -> bool {
        use parser::Expression::*;
        match *self {
            Null => false,
            StringLiteral(_) => false,
            Number(_) => false,
            _ => true,
        }
    }
}

impl Evaluator {
    fn reduce(&mut self, mut node: ASTNode) -> String {
        loop {
            node = self.step(node);
            if !node.is_reducible() {
                break;
            }
        }

        format!("{}", node)
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
            Bundle(l) => {
                for side_effect in l {
                    self.perform_side_effect(side_effect);
                }
            }
            AddBinding(var, value) => {
                self.add_binding(var, value);
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
            FuncNode(func) => {
                let fn_name = func.prototype.name.clone();
                self.add_function(fn_name, func);
                (ExprNode(Expression::Null), None)
            }
        }
    }

    fn reduce_expr(&mut self, expression: Expression) -> Reduction<Expression> {
        use parser::Expression::*;
        match expression {
            Null => (Null, None),
            e @ StringLiteral(_) => (e, None),
            e @ Number(_) => (e, None),
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
                        _ => return (Null, None)
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
            Conditional(_, _, _) => unimplemented!(),
        }
    }

    fn reduce_binop(&mut self, op: String, left: Expression, right: Expression) -> Expression {
        use parser::Expression::*;
        match &op[..] {
            "+" => {
                match (left, right) {
                    (Number(l), Number(r)) => Number(l + r),
                    (StringLiteral(s1), StringLiteral(s2)) => {
                        StringLiteral(format!("{}{}", s1, s2))
                    }
                    _ => Null,
                }
            }
            "-" => {
                match (left, right) {
                    (Number(l), Number(r)) => Number(l - r),
                    _ => Null,
                }
            }
            "*" => {
                match (left, right) {
                    (Number(l), Number(r)) => Number(l * r),
                    _ => Null,
                }
            }
            "/" => {
                match (left, right) {
                    (Number(l), Number(r)) if r != 0.0 => Number(l / r),
                    _ => Null,
                }
            }
            "%" => {
                match (left, right) {
                    (Number(l), Number(r)) => Number(l % r),
                    _ => Null,
                }
            }
            _ => Null,
        }
    }

    fn reduce_call(&mut self,
                   name: String,
                   arguments: Vec<Expression>)
                   -> Reduction<Expression> {
        use parser::Expression::*;

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

        let mut frame = EnvFrame::new();
        for (binding, expr) in function.prototype.parameters.iter().zip(arguments.iter()) {
            frame.variables.insert(binding.clone(), expr.clone());
        }

        self.frames.push(frame);
        let mut retval = Null;
        let mut side_effects = Vec::new();
        for expr in function.body.iter() {
            retval = expr.clone();
            while retval.is_reducible() {
                let r = self.reduce_expr(retval);
                retval = r.0;
                if let Some(s) = r.1 {
                    side_effects.push(s);
                }
            }
        }

        self.frames.pop();
        (retval, Some(SideEffect::Bundle(side_effects)))
    }
}
