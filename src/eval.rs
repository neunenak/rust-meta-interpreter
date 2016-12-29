extern crate take_mut;

use std::collections::HashMap;
use parser::{AST, ASTNode, Expression, Function};

#[derive(Debug)]
enum SideEffect {
    Print(String),
    Bundle(Vec<SideEffect>),
}

struct Varmap {
    map: HashMap<String, Expression>,
}

impl Varmap {
    fn new() -> Varmap {
        Varmap { map: HashMap::new() }
    }
}

struct Funcmap {
    map: HashMap<String, Function>,
}

impl Funcmap {
    fn new() -> Funcmap {
        let map = HashMap::new();
        Funcmap { map: map }
    }
}

pub struct Evaluator {
    varmap: Varmap,
    funcmap: Funcmap,
    frames: Vec<Varmap>,
}

impl Evaluator {
    pub fn new() -> Evaluator {
        Evaluator {
            varmap: Varmap::new(),
            funcmap: Funcmap::new(),
            frames: Vec::new(),
        }
    }

    pub fn run(&mut self, ast: AST) -> Vec<String> {
        ast.into_iter()
            .map(|astnode| self.reduce(astnode))
            .collect()
    }

    fn add_binding(&mut self, var: String, value: Expression) {
        match self.frames.last_mut() {
            Some(frame) => frame.map.insert(var, value),
            None => self.varmap.map.insert(var, value),
        };
    }

    fn lookup_binding(&mut self, var: String) -> Option<Expression> {
        for frame in self.frames.iter().rev() {
            match frame.map.get(&var) {
                None => (),
                Some(expr) => return Some(expr.clone()),
            }
        }

        self.varmap.map.get(&var).map(|expr| expr.clone())
    }

    fn add_function(&mut self, name: String, function: Function) {
        self.funcmap.map.insert(name, function);
    }

    fn lookup_function(&self, name: String) -> Option<Function> {
        self.funcmap.map.get(&name).map(|x| x.clone())
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
        }
    }

    fn reduce_astnode(&mut self, node: ASTNode) -> (ASTNode, Option<SideEffect>) {
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

    fn reduce_expr(&mut self, expression: Expression) -> (Expression, Option<SideEffect>) {
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
                            self.add_binding(var, right);
                            return (Null, None); //TODO variable binding should be an effect
                        }
                        _ => (),
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
            "=" => {
                match (left, right) {
                    (Variable(var), right) => {
                        self.add_binding(var, right);
                        Null
                    }
                    _ => Null,
                }
            }
            _ => Null,
        }
    }

    fn reduce_call(&mut self,
                   name: String,
                   arguments: Vec<Expression>)
                   -> (Expression, Option<SideEffect>) {
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

        let mut frame: Varmap = Varmap::new();
        for (binding, expr) in function.prototype.parameters.iter().zip(arguments.iter()) {
            frame.map.insert(binding.clone(), expr.clone());
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
