use std::collections::HashMap;
use parser::{AST, ASTNode, Expression, Function};

struct Varmap {
    map: HashMap<String, Expression>
}

impl Varmap {
    fn new() -> Varmap {
        Varmap { map: HashMap::new()}
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
        Evaluator { varmap: Varmap::new(),
                    funcmap: Funcmap::new(),
                    frames: Vec::new(),
        }
    }

    pub fn run(&mut self, ast: AST) -> Vec<String> {
        ast.into_iter().map(|astnode| {
            self.reduce(astnode)
        }).collect()
    }

    fn add_binding(&mut self, var: String, value: Expression) {
        match self.frames.last_mut() {
            Some(frame) => frame.map.insert(var, value),
            None => self.varmap.map.insert(var, value),
        };
    }

    fn lookup_binding(&mut self, var: String) -> Option<Expression> {
        for frame in self.frames.iter() {
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
            &FuncNode(_) =>  true,
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
                break
            }
        }

        format!("{}", node)
    }

    fn step(&mut self, node: ASTNode) -> ASTNode {
        self.reduce_astnode(node)
    }

    fn reduce_astnode(&mut self, node: ASTNode) -> ASTNode {
        use parser::ASTNode::*;
        match node {
            ExprNode(expr) => {
                if expr.is_reducible() {
                    ExprNode(self.reduce_expr(expr))
                } else {
                    ExprNode(expr)
                }
            },
            FuncNode(func) => {
                let fn_name = func.prototype.name.clone();
                self.add_function(fn_name, func);
                ExprNode(Expression::Null)
            },
        }
    }

    fn reduce_expr(&mut self, expression: Expression) -> Expression {
        use parser::Expression::*;
        match expression {
            Null => Null,
            e@StringLiteral(_) => e,
            e@Number(_) => e,
            Variable(var) => {
                match self.lookup_binding(var) {
                    None => Null,
                    Some(expr) => expr,
                }
            },
            BinExp(op, box left, box right) => {
                if right.is_reducible() {
                    let new = self.reduce_expr(right);
                    return BinExp(op, Box::new(left), Box::new(new));
                }

                //special case for variable assignment
                if op == "=" {
                    match left {
                        Variable(var) => {
                            self.add_binding(var, right);
                            return Null;
                        },
                        _ => ()
                    }
                }

                if left.is_reducible() {
                    let new = self.reduce_expr(left);
                    BinExp(op, Box::new(new), Box::new(right))
                } else {
                    self.reduce_binop(op, left, right) //can assume both arguments are maximally reduced
                }
            },
            Call(name, args) => self.reduce_call(name, args)
        }
    }

    fn reduce_binop(&mut self, op: String, left: Expression, right: Expression) -> Expression {
        use parser::Expression::*;
        match &op[..] {
            "+" => match (left, right) {
                (Number(l), Number(r)) => Number(l + r),
                (StringLiteral(s1), StringLiteral(s2)) => StringLiteral(format!("{}{}", s1, s2)),
                _ => Null,
            },
            "-" => match (left, right) {
                (Number(l), Number(r)) => Number(l - r),
                _ => Null,
            },
            "*" => match (left, right) {
                (Number(l), Number(r)) => Number(l * r),
                _ => Null,
            },
            "/" => match (left, right) {
                (Number(l), Number(r)) if r != 0.0 => Number(l / r),
                _ => Null,
            },
            "%" => match (left, right) {
                (Number(l), Number(r)) => Number(l % r),
                _ => Null,
            },
            "=" => match (left, right) {
                (Variable(var), right) => {
                    self.add_binding(var, right);
                    Null
                },
                _ => Null,
            },
            _ => Null,
        }
    }

    fn reduce_call(&mut self, name: String, arguments: Vec<Expression>) -> Expression {
        use parser::Expression::*;
        let function = match self.lookup_function(name) {
            Some(func) => func,
            None => return Null
        };

        if function.prototype.parameters.len() != arguments.len() {
            return Null
        }

        let mut frame: Varmap = Varmap::new();
        for (binding, expr) in function.prototype.parameters.iter().zip(arguments.iter()) {
            frame.map.insert(binding.clone(), expr.clone());
        }

        self.frames.push(frame);
        let mut retval = Null;
        for expr in function.body.iter() {
            retval = expr.clone();
            while retval.is_reducible() {
                retval = self.reduce_expr(retval);
            }
        }

        self.frames.pop();
        retval
    }
}
