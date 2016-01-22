use std::collections::HashMap;
use parser::{AST, ASTNode, Expression};

struct Varmap {
    map: HashMap<String, Expression>
}

impl Varmap {
    fn new() -> Varmap {
        let mut map = HashMap::new();
        map.insert("a".to_string(), Expression::Number(10.0));
        Varmap { map: map }
    }

    fn add_binding(&mut self, var: String, value: Expression) {
        self.map.insert(var, value);
    }

    fn lookup_binding(&mut self, var: String) -> Option<&Expression> {
        self.map.get(&var)
    }
}

pub struct Evaluator {
    varmap: Varmap
}

impl Evaluator {

    pub fn new() -> Evaluator {
        Evaluator { varmap: Varmap::new() }
    }

    pub fn run(&mut self, ast: AST) -> Vec<String> {
        ast.into_iter().map(|astnode| {
            self.reduce_node(astnode)
        }).collect()
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
            _ => unimplemented!(),
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
    fn reduce_node(&mut self, mut node: ASTNode) -> String {
        loop {
            node = self.step(node);
            if !node.is_reducible() {
                break
            }
        }

        format!("{}", node) //TODO make better
    }

    fn step(&mut self, node: ASTNode) -> ASTNode {
        println!("Doing one step, current node is {:?}", node);
        self.reduce(node)
    }

    fn reduce(&mut self, node: ASTNode) -> ASTNode { //TODO swap the names of this and reduce_node
        use parser::ASTNode::*;
        match node {
            ExprNode(expr) => {
                if expr.is_reducible() {
                    ExprNode(self.reduce_expr(expr))
                } else {
                    ExprNode(expr)
                }
            },
            _ => unimplemented!(),
        }
    }

    fn reduce_expr(&mut self, expression: Expression) -> Expression {
        use parser::Expression::*;
        match expression {
            e@StringLiteral(_) => e,
            e@Number(_) => e,
            Variable(var) => {
                match self.varmap.lookup_binding(var) {
                    None => Null,
                    Some(expr) => expr.clone()
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
                            self.varmap.add_binding(var, right);
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
            _ => unimplemented!(),
        }
    }

    fn reduce_binop(&mut self, op: String, left: Expression, right: Expression) -> Expression {
        use parser::Expression::*;
        match &op[..] {
            "+" => match (left, right) {
                (Number(l), Number(r)) => Number(l + r),
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
                    self.varmap.add_binding(var, right);
                    Null
                },
                _ => Null,
            },
            _ => unimplemented!(),
        }
    }
}
