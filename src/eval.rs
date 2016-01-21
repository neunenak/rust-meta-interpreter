use std::collections::HashMap;
use parser::{AST, ASTNode, Expression};

struct Varmap {
    map: HashMap<String, Expression>
}

impl Varmap {
    fn new() -> Varmap {
        Varmap { map: HashMap::new() }
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

        format!("{:?}", node) //TODO make better
    }

    fn step(&mut self, node: ASTNode) -> ASTNode {
        println!("Doing one step, current node is {:?}", node);
        self.reduce(node)
    }

    fn reduce(&mut self, node: ASTNode) -> ASTNode {
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
            Variable(var) => Number(20.0),
            _ => unimplemented!(),
        }


    }
}
