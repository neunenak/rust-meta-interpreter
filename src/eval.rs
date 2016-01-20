use parser::{AST, ASTNode, Expression};

pub struct Evaluator {
    data: bool,
}

impl Evaluator {

    pub fn new() -> Evaluator {
        Evaluator { data: false }
    }

    pub fn run(&mut self, ast: AST) -> Vec<String> {
        ast.into_iter().map(|astnode| {
            self.reduce_node(astnode)
        }).collect()
    }
}

trait Evaluable {
    type Output;
    fn is_reducible(&self) -> bool;
    fn reduce(self) -> Self::Output;
}

impl Evaluable for ASTNode {
    type Output = ASTNode;

    fn is_reducible(&self) -> bool {
        use parser::ASTNode::*;
        match self {
            &ExprNode(ref expr) => expr.is_reducible(),
            _ => unimplemented!(),
        }
    }

    fn reduce(self) -> Self::Output {
        use parser::ASTNode::*;
        match self {
            ExprNode(expr) => {
                if expr.is_reducible() {
                    ExprNode(expr.reduce())
                } else {
                    ExprNode(expr)
                }
            },
            _ => unimplemented!(),
        }
    }
}

impl Evaluable for Expression {
    type Output = Expression;

    fn is_reducible(&self) -> bool {
        use parser::Expression::*;
        match *self {
            StringLiteral(_) => false,
            Number(_) => false,
            _ => true,
        }
    }

    fn reduce(self) -> Self::Output {
        use parser::Expression::*;
        match self {
            e@StringLiteral(_) => e,
            e@Number(_) => e,
            Variable(var) => Number(20.0),
            _ => unimplemented!(),
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
        node.reduce()
    }
}
