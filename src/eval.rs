use parser::{AST, ASTNode};

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

impl Evaluator {
    fn reduce_node(&mut self, mut node: ASTNode) -> String {
        loop {
            let (newnode, finished)  = self.step(node);
            node = newnode;
            if finished {
                break
            }
        }

        format!("{:?}", node) //TODO make better
    }

    fn step(&mut self, node: ASTNode) -> (ASTNode, bool) {
        use parser::ASTNode::*;
        use parser::Expression::*;
        println!("Doing one step");
        match node {
            n@ExprNode(StringLiteral(_)) => (n, true),
            n@ExprNode(Number(_)) => (n, true),
            _ => unimplemented!(),
        }
    }
}
