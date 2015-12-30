use parser::AST;

struct Evaluator {
    ast: AST
}

impl Evaluator {
    pub fn run(&mut self) -> String {
        while self.ast.can_reduce() {
            self.ast.reduce();
        }

        format!("{}", self.ast)
    }
}

impl AST {
    fn can_reduce(&self) -> bool {
        false
    }

    fn reduce(&mut self) {

    }
}

pub fn evaluate(ast: AST) -> String {
    let mut ev = Evaluator { ast: ast };
    ev.run()
}
