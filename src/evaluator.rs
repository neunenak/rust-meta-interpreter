use parser::AST;

struct Evaluator {
    ast: AST
}

pub type EvaluatorResult<T> = Result<T, EvaluatorError>;

struct EvaluatorError {
    err: String
}

impl Evaluator {
    pub fn run(self) -> String {
        match reduce_full(self.ast) {
            Err(e) => format!("{}", e.err),
            Ok(ast) => format!("{}", ast)
        }
    }
}

impl AST {
    fn can_reduce(&self) -> bool {
        use parser::AST::*;
        match self {
            &Number(_) => false,
            &Name(_) => false,
            _ => true
        }
    }
}

fn reduce_full(ast: AST) -> EvaluatorResult<AST> {
    let mut ast = ast;
    while ast.can_reduce() {
        ast = try!(reduce_step(ast));
    }

    Ok(ast)
}

fn reduce_step(ast: AST) -> EvaluatorResult<AST> {
    use parser::AST::*;
    match ast {
        BinOp(left, op, right) => {
            let left = try!(reduce_full(*left));
            let op = try!(reduce_full(*op));
            let right = try!(reduce_full(*right));
            match (left, op, right) {
                (Number(l), Name(op), Number(r)) => {
                    match &op[..] {
                        "+" => Ok(Number(l + r)),
                        "-" => Ok(Number(l - r)),
                        "*" => Ok(Number(l * r)),
                        "/" => {
                            if r == 0.0 {
                                Err(EvaluatorError { err: format!("Divide by zero") })
                            } else {
                                Ok(Number(l / r))
                            }
                        },
                        _ => Err(EvaluatorError { err: format!("Bad BinOp operator") })
                    }
                },
                _ => Err(EvaluatorError { 
                    err: format!("Bad arguments for BinOp")
                })
            }
        },
        Number(_) => Ok(ast),
        Name(_) => Ok(ast),

    }
}


pub fn evaluate(ast: AST) -> String {
    let ev = Evaluator { ast: ast };
    ev.run()
}
