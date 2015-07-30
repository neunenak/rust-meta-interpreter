use std::collections::HashMap;

use parser::AST;
use parser::AST::*;

pub struct Environment(pub HashMap<String, AST>);
type EvalResult = (AST, Environment);

impl Environment {
    pub fn new() -> Environment {
        Environment(HashMap::new())
    }
}

pub fn evaluate(ast: AST, env: Environment) -> String {

    let (reduced_ast, final_env) = reduce((ast, env));

    match reduced_ast {
        DoNothing => "".to_string(),
        Number(n) => return format!("{}", n),
        LangString(s) => return format!("\"{}\"", s),
        _ => return "not implemented".to_string()
    }
}

fn reduce(evr: EvalResult) -> EvalResult {
    let (ast, env) = evr;

    match ast {
        Statements(stmts) => {
            let mut reduced_ast = DoNothing;
            let mut reduced_env = env;
            for stmt in stmts.into_iter() {
                let (a, b) = reduce((stmt, reduced_env));
                reduced_ast = a;
                reduced_env = b;
            }
            (reduced_ast, reduced_env)
        },
        other_ast => (other_ast, env)
    }
}
