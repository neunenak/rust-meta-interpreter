use std::collections::HashMap;

use parser::AST;
use parser::AST::*;

pub struct Environment(pub HashMap<String, Box<AST>>);
type EvalResult = (AST, Environment);

impl Environment {
    pub fn new() -> Environment {
        Environment(HashMap::new())
    }

    fn add_binding(&mut self, name: String, binding: Box<AST>) {
        match *self {
            Environment(ref mut hash_map) => hash_map.insert(name, binding)
        };
    }

    fn lookup_binding(&mut self, name: &String) -> Option<&Box<AST>> {
        match *self {
            Environment(ref mut hash_map) => hash_map.get(name)
        }
    }
}

pub fn evaluate(ast: AST, env: Environment) -> String {

    let (mut reduced_ast, final_env) = reduce((ast, env));

    match reduced_ast {
        DoNothing => "".to_string(),
        Number(n) => return format!("{}", n),
        LangString(s) => return format!("\"{}\"", s),
        Null => "null".to_string(),
        _ => return "not implemented".to_string()
    }
}

fn reduce(evr: EvalResult) -> EvalResult {
    let (mut ast, mut env) = evr;

    match ast {
        Name(name) => {
            match env.lookup_binding(&name) {
                Some(_) => {
                    (Null, env)
                },
                None => (Null, env)
            }
        },

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

        Binding(name, binding) => {
            env.add_binding(name, binding);
            (DoNothing, env)
        },

        other_ast => (other_ast, env)
    }
}
