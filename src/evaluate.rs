use std::collections::HashMap;

use parser::AST;
use parser::AST::*;

pub struct Environment(pub HashMap<String, AST>);
type EvalResult = (AST, Environment);

impl Environment {
    pub fn new() -> Environment {
        Environment(HashMap::new())
    }

    fn add_binding(&mut self, name: String, binding: AST) {
        match *self {
            Environment(ref mut hash_map) => hash_map.insert(name, binding)
        };
    }

    fn lookup_binding(&mut self, name: &String) -> Option<&AST> {
        match *self {
            Environment(ref mut hash_map) => hash_map.get(name)
        }
    }

    fn display(&self) {
        match *self {
            Environment(ref hash_map) =>
                for (var, binding) in hash_map {
                    println!("{} : {:?}", var, binding);
                }
        }

        println!("----");
    }
}

pub fn evaluate(ast: AST, env: Environment) -> (String, Environment) {

    let (mut reduced_ast, final_env) = reduce((ast, env));

    let output = match reduced_ast {
        DoNothing => "".to_string(),
        Number(n) => format!("{}", n),
        LangString(s) => format!("\"{}\"", s),
        Null => "null".to_string(),
        _ => "not implemented".to_string()
    };

    (output, final_env)
}

fn reduce(evr: EvalResult) -> EvalResult {
    let (mut ast, mut env) = evr;

    match ast {
        Name(name) => {
            let result = match env.lookup_binding(&name) {
                Some(binding) => match binding {
                    &DoNothing => DoNothing,
                    &Number(n) => Number(n),
                    &LangString(ref s) => LangString(s.clone()),
                    &Null => Null,
                    _ => panic!("Unreduced ast node for name: {:?}", name)
                },
                None => Null
            };

            (result, env)
        },

        Statements(stmts) => {
            let mut reduced_ast = DoNothing;
            let mut reduced_env = env;
            for stmt in stmts.into_iter() {
                let (new_ast, new_env) = reduce((stmt, reduced_env));
                reduced_env = new_env;
                reduced_ast = new_ast;

            }
            (reduced_ast, reduced_env)
        },

        Binding(name, binding) => {
            let unboxed_binding = *binding;
            let (evaluated_binding, mut evaluated_env) = reduce((unboxed_binding, env));

            evaluated_env.add_binding(name, evaluated_binding);
            (DoNothing, evaluated_env)
        },

        other_ast => (other_ast, env)
    }
}
