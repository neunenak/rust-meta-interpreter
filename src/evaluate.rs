use std::collections::HashMap;
use std::clone::Clone;

use parser::AST;
use parser::AST::*;

pub struct Environment(pub HashMap<String, AST>);
type EvalResult = (AST, Environment);

impl Environment {
    pub fn new() -> Environment {
        let mut map = HashMap::new();
        map.insert("true".to_string(), LangTrue);
        map.insert("false".to_string(), LangFalse);
        Environment(map)
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

    pub fn display(&self) {
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

    let mut reduction = (ast, env);
    while is_reducable(&reduction.0) {
        reduction = reduce(reduction);
    }

    let output = match reduction.0 {
        DoNothing => "".to_string(),
        Number(n) => format!("{}", n),
        LangString(s) => format!("\"{}\"", s),
        Null => "null".to_string(),
        LangFalse => "false".to_string(),
        LangTrue => "true".to_string(),
        other => format!("reducing {:?} not implemented", other)
    };

    (output, reduction.1)
}

fn is_reducable(ast: &AST) -> bool {
    match *ast {
        DoNothing => false,
        Number(_) => false,
        LangString(_) => false,
        Null => false,
        LangFalse => false,
        LangTrue => false,
        _ => true
    }
}

fn reduce(evr: EvalResult) -> EvalResult {
    let (ast, mut env) = evr;

    match ast {

        IfStatement(if_clause, then_clause, else_clause) => {
            let (condition, new_env) = (*if_clause, env);
            match condition {
                Null | LangFalse => match else_clause {
                    Some(cl) => (*cl, new_env),
                    None => (DoNothing, new_env)
                },

                _ => (*then_clause, new_env)
            }
        },

        WhileStatement(condition, body) => {
            let (continue_loop, env) = reduce((*condition.clone(), env));
            match continue_loop {
                Null | LangFalse => (DoNothing, env),
                _ => {
                    let (_, new_env) = reduce((*body.clone(), env));
                    (WhileStatement(condition, body), new_env)
                }
            }
        },

        BinOp(op, lhs, rhs) => {
            let (reduced_lhs, new_env) = reduce((*lhs, env));
            let (reduced_rhs, new_env2) = reduce((*rhs, new_env));
            let result: AST = reduce_binop(*op, reduced_lhs, reduced_rhs);
            (result, new_env2)
        },

        Name(name) => {
            let result = match env.lookup_binding(&name) {
                Some(binding) => match binding {
                    &DoNothing => DoNothing,
                    &LangTrue => LangTrue,
                    &LangFalse => LangFalse,
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

fn reduce_binop(op: AST, lhs: AST, rhs: AST) -> AST {

    match (lhs, rhs) {
        (Number(l), Number(r)) => match op {
            Name(ref s) if *s == "+" => Number(l + r),
            Name(ref s) if *s == "-" => Number(l - r),
            Name(ref s) if *s == "*" => Number(l * r),
            Name(ref s) if *s == "/" => if r == 0.0 { Null } else { Number(l / r) },
            Name(ref s) if *s == "==" => if l == r { LangTrue } else { LangFalse },
            Name(ref s) if *s == ">" => if l > r { LangTrue } else { LangFalse },
            Name(ref s) if *s == "<" => if l < r { LangTrue } else { LangFalse },
            _ => Null
        },

        (LangString(s1), LangString(s2)) => match op {
            Name(ref s) if *s == "+" => LangString(format!("{}{}", s1, s2)),
            _ => Null
        },

        _ => Null
    }
}

#[cfg(test)]
mod test {
    use super::*;


}
