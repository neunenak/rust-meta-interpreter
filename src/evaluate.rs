use parser::AST;
use parser::AST::*;

pub fn evaluate(ast: AST) -> String {
    match reduce(ast) {
        None => return "error".to_string(),
        Some(DoNothing) => "".to_string(),
        Some(Number(n)) => return format!("{}", n),
        Some(LangString(s)) => return format!("\"{}\"", s),
        _ => return "not implemented".to_string()
    }
}

fn reduce(ast: AST) -> Option<AST> {
    match ast {
        Statements(stmts) => {
            let mut reduction = Some(DoNothing);
            for stmt in stmts.into_iter() {
                reduction = reduce(stmt);
            }
            reduction
        },
        other_ast => Some(other_ast)
    }
}
