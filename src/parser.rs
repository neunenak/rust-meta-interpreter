use tokenizer::Token;
use tokenizer::Kw;

#[derive(Debug, Clone)]
pub enum ASTNode {
    ExprNode(Expression),
    FuncNode(Function),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub prototype: Prototype,
    pub body: Expression
}

#[derive(Debug, Clone)]
pub struct Prototype {
    pub name: String,
    pub args: Vec<String>
}

#[derive(Debug, Clone)]
pub enum Expression {
    StringLiteral(String),
    Number(f64),
    Variable(String),
    BinExp(String, Box<Expression>, Box<Expression>),
    Call(String, Vec<Expression>),
}

pub type AST = Vec<ASTNode>;

//TODO make this support incomplete parses
pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug)]
pub struct ParseError {
    pub msg: String
}

impl ParseError {
    fn new<T>(msg: &str) -> ParseResult<T> {
        Err(ParseError { msg: msg.to_string() })
    }
}

/* Grammar
   program := (statement delimiter ?)*
   delimiter := Newline | Semicolon
   statement := declaration | expression
   declaraion :=  Fn prototype expression
   prototype := identifier LParen (Ident Comma?)* RParen
   expression := primary_expression (op primary_expression)*
   primary_expression :=  Variable | Number | String | call_expr | paren_expr
   paren_expr := LParen expression RParen
   call_expr := identifier LParen (expression Comma ?)* RParen
   op := '+', '-', etc.
 */

pub fn parse(tokens: &[Token], parsed_tree: &[ASTNode]) -> ParseResult<AST> {
    use tokenizer::Token::*;

    let mut rest: Vec<Token> = tokens.to_vec();
    rest.reverse();

    let mut ast = parsed_tree.to_vec();

    loop {
        let cur_tok = match rest.last() {
            Some(t) => t.clone(),
            None => break
        };

        let result: ParseResult<ASTNode> = match cur_tok {
            Newline | Semicolon => { rest.pop(); continue},
            _ => parse_statement(&mut rest)
        };

        match result {
            Ok(node) => ast.push(node),
            Err(err) => return Err(err)
        }
    }

    Ok(ast)
}

fn parse_statement(tokens: &mut Vec<Token>) -> ParseResult<ASTNode> {
    use tokenizer::Token::*;
    let cur_tok: Token = tokens.last().unwrap().clone();
    let result: ASTNode = match cur_tok {
        Keyword(Kw::Fn) => try!(parse_declaration(tokens)),
        _ => try!(parse_expression(tokens))
    };

    Ok(result)
}

fn parse_declaration(tokens: &mut Vec<Token>) -> ParseResult<ASTNode> {
    use tokenizer::Token::*;
    tokens.pop();
    tokens.pop();
    Ok(ASTNode::ExprNode(Expression::StringLiteral("Declaration".to_string())))
}

fn parse_expression(tokens: &mut Vec<Token>) -> ParseResult<ASTNode> {
    use tokenizer::Token::*;
    tokens.pop();
    Ok(ASTNode::ExprNode(Expression::StringLiteral("Expr".to_string())))
}

