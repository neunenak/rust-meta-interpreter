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
    fn result_from_str<T>(msg: &str) -> ParseResult<T> {
        Err(ParseError { msg: msg.to_string() })
    }
}

/* Grammar
   program := (statement delimiter ?)*
   delimiter := Newline | Semicolon
   statement := declaration | expression
   declaraion :=  Fn prototype expression
   prototype := identifier LParen identlist RParen
   identlist := Ident (Comma Ident)*
   expression := primary_expression (op primary_expression)*
   primary_expression :=  Variable | Number | String | call_expr | paren_expr
   paren_expr := LParen expression RParen
   call_expr := identifier LParen identlist RParen
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

macro_rules! expect {
    ($token:pat, $tokens:expr, $error:expr) => {
        match $tokens.pop() {
            Some($token) => (),
            _ => return ParseError::result_from_str($error)
        };
    }
}

fn parse_statement(tokens: &mut Vec<Token>) -> ParseResult<ASTNode> {
    use tokenizer::Token::*;
    let cur_tok: Token = tokens.last().unwrap().clone();
    let node: ASTNode = match cur_tok {
        Keyword(Kw::Fn) => try!(parse_declaration(tokens)),
        _ => try!(parse_expression(tokens))
    };

    Ok(node)
}

fn parse_declaration(tokens: &mut Vec<Token>) -> ParseResult<ASTNode> {
    use tokenizer::Token::*;
    expect!(Fn, tokens, "Expected 'fn'");
    let prototype = try!(parse_prototype(tokens));
    let body = try!(parse_body(tokens));
    Ok(ASTNode::FuncNode(Function { prototype: prototype, body: body}))
}

fn parse_prototype(tokens: &mut Vec<Token>) -> ParseResult<Prototype> {
    use tokenizer::Token::*;
    let name: String = match tokens.pop() {
        Some(Identifier(name)) => name,
        _ => return ParseError::result_from_str("Expected identifier")
    };
    expect!(LParen, tokens, "Expected '('");
    let mut args: Vec<String> = try!(parse_identlist(tokens));
    expect!(RParen, tokens, "Expected ')'");

    Ok(Prototype {name: name, args: args})
}

fn parse_identlist(tokens: &mut Vec<Token>) -> ParseResult<Vec<String>> {
    use tokenizer::Token::*;
    let mut args: Vec<String> = Vec::new();
    loop {
        match tokens.pop() {
            Some(Identifier(name)) => {
                args.push(name);
                if let Some(&Comma) = tokens.last() {
                    tokens.pop();
                } else {
                    break;
                }
            },

            _ => break
        }
    }

    Ok(args)
}

fn parse_body(tokens: &mut Vec<Token>) -> ParseResult<Expression> {
    use tokenizer::Token::*;
    tokens.pop();
    Ok(Expression::Number(101.01))
}

fn parse_expression(tokens: &mut Vec<Token>) -> ParseResult<ASTNode> {
    use tokenizer::Token::*;
    tokens.pop();
    Ok(ASTNode::ExprNode(Expression::StringLiteral("Expr".to_string())))
}

