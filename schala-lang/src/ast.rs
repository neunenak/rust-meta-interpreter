use std::rc::Rc;

use builtin::{BinOp, PrefixOp};

#[derive(Debug, PartialEq)]
pub struct AST(pub Vec<Statement>);

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
  ExpressionStatement(Expression),
  Declaration(Declaration),
}

pub type Block = Vec<Statement>;

pub type ParamName = Rc<String>;
pub type InterfaceName = Rc<String>; //should be a singleton I think??
pub type FormalParam = (ParamName, Option<TypeName>);

#[derive(Debug, PartialEq, Clone)]
pub enum Declaration {
  FuncSig(Signature),
  FuncDecl(Signature, Block),
  TypeDecl(TypeSingletonName, TypeBody), //should have TypeSingletonName in it
  TypeAlias(Rc<String>, Rc<String>), //should have TypeSingletonName in it, or maybe just String, not sure
  Binding {
    name: Rc<String>,
    constant: bool,
    expr: Expression,
  },
  Impl {
    type_name: TypeName,
    interface_name: Option<InterfaceName>,
    block: Vec<Declaration>,
  },
  Interface {
    name: Rc<String>,
    signatures: Vec<Signature>
  }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Signature {
  pub name: Rc<String>,
  pub params: Vec<FormalParam>,
  pub type_anno: Option<TypeName>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypeBody(pub Vec<Variant>);

#[derive(Debug, PartialEq, Clone)]
pub enum Variant {
  UnitStruct(Rc<String>),
  TupleStruct(Rc<String>, Vec<TypeName>),
  Record(Rc<String>, Vec<(Rc<String>, TypeName)>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Expression(pub ExpressionType, pub Option<TypeName>);

#[derive(Debug, PartialEq, Clone)]
pub enum TypeName {
  Tuple(Vec<TypeName>),
  Singleton(TypeSingletonName)
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypeSingletonName {
  pub name: Rc<String>,
  pub params: Vec<TypeName>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExpressionType {
  NatLiteral(u64),
  FloatLiteral(f64),
  StringLiteral(Rc<String>),
  BoolLiteral(bool),
  BinExp(BinOp, Box<Expression>, Box<Expression>),
  PrefixExp(PrefixOp, Box<Expression>),
  TupleLiteral(Vec<Expression>),
  Value(Rc<String>),
  NamedStruct {
    name: Rc<String>,
    fields: Vec<(Rc<String>, Expression)>,
  },
  Call {
    f: Box<Expression>,
    arguments: Vec<Expression>,
  },
  Index {
    indexee: Box<Expression>,
    indexers: Vec<Expression>,
  },
  IfExpression {
    discriminator: Box<Discriminator>,
    body: Box<IfExpressionBody>,
  },
  WhileExpression {
    condition: Option<Box<Expression>>,
    body: Block,
  },
  ForExpression {
    enumerators: Vec<Enumerator>,
    body: Box<ForBody>,
  },
  Lambda {
    params: Vec<FormalParam>,
    body: Block,
  },
  ListLiteral(Vec<Expression>),
}
#[derive(Debug, PartialEq, Clone)]
pub enum Discriminator {
  Simple(Expression),
  BinOp(Expression, BinOp)
}

#[derive(Debug, PartialEq, Clone)]
pub enum IfExpressionBody {
  SimpleConditional(Block, Option<Block>),
  SimplePatternMatch(Pattern, Block, Option<Block>),
  GuardList(Vec<Guard>)
}

#[derive(Debug, PartialEq, Clone)]
pub struct Guard {
  pat: Pattern,
  body: Block,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Pattern {
  Ignored,
  TuplePattern(Vec<Pattern>),
  Literal(PatternLiteral),
  TupleStruct(Rc<String>, Vec<Pattern>),
  Record(Rc<String>, Vec<(Rc<String>, Pattern)>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum PatternLiteral {
  NumPattern(ExpressionType),
  StringPattern(Rc<String>),
  BoolPattern(bool),
  VarPattern(Rc<String>)
}

#[derive(Debug, PartialEq, Clone)]
pub struct Enumerator {
  pub id: Rc<String>,
  pub generator: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ForBody {
  MonadicReturn(Expression),
  StatementBlock(Block),
}
