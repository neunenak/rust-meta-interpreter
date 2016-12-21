extern crate llvm_sys;

use self::llvm_sys::prelude::*;
use self::llvm_sys::core;


use parser::{ParseResult, AST, ASTNode, Prototype, Function, Expression};

pub fn compile_ast(ast: AST) {
    println!("Compiling!");

}

trait CodeGen {
    fn codegen(&self, LLVMContextRef) ->  LLVMValueRef;
}


impl CodeGen for ASTNode {
    fn codegen(&self, context: LLVMContextRef) -> LLVMValueRef {
        use self::ASTNode::*;
        match self {
            &ExprNode(ref expr) => expr.codegen(context),
            &FuncNode(ref func) => unimplemented!(),
        }
    }
}

impl CodeGen for Expression {
    fn codegen(&self, context: LLVMContextRef) -> LLVMValueRef {
        use self::Expression::*;
        match self {
            &Number(ref n) => unimplemented!(),
            _ => unimplemented!(),
        }
    }
}
