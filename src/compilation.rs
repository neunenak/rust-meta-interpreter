extern crate llvm_sys;
extern crate iron_llvm;

use std::collections::HashMap;
use self::llvm_sys::prelude::LLVMValueRef;

use self::iron_llvm::core;
use self::iron_llvm::core::types::{RealTypeCtor, RealTypeRef};
use self::iron_llvm::{LLVMRef, LLVMRefCtor};

use parser::{ParseResult, AST, ASTNode, Prototype, Function, Expression};

pub struct Context {
    context: core::Context,
    builder: core::Builder,
    named_values: HashMap<String, LLVMValueRef>,
    ty: RealTypeRef,
}

impl Context {
    pub fn new() -> Context {
        let context = core::Context::get_global();
        let builder = core::Builder::new();
        let named_values = HashMap::new();
        let ty = RealTypeRef::get_double();
        Context {
            context: context,
            builder: builder,
            named_values: named_values,
            ty: ty,
        }
    }
}

pub trait ModuleProvider {
    fn dump(&self);
    fn get_module(&mut self) -> &mut core::Module;
    fn get_function(&mut self, name: &str) -> Option<(FunctionRef, bool)>;
}

pub struct SimpleModuleProvider {
    module: core::Module,
}

impl SimpleModuleProvider {
    pub fn new(name: &str) -> SimpleModuleProvider {
        let module = core::Module::new(name);
        SimpleModuleProvider {
            module: module,
        }
    }
}

impl ModuleProvider for SimpleModuleProvider {
    fn dump(&self) {
        self.module.dump();
    }

    fn get_module(&mut self) -> &mut core::Module {
        &mut self.module
    }

    fn get_function(&mut self, name: &str) -> Option<(FunctionRef, bool)> {
        match self.module.get_function_by_name(name) {
            Some(f) => Some((f, f.count_basic_block() > 0)),
            None => None
        }
    }
}

pub type IRBuildingResult = Result<(LLVMValueRef, bool), String>;

fn error(msg: &str) -> IRBuildingResult {
    Err(msg.to_string())
}

pub trait IRBuilder {
    fn codegen(&self, context: &mut Context, module_provider: &mut ModuleProvider) -> IRBuildingResult;
}

impl IRBuilder for ParseResult<AST> {
    fn codegen(&self, context: &mut Context, module_provider: &mut ModuleProvider) -> IRBuildingResult {
        match self {
            &Ok(ast) => ast.codegen(context, module_provider),
            &Err(err) => Err(err.msg.clone())
        }
    }
}

impl IRBuilder for AST {
    fn codegen(&self, context: &mut Context, module_provider: &mut ModuleProvider) -> IRBuildingResult {
        let mut result = error("empty AST");
        for node in self.iter() {
            result = Ok(try!(node.codegen(context, module_provider)));
        }
        result
    }
}

impl IRBuilder for ASTNode {
    fn codegen(&self, context: &mut Context, module_provider: &mut ModuleProvider) -> IRBuildingResult {
        match self {
            &ASTNode::ExprNode(ref expression) => expression.codegen(context, module_provider),
            &ASTNode::FuncNode(ref function) => function.codegen(context, module_provider),
        }
    }
}

impl IRBuilder for Function {
    fn codegen(&self, context: &mut Context, module_provider: &mut ModuleProvider) -> IRBuildingResult {
        unimplemented!()
    }
}

impl IRBuilder for Expression {
    fn codegen(&self, context: &mut Context, module_provider: &mut ModuleProvider) -> IRBuildingResult {
        unimplemented!()
    }
}


