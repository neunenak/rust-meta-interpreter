extern crate llvm_sys;

use self::llvm_sys::prelude::*;
use self::llvm_sys::core;
use std::ptr;


use parser::{ParseResult, AST, ASTNode, Prototype, Function, Expression};

mod LLVMWrap {
    extern crate llvm_sys;
    use self::llvm_sys::prelude::*;
    use self::llvm_sys::core;
    use std::ptr;
    pub fn create_context() -> LLVMContextRef {
        unsafe {
            core::LLVMContextCreate()
        }
    }
    pub fn module_create_with_name(name: &str) -> LLVMModuleRef {
        unsafe {
            let n = name.as_ptr() as *const _;
            core::LLVMModuleCreateWithName(n)
        }
    }
    pub fn CreateBuilderInContext(context: LLVMContextRef) -> LLVMBuilderRef {
        unsafe {
            core::LLVMCreateBuilderInContext(context)
        }
    }
}

pub fn compile_ast(ast: AST) {
    println!("Compiling!");
    let context = LLVMWrap::create_context();
    let module = LLVMWrap::module_create_with_name("schala");
    let builder = LLVMWrap::CreateBuilderInContext(context);
    unsafe {

        let void = core::LLVMVoidTypeInContext(context);
        let function_type = core::LLVMFunctionType(void, ptr::null_mut(), 0, 0);
        let function = core::LLVMAddFunction(module, b"nop\0".as_ptr() as *const _,
                                                   function_type);

        let bb = core::LLVMAppendBasicBlockInContext(context, function,
                                                           b"entry\0".as_ptr() as *const _);
        core::LLVMPositionBuilderAtEnd(builder, bb);

        // Emit a `ret void` into the function
        core::LLVMBuildRetVoid(builder);

        // Dump the module as IR to stdout.
        core::LLVMDumpModule(module);

        // Clean up. Values created in the context mostly get cleaned up there.
        core::LLVMDisposeBuilder(builder);
        core::LLVMDisposeModule(module);
        core::LLVMContextDispose(context);
    }
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
            &BinExp(ref op, ref left, ref right) => {
                unimplemented!()
            },
            &Number(ref n) => {
                unimplemented!()
            },
            _ => unimplemented!(),
        }
    }
}
