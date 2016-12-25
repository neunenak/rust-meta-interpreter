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
    use std::ffi::CString;

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

    pub fn AppendBasicBlockInContext(context: LLVMContextRef, function: LLVMValueRef, name: &str) -> LLVMBasicBlockRef {
        let c_name = CString::new(name).unwrap();
        unsafe {
            core::LLVMAppendBasicBlockInContext(context, function, c_name.as_ptr())
        }
    }

    pub fn AddFunction(module: LLVMModuleRef, name: &str, function_type: LLVMTypeRef) ->  LLVMValueRef {
        let c_name = CString::new(name).unwrap();
        unsafe {
            core::LLVMAddFunction(module, c_name.as_ptr(), function_type)
        }
    }

    //NOTE this is incomplete
    pub fn FunctionType(return_type: LLVMTypeRef, param_types: &[LLVMTypeRef], is_var_rag: bool) -> LLVMTypeRef {
        let len = param_types.len();
        unsafe {
            core::LLVMFunctionType(return_type, ptr::null_mut(), len as u32, if is_var_rag { 1 } else { 0 })
        }
    }

    pub fn VoidTypeInContext(context: LLVMContextRef) -> LLVMTypeRef {
        unsafe {
            core::LLVMVoidTypeInContext(context)
        }
    }

    pub fn DisposeBuilder(builder: LLVMBuilderRef) {
        unsafe {
            core::LLVMDisposeBuilder(builder)
        }
    }

    pub fn DisposeModule(module: LLVMModuleRef) {
        unsafe {
            core::LLVMDisposeModule(module)
        }
    }

    pub fn ContextDispose(context: LLVMContextRef) {
        unsafe {
            core::LLVMContextDispose(context)
        }
    }

    pub fn PositionBuilderAtEnd(builder: LLVMBuilderRef, basic_block: LLVMBasicBlockRef) {
        unsafe {
            core::LLVMPositionBuilderAtEnd(builder, basic_block)
        }
    }

    pub fn BuildRetVoid(builder: LLVMBuilderRef) -> LLVMValueRef {
        unsafe {
            core::LLVMBuildRetVoid(builder)
        }
    }

    pub fn DumpModule(module: LLVMModuleRef) {
        unsafe {
            core::LLVMDumpModule(module)
        }
    }
}

pub fn compile_ast(ast: AST) {
    println!("Compiling!");

    let name = "nop";
    let context = LLVMWrap::create_context();
    let module = LLVMWrap::module_create_with_name(name);
    let builder = LLVMWrap::CreateBuilderInContext(context);

    let void = LLVMWrap::VoidTypeInContext(context);
    let function_type = LLVMWrap::FunctionType(void, &Vec::new(), false);
    let function = LLVMWrap::AddFunction(module, name, function_type);

    let bb = LLVMWrap::AppendBasicBlockInContext(context, function, "entry");

    LLVMWrap::PositionBuilderAtEnd(builder, bb);

    // Emit a `ret void` into the function
    LLVMWrap::BuildRetVoid(builder);

    // Dump the module as IR to stdout.
    LLVMWrap::DumpModule(module);

    // Clean up. Values created in the context mostly get cleaned up there.
    LLVMWrap::DisposeBuilder(builder);
    LLVMWrap::DisposeModule(module);
    LLVMWrap::ContextDispose(context);
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
