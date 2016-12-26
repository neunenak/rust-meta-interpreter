extern crate llvm_sys;

use self::llvm_sys::prelude::*;
use self::llvm_sys::core;
use std::ptr;
use std::ffi::CString;

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

    pub fn BuildRet(builder: LLVMBuilderRef, val: LLVMValueRef) -> LLVMValueRef {
        unsafe {
            core::LLVMBuildRet(builder, val)
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

    pub fn Int64TypeInContext(context: LLVMContextRef) -> LLVMTypeRef {
        unsafe {
            core::LLVMInt64TypeInContext(context)
        }
    }

    pub fn ConstInt(int_type: LLVMTypeRef, n: u64, sign_extend: bool) -> LLVMValueRef {
        unsafe {
            core::LLVMConstInt(int_type, n, if sign_extend { 1 } else { 0 })
        }
    }
}

pub fn compile_ast(ast: AST) {
    println!("Compiling!");
    println!("AST is {:?}", ast);

    let context = LLVMWrap::create_context();
    let module = LLVMWrap::module_create_with_name("example module");
    let builder = LLVMWrap::CreateBuilderInContext(context);

    //let void = LLVMWrap::VoidTypeInContext(context);

    let int_type = LLVMWrap::Int64TypeInContext(context);
    let function_type = LLVMWrap::FunctionType(int_type, &Vec::new(), false);
    let function = LLVMWrap::AddFunction(module, "main", function_type);

    let bb = LLVMWrap::AppendBasicBlockInContext(context, function, "entry");
    LLVMWrap::PositionBuilderAtEnd(builder, bb);

    /*
    let int_value: u64 = 84;
    let int_value = LLVMWrap::ConstInt(int_type, int_value, false);
    */

    let value = ast.codegen(context);

    LLVMWrap::BuildRet(builder, value);

    unsafe {
        let out_file = CString::new("out.ll").unwrap();
        core::LLVMPrintModuleToFile(module, out_file.as_ptr(), ptr::null_mut());
    }

    // Clean up. Values created in the context mostly get cleaned up there.
    LLVMWrap::DisposeBuilder(builder);
    LLVMWrap::DisposeModule(module);
    LLVMWrap::ContextDispose(context);
}

trait CodeGen {
    fn codegen(&self, LLVMContextRef) ->  LLVMValueRef;
}

impl CodeGen for AST {
    fn codegen(&self, context: LLVMContextRef) -> LLVMValueRef {
        let first = self.get(0).unwrap();
        first.codegen(context)
    }
}

impl CodeGen for ASTNode {
    fn codegen(&self, context: LLVMContextRef) -> LLVMValueRef {
        use self::ASTNode::*;
        match self {
            &ExprNode(ref expr) => expr.codegen(context),
            &FuncNode(ref func) => func.codegen(context),
        }
    }
}

impl CodeGen for Function {
    fn codegen(&self, context: LLVMContextRef) -> LLVMValueRef {
        let ref body = self.body;
        let first = body.get(0).unwrap();
        first.codegen(context)
    }
}


impl CodeGen for Expression {
    fn codegen(&self, context: LLVMContextRef) -> LLVMValueRef {
        use self::Expression::*;

        let int_type = LLVMWrap::Int64TypeInContext(context);

        match self {
            &BinExp(ref op, ref left, ref right) => {
                unimplemented!()
            },
            &Number(ref n) => {
                let native_val = *n as u64;
                let int_value: LLVMValueRef = LLVMWrap::ConstInt(int_type, native_val, false);
                int_value
            },
            _ => unimplemented!(),
        }
    }
}
