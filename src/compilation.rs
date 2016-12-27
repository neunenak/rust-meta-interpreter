extern crate llvm_sys;

use self::llvm_sys::prelude::*;
use self::llvm_sys::core;
use std::ptr;
use std::ffi::CString;

use parser::{ParseResult, AST, ASTNode, Prototype, Function, Expression};

use llvm_wrap as LLVMWrap;

pub fn compilation_sequence(ast: AST, sourcefile: &str) {
    use std::process::Command;

    let ll_filename = "out.ll";
    let obj_filename = "out.o";
    let q: Vec<&str>  = sourcefile.split('.').collect();
    let bin_filename = match &q[..] {
        &[name, "schala"] => name,
        _ => panic!("Bad filename {}", sourcefile),
    };

    compile_ast(ast, ll_filename);
    Command::new("llc")
        .arg("-filetype=obj")
        .arg(ll_filename)
        .output()
        .expect("Failed to run llc");

    Command::new("gcc")
        .arg(format!("-o{}", bin_filename))
        .arg(obj_filename)
        .output()
        .expect("failed to run gcc");

    for filename in [obj_filename].iter() {
        Command::new("rm")
            .arg(filename)
            .output()
            .expect(&format!("failed to run rm {}", filename));
    }
}

fn compile_ast(ast: AST, filename: &str) {
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

    let value = ast.codegen(context, builder);

    LLVMWrap::BuildRet(builder, value);

    unsafe {
        let out_file = CString::new(filename).unwrap();
        core::LLVMPrintModuleToFile(module, out_file.as_ptr(), ptr::null_mut());
    }

    // Clean up. Values created in the context mostly get cleaned up there.
    LLVMWrap::DisposeBuilder(builder);
    LLVMWrap::DisposeModule(module);
    LLVMWrap::ContextDispose(context);
}

trait CodeGen {
    fn codegen(&self, LLVMContextRef, LLVMBuilderRef) ->  LLVMValueRef;
}

impl CodeGen for AST {
    fn codegen(&self, context: LLVMContextRef, builder: LLVMBuilderRef) -> LLVMValueRef {
        let first = self.get(0).unwrap();
        first.codegen(context, builder)
    }
}

impl CodeGen for ASTNode {
    fn codegen(&self, context: LLVMContextRef, builder: LLVMBuilderRef) -> LLVMValueRef {
        use self::ASTNode::*;
        match self {
            &ExprNode(ref expr) => expr.codegen(context, builder),
            &FuncNode(ref func) => func.codegen(context, builder),
        }
    }
}

impl CodeGen for Function {
    fn codegen(&self, context: LLVMContextRef, builder: LLVMBuilderRef) -> LLVMValueRef {
        let ref body = self.body;
        let first = body.get(0).unwrap();
        first.codegen(context, builder)
    }
}

impl CodeGen for Expression {
    fn codegen(&self, context: LLVMContextRef, builder: LLVMBuilderRef) -> LLVMValueRef {
        use self::Expression::*;

        let int_type = LLVMWrap::Int64TypeInContext(context);

        match self {
            &BinExp(ref op, ref left, ref right) => {
                let lhs = left.codegen(context, builder);
                let rhs = right.codegen(context, builder);
                let generator = match op.as_ref() {
                    "+" => LLVMWrap::BuildAdd,
                    "-" => LLVMWrap::BuildSub,
                    "*" => LLVMWrap::BuildMul,
                    "/" => LLVMWrap::BuildUDiv,
                    "%" => LLVMWrap::BuildSRem,
                    _ => panic!("Bad operator {}", op),
                };

                generator(builder, lhs, rhs, "temp")
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
