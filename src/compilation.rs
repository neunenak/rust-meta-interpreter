extern crate llvm_sys;

use std::collections::HashMap;

use self::llvm_sys::prelude::*;
use parser::{AST, Statement, Function, Expression};

use llvm_wrap as LLVMWrap;

pub fn compilation_sequence(ast: AST, sourcefile: &str) {
    use std::process::Command;

    let ll_filename = "out.ll";
    let obj_filename = "out.o";
    let q: Vec<&str> = sourcefile.split('.').collect();
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

type VariableMap = HashMap<String, LLVMValueRef>;

struct CompilationData {
    context: LLVMContextRef,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,
    variables: VariableMap,
}

fn compile_ast(ast: AST, filename: &str) {
    println!("Compiling!");
    let names: VariableMap = HashMap::new();

    let context = LLVMWrap::create_context();
    let module = LLVMWrap::module_create_with_name("example module");
    let builder = LLVMWrap::CreateBuilderInContext(context);

    let mut data = CompilationData {
        context: context,
        module: module,
        builder: builder,
        variables: names,
    };

    let int_type = LLVMWrap::Int64TypeInContext(data.context);
    let function_type = LLVMWrap::FunctionType(int_type, &Vec::new(), false);
    let function = LLVMWrap::AddFunction(data.module, "main", function_type);

    let bb = LLVMWrap::AppendBasicBlockInContext(data.context, function, "entry");
    LLVMWrap::PositionBuilderAtEnd(builder, bb);

    let value = ast.codegen(&mut data);

    LLVMWrap::BuildRet(builder, value);

    LLVMWrap::PrintModuleToFile(module, filename);

    // Clean up. Values created in the context mostly get cleaned up there.
    LLVMWrap::DisposeBuilder(builder);
    LLVMWrap::DisposeModule(module);
    LLVMWrap::ContextDispose(context);
}

trait CodeGen {
    fn codegen(&self, &mut CompilationData) -> LLVMValueRef;
}

impl CodeGen for AST {
    fn codegen(&self, data: &mut CompilationData) -> LLVMValueRef {

        let int_type = LLVMWrap::Int64TypeInContext(data.context);
        let mut ret = LLVMWrap::ConstInt(int_type, 0, false);

        for statement in self {
            ret = statement.codegen(data);
        }
        ret
    }
}

impl CodeGen for Statement {
    fn codegen(&self, data: &mut CompilationData) -> LLVMValueRef {
        use self::Statement::*;
        match self {
            &ExprNode(ref expr) => expr.codegen(data),
            &FuncDefNode(ref func) => func.codegen(data),
        }
    }
}

impl CodeGen for Function {
    fn codegen(&self, data: &mut CompilationData) -> LLVMValueRef {
        let ref body = self.body;
        let int_type = LLVMWrap::Int64TypeInContext(data.context);
        let mut ret = LLVMWrap::ConstInt(int_type, 0, false);
        for expr in body {
            ret = expr.codegen(data);
        }
        ret
    }
}

impl CodeGen for Expression {
    fn codegen(&self, data: &mut CompilationData) -> LLVMValueRef {
        println!("Running codegen on: {:?}", self);
        use self::Expression::*;

        let int_type = LLVMWrap::Int64TypeInContext(data.context);

        match *self {
            Variable(ref name) => *data.variables.get(&**name).unwrap(),
            BinExp(ref op, ref left, ref right) if **op == "=" => {
                if let Variable(ref name) = **left {
                    let new_value = right.codegen(data);
                    data.variables.insert((**name).clone(), new_value);
                    new_value
                } else {
                    panic!("Bad variable assignment")
                }
            }
            BinExp(ref op, ref left, ref right) => {
                let lhs = left.codegen(data);
                let rhs = right.codegen(data);
                let generator = match op.as_ref().as_ref() {
                    "+" => LLVMWrap::BuildAdd,
                    "-" => LLVMWrap::BuildSub,
                    "*" => LLVMWrap::BuildMul,
                    "/" => LLVMWrap::BuildUDiv,
                    "%" => LLVMWrap::BuildSRem,
                    _ => panic!("Bad operator {}", op),
                };

                generator(data.builder, lhs, rhs, "temp")
            }
            Number(ref n) => {
                let native_val = *n as u64;
                let int_value: LLVMValueRef = LLVMWrap::ConstInt(int_type, native_val, false);
                int_value
            }
            Conditional(ref test, ref then_expr, ref else_expr) => {
                let condition_value = test.codegen(data);
                let zero = LLVMWrap::ConstInt(int_type, 0, false);
                let is_nonzero =
                    LLVMWrap::BuildICmp(data.builder,
                                        llvm_sys::LLVMIntPredicate::LLVMIntNE,
                                        condition_value,
                                        zero,
                                        "is_nonzero");

                let func: LLVMValueRef = zero;
                let then_block =
                    LLVMWrap::AppendBasicBlockInContext(data.context, func, "entry");
                let else_block =
                    LLVMWrap::AppendBasicBlockInContext(data.context, func, "entry");
                let merge_block =
                    LLVMWrap::AppendBasicBlockInContext(data.context, func, "entry");
                LLVMWrap::BuildCondBr(data.builder, is_nonzero, then_block, else_block);
                LLVMWrap::PositionBuilderAtEnd(data.builder, then_block);
                let then_return = then_expr.codegen(data);
                LLVMWrap::BuildBr(data.builder, merge_block);
                let else_return = match else_expr {
                    &Some(e) => e.codegen(data),
                    &None => zero,
                };
                LLVMWrap::BuildBr(data.builder, merge_block);
                LLVMWrap::PositionBuilderAtEnd(data.builder, else_block);
                zero
            }
            _ => unimplemented!(),
        }
    }
}
