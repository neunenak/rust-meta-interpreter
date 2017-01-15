extern crate llvm_sys;

use std::collections::HashMap;

use self::llvm_sys::prelude::*;
use self::llvm_sys::{LLVMIntPredicate, LLVMRealPredicate};

use parser::{AST, Statement, Function, Prototype, Expression, BinOp};

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
    main_function: LLVMValueRef,
}

fn compile_ast(ast: AST, filename: &str) {
    println!("Compiling!");
    let names: VariableMap = HashMap::new();

    let context = LLVMWrap::create_context();
    let module = LLVMWrap::module_create_with_name("example module");
    let builder = LLVMWrap::CreateBuilderInContext(context);

    let program_return_type = LLVMWrap::Int64TypeInContext(context);
    let main_function_type = LLVMWrap::FunctionType(program_return_type, Vec::new(), false);
    let main_function: LLVMValueRef = LLVMWrap::AddFunction(module, "main", main_function_type);

    let mut data = CompilationData {
        context: context,
        builder: builder,
        module: module,
        variables: names,
        main_function: main_function,
    };

    let bb = LLVMWrap::AppendBasicBlockInContext(data.context, data.main_function, "entry");
    LLVMWrap::PositionBuilderAtEnd(builder, bb);

    let value = ast.codegen(&mut data);

    LLVMWrap::BuildRet(builder, value);

    println!("Printing {} to file", filename);
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

        let function = self.prototype.codegen(data);
        let ref body = self.body;

        let return_type = LLVMWrap::Int64TypeInContext(data.context);
        let mut ret = LLVMWrap::ConstInt(return_type, 0, false);

        println!("Getting here");

        for expr in body {
            ret = expr.codegen(data);
        }
        ret
    }
}

impl CodeGen for Prototype {
    fn codegen(&self, data: &mut CompilationData) -> LLVMValueRef {
        let num_args = self.parameters.len();
        let return_type = LLVMWrap::Int64TypeInContext(data.context);
        let mut arguments: Vec<LLVMTypeRef> = vec![];

        for _ in 0..num_args {
            arguments.push(LLVMWrap::Int64TypeInContext(data.context));
        }
        
        let function_type =
            LLVMWrap::FunctionType(return_type,
                                   arguments,
                                   false);

        let function = LLVMWrap::AddFunction(data.module,
                                             &*self.name,
                                             function_type);

        function
    }
}

impl CodeGen for Expression {
    fn codegen(&self, data: &mut CompilationData) -> LLVMValueRef {
        use self::BinOp::*;
        use self::Expression::*;

        let int_type = LLVMWrap::Int64TypeInContext(data.context);
        let zero = LLVMWrap::ConstInt(int_type, 0, false);

        match *self {
            Variable(ref name) => *data.variables.get(&**name).unwrap(),
            BinExp(Assign, ref left, ref right) => {
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
                op.codegen_with_ops(data, lhs, rhs)
            }
            Number(ref n) => {
                let native_val = *n as u64;
                let int_value: LLVMValueRef = LLVMWrap::ConstInt(int_type, native_val, false);
                int_value
            }
            Conditional(ref test, ref then_expr, ref else_expr) => {
                let condition_value = test.codegen(data);
                let is_nonzero =
                    LLVMWrap::BuildICmp(data.builder,
                                        LLVMIntPredicate::LLVMIntNE,
                                        condition_value,
                                        zero,
                                        "is_nonzero");

                let func: LLVMValueRef = data.main_function;
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

                LLVMWrap::PositionBuilderAtEnd(data.builder, else_block);
                let else_return = match *else_expr {
                    Some(ref e) => e.codegen(data),
                    None => zero,
                };
                LLVMWrap::BuildBr(data.builder, merge_block);
                LLVMWrap::PositionBuilderAtEnd(data.builder, merge_block);
                zero
            }
            Block(ref exprs) => {
                let mut ret = zero;
                for e in exprs.iter() {
                    ret = e.codegen(data);
                }
                ret
            }
            ref e => {
                println!("Unimplemented {:?}", e);
                unimplemented!()
            }
        }
    }
}

impl BinOp {
    fn codegen_with_ops(&self, data: &CompilationData, lhs: LLVMValueRef, rhs: LLVMValueRef) -> LLVMValueRef {
        use self::BinOp::*;
        macro_rules! simple_binop {
            ($fnname: expr, $name: expr) => {
                $fnname(data.builder, lhs, rhs, $name)
            }
        }
        let int_type = LLVMWrap::Int64TypeInContext(data.context);
        match *self {
            Add => simple_binop!(LLVMWrap::BuildAdd, "addtemp"),
            Sub => simple_binop!(LLVMWrap::BuildSub, "subtemp"),
            Mul => simple_binop!(LLVMWrap::BuildMul, "multemp"),
            Div => simple_binop!(LLVMWrap::BuildUDiv, "divtemp"),
            Mod => simple_binop!(LLVMWrap::BuildSRem, "remtemp"),
            Less => {
                let pred: LLVMValueRef =
                    LLVMWrap::BuildICmp(data.builder, LLVMIntPredicate::LLVMIntULT, lhs, rhs, "tmp");
                LLVMWrap::BuildZExt(data.builder, pred, int_type, "temp")
            }
            Greater => {
                let pred: LLVMValueRef =
                    LLVMWrap::BuildICmp(data.builder, LLVMIntPredicate::LLVMIntUGT, lhs, rhs, "tmp");
                LLVMWrap::BuildZExt(data.builder, pred, int_type, "temp")
            }
            ref unknown => panic!("Bad operator {:?}", unknown),
        }
    }
}

