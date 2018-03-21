extern crate llvm_sys;

use std::collections::HashMap;

use self::llvm_sys::prelude::*;
use self::llvm_sys::{LLVMIntPredicate};

use maaru_lang::parser::{AST, Statement, Function, Prototype, Expression, BinOp};
use schala_repl::LLVMCodeString;

use schala_repl::llvm_wrap as LLVMWrap;

type VariableMap = HashMap<String, LLVMValueRef>;

struct CompilationData {
    context: LLVMContextRef,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,
    variables: VariableMap,
    main_function: LLVMValueRef,
    current_function: Option<LLVMValueRef>,
}

pub fn compile_ast(ast: AST) -> LLVMCodeString {
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
        current_function: None,
    };

    let bb = LLVMWrap::AppendBasicBlockInContext(data.context, data.main_function, "entry");
    LLVMWrap::PositionBuilderAtEnd(builder, bb);

    let value = ast.codegen(&mut data);

    LLVMWrap::BuildRet(builder, value);

    let ret = LLVMWrap::PrintModuleToString(module);

    // Clean up. Values created in the context mostly get cleaned up there.
    LLVMWrap::DisposeBuilder(builder);
    LLVMWrap::DisposeModule(module);
    LLVMWrap::ContextDispose(context);
    LLVMCodeString(ret)
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

        /* should have a check here for function already being defined */
        let function = self.prototype.codegen(data);
        let ref body = self.body;

        data.current_function = Some(function);

        let return_type = LLVMWrap::Int64TypeInContext(data.context);
        let mut ret = LLVMWrap::ConstInt(return_type, 0, false);

        let block = LLVMWrap::AppendBasicBlockInContext(data.context, function, "entry");
        LLVMWrap::PositionBuilderAtEnd(data.builder, block);

        //insert function params into variables
        for value in LLVMWrap::GetParams(function) {
            let name = LLVMWrap::GetValueName(value);
            data.variables.insert(name, value);
        }

        for expr in body {
            ret = expr.codegen(data);
        }

        LLVMWrap::BuildRet(data.builder, ret);

        // get basic block of main
        let main_bb = LLVMWrap::GetBasicBlocks(data.main_function).get(0).expect("Couldn't get first block of main").clone();
        LLVMWrap::PositionBuilderAtEnd(data.builder, main_bb);

        data.current_function = None;

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

        let function_params = LLVMWrap::GetParams(function);
        for (index, param) in function_params.iter().enumerate() {
            let name = self.parameters.get(index).expect(&format!("Failed this check at index {}", index));
            let new = *param;

            LLVMWrap::SetValueName(new, name);
        }

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
            Variable(ref name) => *data.variables.get(&**name).expect(&format!("Can't find variable {}", name)),
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
                                        "ifcond");

                let func = LLVMWrap::GetBasicBlockParent(LLVMWrap::GetInsertBlock(data.builder));

                let mut then_block =
                    LLVMWrap::AppendBasicBlockInContext(data.context, func, "then_block");
                let mut else_block =
                    LLVMWrap::AppendBasicBlockInContext(data.context, func, "else_block");
                let merge_block =
                    LLVMWrap::AppendBasicBlockInContext(data.context, func, "ifcont");

                // add conditional branch to ifcond block
                LLVMWrap::BuildCondBr(data.builder, is_nonzero, then_block, else_block);

                // start inserting into then block
                LLVMWrap::PositionBuilderAtEnd(data.builder, then_block);

                // then-block codegen
                let then_return = then_expr.codegen(data);
                LLVMWrap::BuildBr(data.builder, merge_block);

                // update then block b/c recursive codegen() call may have changed the notion of
                // the current block
                then_block = LLVMWrap::GetInsertBlock(data.builder);

                // then do the same stuff again for the else branch
                //
                LLVMWrap::PositionBuilderAtEnd(data.builder, else_block);
                let else_return = match *else_expr {
                    Some(ref e) => e.codegen(data),
                    None => zero,
                };
                LLVMWrap::BuildBr(data.builder, merge_block);
                else_block = LLVMWrap::GetInsertBlock(data.builder);

                LLVMWrap::PositionBuilderAtEnd(data.builder, merge_block);

                let phi = LLVMWrap::BuildPhi(data.builder, int_type, "phinode");
                let values = vec![then_return, else_return];
                let blocks = vec![then_block, else_block];
                LLVMWrap::AddIncoming(phi, values, blocks);
                phi
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

