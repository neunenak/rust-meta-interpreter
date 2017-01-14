#![allow(non_snake_case)]
#![allow(dead_code)]
extern crate llvm_sys;

use self::llvm_sys::{LLVMIntPredicate, LLVMRealPredicate};
use self::llvm_sys::prelude::*;
use self::llvm_sys::core;
use std::ptr;
use std::ffi::CString;

pub fn create_context() -> LLVMContextRef {
    unsafe { core::LLVMContextCreate() }
}
pub fn module_create_with_name(name: &str) -> LLVMModuleRef {
    unsafe {
        let n = name.as_ptr() as *const _;
        core::LLVMModuleCreateWithName(n)
    }
}
pub fn CreateBuilderInContext(context: LLVMContextRef) -> LLVMBuilderRef {
    unsafe { core::LLVMCreateBuilderInContext(context) }
}

pub fn AppendBasicBlockInContext(context: LLVMContextRef,
                                 function: LLVMValueRef,
                                 name: &str)
                                 -> LLVMBasicBlockRef {
    let c_name = CString::new(name).unwrap();
    unsafe { core::LLVMAppendBasicBlockInContext(context, function, c_name.as_ptr()) }
}

pub fn AddFunction(module: LLVMModuleRef, name: &str, function_type: LLVMTypeRef) -> LLVMValueRef {
    let c_name = CString::new(name).unwrap();
    unsafe { core::LLVMAddFunction(module, c_name.as_ptr(), function_type) }
}

// NOTE this is incomplete
pub fn FunctionType(return_type: LLVMTypeRef,
                    param_types: &[LLVMTypeRef],
                    is_var_rag: bool)
                    -> LLVMTypeRef {
    let len = param_types.len();
    unsafe {
        core::LLVMFunctionType(return_type,
                               ptr::null_mut(),
                               len as u32,
                               if is_var_rag { 1 } else { 0 })
    }
}

pub fn VoidTypeInContext(context: LLVMContextRef) -> LLVMTypeRef {
    unsafe { core::LLVMVoidTypeInContext(context) }
}

pub fn DisposeBuilder(builder: LLVMBuilderRef) {
    unsafe { core::LLVMDisposeBuilder(builder) }
}

pub fn DisposeModule(module: LLVMModuleRef) {
    unsafe { core::LLVMDisposeModule(module) }
}

pub fn ContextDispose(context: LLVMContextRef) {
    unsafe { core::LLVMContextDispose(context) }
}

pub fn PositionBuilderAtEnd(builder: LLVMBuilderRef, basic_block: LLVMBasicBlockRef) {
    unsafe { core::LLVMPositionBuilderAtEnd(builder, basic_block) }
}

pub fn BuildRet(builder: LLVMBuilderRef, val: LLVMValueRef) -> LLVMValueRef {
    unsafe { core::LLVMBuildRet(builder, val) }
}

pub fn BuildRetVoid(builder: LLVMBuilderRef) -> LLVMValueRef {
    unsafe { core::LLVMBuildRetVoid(builder) }
}

pub fn DumpModule(module: LLVMModuleRef) {
    unsafe { core::LLVMDumpModule(module) }
}

pub fn Int64TypeInContext(context: LLVMContextRef) -> LLVMTypeRef {
    unsafe { core::LLVMInt64TypeInContext(context) }
}

pub fn ConstInt(int_type: LLVMTypeRef, n: u64, sign_extend: bool) -> LLVMValueRef {
    unsafe { core::LLVMConstInt(int_type, n, if sign_extend { 1 } else { 0 }) }
}

pub fn BuildAdd(builder: LLVMBuilderRef,
                lhs: LLVMValueRef,
                rhs: LLVMValueRef,
                reg_name: &str)
                -> LLVMValueRef {
    let name = CString::new(reg_name).unwrap();
    unsafe { core::LLVMBuildAdd(builder, lhs, rhs, name.as_ptr()) }
}

pub fn BuildSub(builder: LLVMBuilderRef,
                lhs: LLVMValueRef,
                rhs: LLVMValueRef,
                reg_name: &str)
                -> LLVMValueRef {
    let name = CString::new(reg_name).unwrap();
    unsafe { core::LLVMBuildSub(builder, lhs, rhs, name.as_ptr()) }
}

pub fn BuildMul(builder: LLVMBuilderRef,
                lhs: LLVMValueRef,
                rhs: LLVMValueRef,
                reg_name: &str)
                -> LLVMValueRef {
    let name = CString::new(reg_name).unwrap();
    unsafe { core::LLVMBuildMul(builder, lhs, rhs, name.as_ptr()) }
}

pub fn BuildUDiv(builder: LLVMBuilderRef,
                 lhs: LLVMValueRef,
                 rhs: LLVMValueRef,
                 reg_name: &str)
                 -> LLVMValueRef {
    let name = CString::new(reg_name).unwrap();
    unsafe { core::LLVMBuildUDiv(builder, lhs, rhs, name.as_ptr()) }
}

pub fn BuildSRem(builder: LLVMBuilderRef,
                 lhs: LLVMValueRef,
                 rhs: LLVMValueRef,
                 reg_name: &str)
                 -> LLVMValueRef {
    let name = CString::new(reg_name).unwrap();
    unsafe { core::LLVMBuildSRem(builder, lhs, rhs, name.as_ptr()) }
}

pub fn BuildCondBr(builder: LLVMBuilderRef,
                   if_expr: LLVMValueRef,
                   then_expr: LLVMBasicBlockRef,
                   else_expr: LLVMBasicBlockRef) -> LLVMValueRef {


    unsafe { core::LLVMBuildCondBr(builder, if_expr, then_expr, else_expr) }
}

pub fn BuildBr(builder: LLVMBuilderRef,
               dest: LLVMBasicBlockRef) -> LLVMValueRef {
    unsafe { core::LLVMBuildBr(builder, dest) }
}

pub fn GetInsertBlock(builder: LLVMBuilderRef) -> LLVMBasicBlockRef {
    unsafe { core::LLVMGetInsertBlock(builder) }
}

pub fn BuildPhi(builder: LLVMBuilderRef, ty: LLVMTypeRef, name: &str) -> LLVMValueRef {
    let name = CString::new(name).unwrap();
    unsafe { core::LLVMBuildPhi(builder, ty, name.as_ptr()) }
}

pub fn AddIncoming(phi: LLVMValueRef, incoming_values: *mut LLVMValueRef, incoming_blocks: *mut LLVMBasicBlockRef,
                   count: u32) {

    unsafe { core::LLVMAddIncoming(phi, incoming_values, incoming_blocks, count) }
}

pub fn BuildFCmp(builder: LLVMBuilderRef,
                 op: LLVMRealPredicate,
                 lhs: LLVMValueRef,
                 rhs: LLVMValueRef,
                 name: &str) -> LLVMValueRef {
    let name = CString::new(name).unwrap();
    unsafe { core::LLVMBuildFCmp(builder, op, lhs, rhs, name.as_ptr()) }
}

pub fn BuildUIToFP(builder: LLVMBuilderRef,
                       val: LLVMValueRef,
                       dest_type: LLVMTypeRef,
                       name: &str) -> LLVMValueRef {

    let name = CString::new(name).unwrap();
    unsafe { let p = name.as_ptr();
        println!("Pointer {:?}", p);
        core::LLVMBuildUIToFP(builder, val, dest_type, p) }
}

pub fn BuildICmp(builder: LLVMBuilderRef,
                 op: LLVMIntPredicate,
                 lhs: LLVMValueRef,
                 rhs: LLVMValueRef,
                 name: &str) -> LLVMValueRef {
    let name = CString::new(name).unwrap();
    unsafe { core::LLVMBuildICmp(builder, op, lhs, rhs, name.as_ptr()) }
}

pub fn PrintModuleToFile(module: LLVMModuleRef, filename: &str) -> LLVMBool {
    let out_file = CString::new(filename).unwrap();
    unsafe { core::LLVMPrintModuleToFile(module, out_file.as_ptr(), ptr::null_mut()) }
}
