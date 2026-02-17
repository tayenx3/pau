//! Middle-end/Codegen

use std::collections::HashMap;
use cranelift::prelude::*;
use cranelift_codegen::{ir::{UserExternalName, UserFuncName}, settings::{self, Flags}};
use cranelift_codegen::ir::{
    BlockArg,
    StackSlot,
    condcodes::{
        IntCC,
        FloatCC
    },
};
use cranelift_module::{FuncId, Linkage, Module, default_libcall_names};
use cranelift_object::{ObjectBuilder, ObjectModule};
use colored::Colorize;
use crate::parser::ast::{Node, NodeKind, Param};
use crate::operator::Operator;
use crate::semantics::{ty, StructData, StructID};

pub fn generate_obj(path: &str, ast: &[Node], emit_ir: bool, ids: HashMap<String, StructID>, structs: HashMap<StructID, StructData>) -> Result<Vec<u8>, String> {
    use std::path::Path;

    let module_name = Path::new(path).file_stem().unwrap().display().to_string();
    IRGenerator::new(module_name, ids, structs)?.generate(ast, emit_ir)
}

fn seman_err() -> ! {
    panic!("compiler pipeline may have missed semantics checker")
}

struct IRGenerator<'irg> {
    module: ObjectModule,
    unit: Option<Value>,
    scope: Vec<HashMap<String, (StackSlot, Type)>>,
    functions: HashMap<String, (Signature, FuncId, &'irg [Param], &'irg [Node])>,
    ids: HashMap<String, StructID>,
    structs: HashMap<StructID, StructData>,
}

impl<'irg> IRGenerator<'irg> {
    fn new(module_name: String, ids: HashMap<String, StructID>, structs: HashMap<StructID, StructData>) -> Result<Self, String> {
        let mut builder = settings::builder();
        builder.set("opt_level", "speed_and_size")
            .map_err(|err|
                format!("{}: cranelift error: {err}", "error".bright_red().bold())
            )?;
        let flags = Flags::new(builder);
        
        let isa = cranelift_native::builder()
            .map_err(|err|
                format!("{}: cranelift error: {err}", "error".bright_red().bold())
            )?.finish(flags)
            .map_err(|err|
                format!("{}: cranelift error: {err}", "error".bright_red().bold())
            )?;

        let builder = ObjectBuilder::new(isa, module_name, default_libcall_names())
            .map_err(|err|
                format!("{}: cranelift error: {err}", "error".bright_red().bold())
            )?;
        let module = ObjectModule::new(builder);

        Ok(Self {
            module,
            unit: None,
            scope: Vec::new(),
            functions: HashMap::new(),
            ids,
            structs
        })
    }

    fn collect_function(&mut self, node: &'irg Node, emit_ir: bool) -> Result<(), String> {
        match &node.kind {
            NodeKind::FunctionDef {
                name,
                params,
                ty_cache,
                body,
                ..
            } => {
                self.scope = vec![HashMap::new()];
                self.unit = None;

                let mut sig = self.module.make_signature();
                for param in params {
                    let vt = param.ty_cache.as_ref().unwrap_or_else(|| seman_err()).to_clif_ty();
                    sig.params.push(AbiParam::new(vt));
                }
                let vt = ty_cache.as_ref().unwrap_or_else(|| seman_err()).to_clif_ty();
                sig.returns.push(AbiParam::new(vt));
                let func_id = self.module.declare_function(name, Linkage::Export, &sig)
                    .map_err(|err|
                        format!("{}: cranelift error: {err}", "error".bright_red().bold())
                    )?;
                self.functions.insert(name.clone(), (sig, func_id, params, body));
            },
            NodeKind::Semi(stmt) => self.collect_function(stmt, emit_ir)?,
            _ => {},
        }

        Ok(())
    }

    fn generate(mut self, ast: &'irg [Node], emit_ir: bool) -> Result<Vec<u8>, String> {
        for node in ast {
            self.collect_function(node, emit_ir)?;
        }

        for (name, (sig, func_id, params, body)) in self.functions.clone() {
            let mut ctx = self.module.make_context();
            ctx.func.signature = sig;
            ctx.func.name = UserFuncName::User(UserExternalName::new(0, func_id.as_u32()));
            let mut builder_ctx = FunctionBuilderContext::new();
            let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_ctx);

            let block = builder.create_block();
            builder.append_block_params_for_function_params(block);
            builder.switch_to_block(block);
            builder.seal_block(block);
        
            let p = builder.block_params(block).to_vec();
        
            for (pval, param) in p.iter().zip(params.iter()) {
                let param_ty = param.ty_cache.as_ref().unwrap_or_else(|| seman_err());
                let ss = builder.create_sized_stack_slot(StackSlotData {
                    kind: StackSlotKind::ExplicitSlot,
                    size: param_ty.size(&self.structs),
                    align_shift: param_ty.align(&self.structs),
                    key: None,
                });
                builder.ins().stack_store(*pval, ss, 0);
                self.scope.last_mut().unwrap().insert(param.name.clone(), (ss, param_ty.to_clif_ty()));
            }

            self.unit = Some(builder.ins().iconst(types::I8, 0));
            let mut result = self.unit.unwrap();

            for node in body {
                result = self.generate_node(node, &mut builder);
            }
            builder.ins().return_(&[result]);

            builder.finalize();

            if emit_ir {
                use colored::Colorize;

                println!("{}: IR (function `{name}`):\n{:?}", "debug".bright_cyan().bold(), ctx.func);
            }

            self.module.define_function(func_id, &mut ctx).unwrap();
            self.module.clear_context(&mut ctx);
        }

        let obj = self.module.finish();
        let bytes = obj.emit()
            .map_err(|err|
                format!("{}: cranelift error: {err}", "error".bright_red().bold())
            )?;

        Ok(bytes)
    }

    fn find_var(&self, name: &str) -> (StackSlot, Type) {
        for s in self.scope.iter().rev() {
            if s.contains_key(name) {
                return s[name]
            }
        }

        seman_err()
    }

    fn generate_node(&mut self, node: &Node, builder: &mut FunctionBuilder) -> Value {
        match &node.kind {
            NodeKind::Integer(n) => builder.ins().iconst(match size_of::<usize>() {
                4 => types::I32,
                8 => types::I64,
                _ => unreachable!()
            }, *n as i64),
            NodeKind::UnsignedInt(n) => builder.ins().iconst(match size_of::<usize>() {
                4 => types::I32,
                8 => types::I64,
                _ => unreachable!()
            }, *n as i64),
            NodeKind::Float(n) => match size_of::<usize>() {
                4 => builder.ins().f32const(*n as f32),
                8 => builder.ins().f64const(*n),
                _ => unreachable!()
            },
            NodeKind::Boolean(n) => builder.ins().iconst(types::I8, *n as i64),
            NodeKind::I8(n) => builder.ins().iconst(types::I8, *n as i64),
            NodeKind::I16(n) => builder.ins().iconst(types::I16, *n as i64),
            NodeKind::I32(n) => builder.ins().iconst(types::I32, *n as i64),
            NodeKind::I64(n) => builder.ins().iconst(types::I64, *n as i64),
            NodeKind::U8(n) => builder.ins().iconst(types::I8, *n as i64),
            NodeKind::U16(n) => builder.ins().iconst(types::I16, *n as i64),
            NodeKind::U32(n) => builder.ins().iconst(types::I32, *n as i64),
            NodeKind::U64(n) => builder.ins().iconst(types::I64, *n as i64),
            NodeKind::F32(n) => builder.ins().f32const(*n),
            NodeKind::F64(n) => builder.ins().f64const(*n),
            NodeKind::Identifier(n) => {
                let (slot, ty) = self.find_var(n);
                builder.ins().stack_load(ty, slot, 0)
            },
            NodeKind::Semi(stmt) => {
                self.generate_node(stmt, builder);
                self.unit.unwrap()
            },
            NodeKind::BinaryOp {
                op,
                lhs,
                rhs,
            } => {
                if *op == Operator::Walrus {
                    let rval = self.generate_node(rhs, builder);

                    if let NodeKind::Identifier(n) = &lhs.kind {
                        let (ss, _) = self.find_var(n);
                        builder.ins().stack_store(rval, ss, 0);
                    } else if let NodeKind::Index { collection, index } = &lhs.kind {
                        let cval = self.generate_node(collection, builder);
                        let idx_value = self.generate_node(index, builder);

                        let size = builder.ins().iconst(
                            ty::Type::UInt.to_clif_ty(),
                            lhs.ty.as_ref().unwrap().size(&self.structs) as i64
                        );
                        let offset = builder.ins().imul(idx_value, size);
                        let addr = builder.ins().iadd(cval, offset);
                        builder.ins().store(MemFlags::new(), rval, addr, 0);
                    } else if let NodeKind::FieldAccess { object, field, id } = &lhs.kind {
                        let oval = self.generate_node(object, builder);
                        let data = &self.structs[&id.unwrap()];
                        let mut current_offset = 0;
                        for (name, ty) in &data.fields {
                            let align = ty.align(&self.structs) as usize;
                            let padding = (align - (current_offset % align)) % align;
                            current_offset += padding;

                            if field.0 == *name { break }
                            current_offset += ty.size(&self.structs) as usize;
                        }
                        builder.ins().store(MemFlags::new(), rval, oval, current_offset as i32);
                    } else {
                        unreachable!("invalid mutation target")
                    }
                    
                    return rval;
                }
                
                let lval = self.generate_node(lhs, builder);
                let rval = self.generate_node(rhs, builder);

                match op {
                    Operator::Plus => match lhs.ty.as_ref().unwrap_or_else(|| seman_err()) {
                        ty if ty.is_int() => builder.ins().iadd(lval, rval),
                        ty if ty.is_float() => builder.ins().fadd(lval, rval),
                        _ => seman_err(),
                    },
                    Operator::Minus => match lhs.ty.as_ref().unwrap_or_else(|| seman_err()) {
                        ty if ty.is_int() => builder.ins().isub(lval, rval),
                        ty if ty.is_float() => builder.ins().fsub(lval, rval),
                        _ => seman_err(),
                    },
                    Operator::Star => match lhs.ty.as_ref().unwrap_or_else(|| seman_err()) {
                        ty if ty.is_int() => builder.ins().imul(lval, rval),
                        ty if ty.is_float() => builder.ins().fmul(lval, rval),
                        _ => seman_err(),
                    },
                    Operator::Slash => match lhs.ty.as_ref().unwrap_or_else(|| seman_err()) {
                        ty if ty.is_int() && ty.is_signed() => builder.ins().sdiv(lval, rval),
                        ty if ty.is_int() && ty.is_unsigned() => builder.ins().udiv(lval, rval),
                        ty if ty.is_float() => builder.ins().fdiv(lval, rval),
                        _ => seman_err(),
                    },
                    Operator::Modulo => match lhs.ty.as_ref().unwrap_or_else(|| seman_err()) {
                        ty if ty.is_int() && ty.is_signed() => builder.ins().srem(lval, rval),
                        ty if ty.is_int() && ty.is_unsigned() => builder.ins().urem(lval, rval),
                        ty if ty.is_float() => frem(lval, rval, builder),
                        _ => seman_err(),
                    },
                    Operator::Eq => match lhs.ty.as_ref().unwrap_or_else(|| seman_err()) {
                        ty if ty.is_int() => builder.ins().icmp(IntCC::Equal, lval, rval),
                        ty if ty.is_float() => builder.ins().fcmp(FloatCC::UnorderedOrEqual, lval, rval),
                        ty::Type::Unit => builder.ins().iconst(types::I8, 1), // () == () is always true
                        ty::Type::Bool => builder.ins().icmp(IntCC::Equal, lval, rval), // bool is i8 in CLIF
                        _ => unreachable!(),
                    },
                    Operator::Ne => match lhs.ty.as_ref().unwrap_or_else(|| seman_err()) {
                        ty if ty.is_int() => builder.ins().icmp(IntCC::NotEqual, lval, rval),
                        ty if ty.is_float() => builder.ins().fcmp(FloatCC::OrderedNotEqual, lval, rval),
                        ty::Type::Unit => builder.ins().iconst(types::I8, 0), // () != () is always false
                        ty::Type::Bool => builder.ins().icmp(IntCC::NotEqual, lval, rval), // bool is i8 in CLIF
                        _ => unreachable!(),
                    },
                    Operator::Gt => match lhs.ty.as_ref().unwrap_or_else(|| seman_err()) {
                        ty if ty.is_int() && ty.is_signed() => builder.ins().icmp(IntCC::SignedGreaterThan, lval, rval),
                        ty if ty.is_int() && ty.is_unsigned() => builder.ins().icmp(IntCC::UnsignedGreaterThan, lval, rval),
                        ty if ty.is_float() => builder.ins().fcmp(FloatCC::GreaterThan, lval, rval),
                        _ => seman_err(),
                    },
                    Operator::Lt => match lhs.ty.as_ref().unwrap_or_else(|| seman_err()) {
                        ty if ty.is_int() && ty.is_signed() => builder.ins().icmp(IntCC::SignedLessThan, lval, rval),
                        ty if ty.is_int() && ty.is_unsigned() => builder.ins().icmp(IntCC::UnsignedLessThan, lval, rval),
                        ty if ty.is_float() => builder.ins().fcmp(FloatCC::LessThan, lval, rval),
                        _ => seman_err(),
                    },
                    Operator::Ge => match lhs.ty.as_ref().unwrap_or_else(|| seman_err()) {
                        ty if ty.is_int() && ty.is_signed() => builder.ins().icmp(IntCC::SignedGreaterThanOrEqual, lval, rval),
                        ty if ty.is_int() && ty.is_unsigned() => builder.ins().icmp(IntCC::UnsignedGreaterThanOrEqual, lval, rval),
                        ty if ty.is_float() => builder.ins().fcmp(FloatCC::UnorderedOrGreaterThanOrEqual, lval, rval),
                        _ => seman_err(),
                    },
                    Operator::Le => match lhs.ty.as_ref().unwrap_or_else(|| seman_err()) {
                        ty if ty.is_int() && ty.is_signed() => builder.ins().icmp(IntCC::SignedLessThanOrEqual, lval, rval),
                        ty if ty.is_int() && ty.is_unsigned() => builder.ins().icmp(IntCC::UnsignedLessThanOrEqual, lval, rval),
                        ty if ty.is_float() => builder.ins().fcmp(FloatCC::UnorderedOrLessThanOrEqual, lval, rval),
                        _ => seman_err(),
                    },
                    Operator::Walrus | Operator::Bang => seman_err(),
                }
            },
            NodeKind::UnaryOp {
                op,
                operand,
            } => {
                let oval = self.generate_node(operand, builder);

                match op {
                    Operator::Plus => match operand.ty.as_ref().unwrap_or_else(|| seman_err()) {
                        ty if ty.is_int() => oval,
                        ty if ty.is_float() => oval,
                        _ => seman_err(),
                    },
                    Operator::Minus => match operand.ty.as_ref().unwrap_or_else(|| seman_err()) {
                        ty if ty.is_int() => builder.ins().ineg(oval),
                        ty if ty.is_float() => builder.ins().fneg(oval),
                        _ => seman_err(),
                    },
                    Operator::Bang => match operand.ty.as_ref().unwrap_or_else(|| seman_err()) {
                        ty if ty.is_int() => builder.ins().bnot(oval),
                        ty::Type::Bool => lnot(oval, builder),
                        _ => seman_err(),
                    }
                    _ => seman_err(),
                }
            },
            NodeKind::Declaration {
                name,
                ty: _,
                init,
                mutability: _,
            } => {
                let ss = builder.create_sized_stack_slot(StackSlotData {
                    kind: StackSlotKind::ExplicitSlot,
                    size: node.ty.as_ref().unwrap().size(&self.structs),
                    align_shift: node.ty.as_ref().unwrap().align(&self.structs),
                    key: None,
                });
                let init = init.as_ref()
                    .map(|expr| self.generate_node(expr, builder))
                    .unwrap_or(self.unit.unwrap());
                builder.ins().stack_store(init, ss, 0);

                let ty = node.ty.as_ref().unwrap().to_clif_ty();

                self.scope.last_mut().unwrap().insert(name.clone(), (ss, ty));

                init
            },
            NodeKind::IfCondition {
                condition,
                then_body,
                else_body,
            } => {
                let then_block = builder.create_block();
                builder.append_block_param(then_block, types::I8);
                let else_block = builder.create_block();
                builder.append_block_param(else_block, types::I8);
                let merge_block = builder.create_block();
                builder.append_block_param(merge_block, node.ty.as_ref().unwrap_or_else(|| seman_err()).to_clif_ty());
                builder.append_block_param(merge_block, types::I8);

                let cond_val = self.generate_node(condition, builder);
                // unit lives on the main block so we have to pass it along
                let unit = self.unit.unwrap();
                builder.ins().brif(
                    cond_val,
                    then_block, &[BlockArg::Value(unit)],
                    else_block, &[BlockArg::Value(unit)]
                );

                builder.switch_to_block(then_block);
                self.unit = Some(builder.block_params(then_block)[0]);
                let mut then_val = self.unit.unwrap();
                self.scope.push(HashMap::new());
                for node in then_body {
                    then_val = self.generate_node(node, builder);
                }
                self.scope.pop();
                builder.ins().jump(merge_block, &[BlockArg::Value(then_val), BlockArg::Value(self.unit.unwrap())]); // keep passing unit

                builder.switch_to_block(else_block);
                self.unit = Some(builder.block_params(else_block)[0]);
                let else_val = match else_body {
                    Some(else_body) => {
                        self.scope.push(HashMap::new());
                        let mut val = self.unit.unwrap();
                        for node in else_body {
                            val = self.generate_node(node, builder);
                        }
                        self.scope.pop();
                        val
                    },
                    None => builder.block_params(else_block)[0],
                };
                builder.ins().jump(merge_block, &[BlockArg::Value(else_val), BlockArg::Value(self.unit.unwrap())]); // keep passing unit

                builder.switch_to_block(merge_block);
                self.unit = Some(builder.block_params(merge_block)[1]); // switch to new unit value

                builder.seal_block(then_block);
                builder.seal_block(else_block);
                builder.seal_block(merge_block);

                builder.block_params(merge_block)[0]
            },
            NodeKind::WhileLoop {
                condition,
                body,
            } => {
                let cond_block = builder.create_block();
                builder.append_block_param(cond_block, types::I8);
                let body_block = builder.create_block();
                builder.append_block_param(body_block, types::I8);
                let break_block = builder.create_block();
                builder.append_block_param(break_block, types::I8);

                let unit = self.unit.unwrap();
                builder.ins().jump(
                    cond_block,
                    &[BlockArg::Value(unit)]
                );
                builder.switch_to_block(cond_block);
                let unit = builder.block_params(cond_block)[0];
                self.unit = Some(unit);
                let cond_val = self.generate_node(condition, builder);
                builder.ins().brif(
                    cond_val,
                    body_block, &[BlockArg::Value(unit)],
                    break_block, &[BlockArg::Value(unit)]
                );

                builder.switch_to_block(body_block);
                let unit = builder.block_params(body_block)[0];
                self.unit = Some(unit);
                for node in body {
                    self.generate_node(node, builder);
                }
                builder.ins().jump(cond_block, &[BlockArg::Value(unit)]);

                builder.seal_block(cond_block);
                builder.seal_block(body_block);

                builder.switch_to_block(break_block);
                builder.seal_block(break_block);
                self.unit = Some(builder.block_params(break_block)[0]);
                self.unit.unwrap()
            },
            NodeKind::FunctionCall {
                callee,
                args
            } => {
                if let Some(id) = self.ids.get(callee) {
                    let data = self.structs.get(id).unwrap().clone();
                    let ty = ty::Type::Struct(*id, callee.clone());
                    let ss = builder.create_sized_stack_slot(StackSlotData {
                        kind: StackSlotKind::ExplicitSlot,
                        size: ty.size(&self.structs),
                        align_shift: ty.align(&self.structs),
                        key: None
                    });
                    
                    let mut current_offset = 0;
                    for (field, (_, ty)) in args.iter().zip(data.fields) {
                        let align = ty.align(&self.structs) as usize;
                        let padding = (align - (current_offset % align)) % align;
                        current_offset += padding;
                        
                        let value = self.generate_node(field, builder);
                        builder.ins().stack_store(value, ss, current_offset as i32);
                        
                        current_offset += ty.size(&self.structs) as usize;
                    }
                    
                    builder.ins().stack_addr(ty::Type::Int.to_clif_ty(), ss, 0)
                } else {
                    let mut compiled_args = Vec::new();

                    for arg in args {
                        compiled_args.push(self.generate_node(arg, builder));
                    }

                    let func_id = self.functions[callee].1;
                    let func_ref = self.module.declare_func_in_func(
                        func_id,
                        builder.func
                    );
                    let call = builder.ins().call(func_ref, &compiled_args);
                    builder.inst_results(call)[0]
                }
            },
            NodeKind::Array(items) => {
                let ty = node.ty.as_ref().unwrap();
                match ty {
                    ty::Type::Array(inner, len) => {
                        let size = inner.size(&self.structs) * len.unwrap() as u32;
                        let ss = builder.create_sized_stack_slot(StackSlotData {
                            kind: StackSlotKind::ExplicitSlot,
                            size,
                            align_shift: inner.align(&self.structs),
                            key: None,
                        });
                        for (idx, item) in items.iter().enumerate() {
                            let value = self.generate_node(item, builder);
                            builder.ins().stack_store(value, ss, (inner.size(&self.structs) * idx as u32) as i32);
                        }
                        builder.ins().stack_addr(ty::Type::Int.to_clif_ty(), ss, 0)
                    },
                    _ => unreachable!()
                }
            },
            NodeKind::Index {
                collection, index
            } => {
                let cval = self.generate_node(collection, builder);
                let idx_value = self.generate_node(index, builder);

                let size = builder.ins().iconst(
                    ty::Type::UInt.to_clif_ty(),
                    node.ty.as_ref().unwrap().size(&self.structs) as i64
                );
                let offset = builder.ins().imul(idx_value, size);
                let addr = builder.ins().iadd(cval, offset);
                builder.ins().load(node.ty.as_ref().unwrap().to_clif_ty(), MemFlags::new(), addr, 0)
            },
            NodeKind::FunctionDef { .. } | NodeKind::StructDef { .. }
            | NodeKind::Const { .. }=> self.unit.unwrap_or_else(|| {
                self.unit = Some(builder.ins().iconst(types::I8, 0));
                self.unit.unwrap()
            }),
            NodeKind::FieldAccess { object, field, id } => {
                let value = self.generate_node(object, builder);
                let data = &self.structs[&id.unwrap()];
                let mut current_offset = 0;
                for (name, ty) in &data.fields {
                    let align = ty.align(&self.structs) as usize;
                    let padding = (align - (current_offset % align)) % align;
                    current_offset += padding;

                    if field.0 == *name { break }
                    current_offset += ty.size(&self.structs) as usize;
                }

                builder.ins().load(
                    node.ty.as_ref().unwrap().to_clif_ty(),
                    MemFlags::new(),
                    value,
                    current_offset as i32
                )
            },
        }
    }
}

// cranelift doesnt have frem for some reason
fn frem(x: Value, y: Value, builder: &mut FunctionBuilder) -> Value {
    let v0 = builder.ins().fdiv(x, y);
    let v1 = builder.ins().trunc(v0);
    let v3 = builder.ins().fmul(y, v1);
    builder.ins().fsub(x, v3)
}

// cranelift also doesn't have lnot
fn lnot(x: Value, builder: &mut FunctionBuilder) -> Value {
    let one = builder.ins().iconst(types::I8, 1);
    builder.ins().bxor(x, one)
}