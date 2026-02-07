use std::collections::HashMap;

use cranelift::prelude::*;
use cranelift_codegen::{ir::StackSlot, settings::{self, Flags}};
use cranelift_module::{default_libcall_names, Module, Linkage};
use cranelift_object::{ObjectBuilder, ObjectModule};
use colored::Colorize;
use crate::parser::ast::{Node, NodeKind};
use crate::operator::Operator;
use crate::semantics::ty as ptys;

pub fn generate_obj(path: &str, ast: &[Node], emit_ir: bool) -> Result<Vec<u8>, String> {
    use std::path::Path;

    let module_name = Path::new(path).file_stem().unwrap().display().to_string();
    IRGenerator::new(module_name)?.generate(ast, emit_ir)
}

fn seman_err() -> ! {
    panic!("compiler pipeline may have missed semantics checker")
}

struct IRGenerator {
    module: ObjectModule,
    unit: Option<Value>,
    scope: Vec<HashMap<String, (StackSlot, Type)>>,
}

impl IRGenerator {
    fn new(module_name: String) -> Result<Self, String> {
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
            scope: vec![HashMap::new()]
        })
    }

    fn cache_unit(&mut self, builder: &mut FunctionBuilder) -> Value {
        if self.unit.is_none() {
            self.unit = Some(builder.ins().iconst(types::I64, 0));
        }

        self.unit.unwrap()
    }

    fn generate(mut self, ast: &[Node], emit_ir: bool) -> Result<Vec<u8>, String> {
        let mut sig = self.module.make_signature();
        sig.returns.push(AbiParam::new(types::I64));

        let func_id = self.module.declare_function("main", Linkage::Export, &sig)
            .map_err(|err|
                format!("{}: cranelift error: {err}", "error".bright_red().bold())
            )?;

        let mut ctx = self.module.make_context();
        ctx.func.signature = sig;

        let mut builder_ctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_ctx);

        let block = builder.create_block();
        builder.append_block_params_for_function_params(block);
        builder.switch_to_block(block);
        builder.seal_block(block);

        let mut result = self.cache_unit(&mut builder);
        for node in ast {
            result = self.generate_node(node, &mut builder);
        }
        builder.ins().return_(&[result]);

        builder.finalize();

        if emit_ir {
            use colored::Colorize;

            println!("{}: IR:\n{:?}", "debug".bright_cyan().bold(), ctx.func);
        }

        self.module.define_function(func_id, &mut ctx).unwrap();
        self.module.clear_context(&mut ctx);

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
            }, *n),
            NodeKind::Float(n) => match size_of::<usize>() {
                4 => builder.ins().f32const(*n as f32),
                8 => builder.ins().f64const(*n),
                _ => unreachable!()
            },
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
                self.cache_unit(builder)
            },
            NodeKind::BinaryOp {
                op,
                lhs,
                rhs,
                ty_cache,
            } => {
                let lval = self.generate_node(lhs, builder);
                let rval = self.generate_node(rhs, builder);

                match op {
                    Operator::Plus => match ty_cache.as_ref().unwrap_or_else(|| seman_err()) {
                        ptys::Type::Int => builder.ins().iadd(lval, rval),
                        ptys::Type::Float => builder.ins().fadd(lval, rval),
                        _ => seman_err(),
                    },
                    Operator::Minus => match ty_cache.as_ref().unwrap_or_else(|| seman_err()) {
                        ptys::Type::Int => builder.ins().isub(lval, rval),
                        ptys::Type::Float => builder.ins().fsub(lval, rval),
                        _ => seman_err(),
                    },
                    Operator::Star => match ty_cache.as_ref().unwrap_or_else(|| seman_err()) {
                        ptys::Type::Int => builder.ins().imul(lval, rval),
                        ptys::Type::Float => builder.ins().fmul(lval, rval),
                        _ => seman_err(),
                    },
                    Operator::Slash => match ty_cache.as_ref().unwrap_or_else(|| seman_err()) {
                        ptys::Type::Int => builder.ins().sdiv(lval, rval),
                        ptys::Type::Float => builder.ins().fdiv(lval, rval),
                        _ => seman_err(),
                    },
                    Operator::Modulo => match ty_cache.as_ref().unwrap_or_else(|| seman_err()) {
                        ptys::Type::Int => builder.ins().srem(lval, rval),
                        ptys::Type::Float => frem(lval, rval, builder),
                        _ => seman_err(),
                    },
                    Operator::Walrus => seman_err(),
                }
            },
            NodeKind::UnaryOp {
                op,
                operand,
                ty_cache,
            } => {
                let oval = self.generate_node(operand, builder);

                match op {
                    Operator::Plus => match ty_cache.as_ref().unwrap_or_else(|| seman_err()) {
                        ptys::Type::Int | ptys::Type::Float => oval,
                        _ => seman_err(),
                    },
                    Operator::Minus => match ty_cache.as_ref().unwrap_or_else(|| seman_err()) {
                        ptys::Type::Int => builder.ins().ineg(oval),
                        ptys::Type::Float => builder.ins().fneg(oval),
                        _ => seman_err(),
                    },
                    _ => seman_err(),
                }
            },
            NodeKind::Declaration {
                name,
                ty: _,
                resolved_ty,
                init,
                mutability: _,
            } => {
                let resolved_ty = resolved_ty.as_ref().unwrap();
                let ss = builder.create_sized_stack_slot(StackSlotData {
                    kind: StackSlotKind::ExplicitSlot,
                    size: resolved_ty.size(),
                    align_shift: resolved_ty.align(),
                    key: None,
                });
                let init = init.as_ref()
                    .map(|expr| self.generate_node(expr, builder))
                    .unwrap_or(self.cache_unit(builder));
                builder.ins().stack_store(init, ss, 0);

                let ty = resolved_ty.to_clif_ty();

                self.scope.last_mut().unwrap().insert(name.clone(), (ss, ty));

                init
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