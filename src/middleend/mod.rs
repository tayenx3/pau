use cranelift::prelude::*;
use cranelift_codegen::{settings::{self, Flags}};
use cranelift_module::{default_libcall_names, Module, Linkage};
use cranelift_object::{ObjectBuilder, ObjectModule};
use colored::Colorize;
use crate::parser::ast::{Node, NodeKind};
use crate::operator::Operator;
use crate::semantics::ty as ptys;

pub fn generate_obj(path: &str, ast: &[Node]) -> Result<Vec<u8>, String> {
    use std::path::Path;

    let module_name = Path::new(path).file_stem().unwrap().display().to_string();
    IRGenerator::new(module_name)?.generate(ast)
}

fn seman_err() -> ! {
    panic!("compiler pipeline may have missed semantics checker")
}

struct IRGenerator {
    module: ObjectModule,
}

impl IRGenerator {
    fn new(module_name: String) -> Result<Self, String> {
        let mut builder = settings::builder();
        builder.set("opt_level", "speed")
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
            module
        })
    }

    fn generate(mut self, ast: &[Node]) -> Result<Vec<u8>, String> {
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

        let zero = builder.ins().iconst(types::I64, 1);
        builder.ins().return_(&[zero]);

        builder.finalize();

        self.module.define_function(func_id, &mut ctx).unwrap();
        self.module.clear_context(&mut ctx);

        let obj = self.module.finish();
        let bytes = obj.emit()
            .map_err(|err|
                format!("{}: cranelift error: {err}", "error".bright_red().bold())
            )?;

        Ok(bytes)
    }

    fn generate_node(&mut self, node: &Node, builder: &mut FunctionBuilder) -> Result<Value, String> {
        match &node.kind {
            NodeKind::Integer(n) => Ok(builder.ins().iconst(types::I64, *n)),
            NodeKind::Float(n) => Ok(builder.ins().f64const(*n)),
            NodeKind::BinaryOp {
                op,
                lhs,
                rhs,
                ty_cache,
            } => {
                let lval = self.generate_node(lhs, builder)?;
                let rval = self.generate_node(rhs, builder)?;

                match op {
                    Operator::Plus => match ty_cache.as_ref().unwrap_or_else(|| seman_err()) {
                        ptys::Type::Int => Ok(builder.ins().iadd(lval, rval)),
                        ptys::Type::Float => Ok(builder.ins().fadd(lval, rval)),
                        _ => seman_err(),
                    },
                    Operator::Minus => match ty_cache.as_ref().unwrap_or_else(|| seman_err()) {
                        ptys::Type::Int => Ok(builder.ins().isub(lval, rval)),
                        ptys::Type::Float => Ok(builder.ins().fsub(lval, rval)),
                        _ => seman_err(),
                    },
                    Operator::Star => match ty_cache.as_ref().unwrap_or_else(|| seman_err()) {
                        ptys::Type::Int => Ok(builder.ins().imul(lval, rval)),
                        ptys::Type::Float => Ok(builder.ins().fmul(lval, rval)),
                        _ => seman_err(),
                    },
                    Operator::Slash => match ty_cache.as_ref().unwrap_or_else(|| seman_err()) {
                        ptys::Type::Int => Ok(builder.ins().sdiv(lval, rval)),
                        ptys::Type::Float => Ok(builder.ins().fdiv(lval, rval)),
                        _ => seman_err(),
                    },
                    Operator::Modulo => match ty_cache.as_ref().unwrap_or_else(|| seman_err()) {
                        ptys::Type::Int => Ok(builder.ins().srem(lval, rval)),
                        ptys::Type::Float => Ok(frem(lval, rval, builder)),
                        _ => seman_err(),
                    },
                    Operator::Walrus => seman_err(),
                }
            },
            _ => todo!("{node:#?}"),
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