use crate::ir::{self, CType};

const RUNTIME_C: &str = include_str!("./runtime.h");

pub fn generate(program: &ir::Program) -> String {
    let mut code = String::new();

    code.push_str(RUNTIME_C);
    code.push_str("\n// --- Generated Code ---\n\n");

    // Forward Declarations (Structs)
    for s in &program.structs {
        code.push_str(&format!("typedef struct {} {};\n", s.name, s.name));
    }
    code.push_str("\n");

    // Struct Definitions
    for s in &program.structs {
        code.push_str(&format!("struct {} {{\n", s.name));
        for (f_name, f_type) in &s.fields {
            code.push_str(&format!("    {} {};\n", emit_type(f_type), f_name));
        }
        code.push_str("};\n\n");
    }

    // Forward declarations (functions)
    for f in &program.functions {
        code.push_str(&format!(
            "{} {}(",
            emit_type(&f.ret_type),
            mangle_name(&f.name)
        ));
        for (i, (p_name, p_type)) in f.params.iter().enumerate() {
            if i > 0 {
                code.push_str(", ");
            }
            code.push_str(&format!("{} {}", emit_type(p_type), p_name));
        }
        code.push_str(");\n");
    }
    code.push_str("\n");

    // Function Bodies
    for f in &program.functions {
        code.push_str(&format!(
            "{} {}(",
            emit_type(&f.ret_type),
            mangle_name(&f.name)
        ));
        for (i, (p_name, p_type)) in f.params.iter().enumerate() {
            if i > 0 {
                code.push_str(", ");
            }
            code.push_str(&format!("{} {}", emit_type(p_type), p_name));
        }
        code.push_str(") {\n");

        for stmt in &f.body {
            emit_stmt(stmt, &mut code, 1);
        }

        if f.name == "main" {
            // Inject runtime init if it's main
        }

        code.push_str("}\n\n");
    }

    // Entry Point Wrapper
    code.push_str("int main() {\n");
    code.push_str("    runtime_init();\n");
    code.push_str("    user_main();\n"); 
    code.push_str("    return 0;\n");
    code.push_str("}\n");

    code
}

fn emit_type(t: &CType) -> String {
    match t {
        CType::Void => "void".into(),
        CType::Int => "int".into(), 
        CType::Bool => "bool".into(),
        CType::U8 => "uint8_t".into(),
        CType::CharStar => "char*".into(),
        CType::GenRef => "GenRef".into(),
        CType::Struct(s) => s.clone(), 
    }
}

fn mangle_name(n: &str) -> String {
    if n == "main" {
        return "user_main".into();
    }
    n.replace("::", "__")
}

fn emit_stmt(stmt: &ir::Stmt, code: &mut String, indent: usize) {
    let pad = "    ".repeat(indent);
    match stmt {
        ir::Stmt::VarDecl { name, ty, value } => {
            code.push_str(&format!("{}{}", pad, emit_type(ty)));
            code.push_str(&format!(" {}", name));
            if let Some(v) = value {
                code.push_str(" = ");
                emit_expr(v, code);
            }
            code.push_str(";\n");
        }
        ir::Stmt::Assign { target, value } => {
            code.push_str(&pad);
            emit_expr(target, code);
            code.push_str(" = ");
            emit_expr(value, code);
            code.push_str(";\n");
        }
        ir::Stmt::Return(opt_e) => {
            code.push_str(&pad);
            code.push_str("return");
            if let Some(e) = opt_e {
                code.push_str(" ");
                emit_expr(e, code);
            }
            code.push_str(";\n");
        }
        ir::Stmt::Expr(e) => {
            code.push_str(&pad);
            emit_expr(e, code);
            code.push_str(";\n");
        }
        ir::Stmt::If {
            cond,
            then_block,
            else_block,
        } => {
            code.push_str(&pad);
            code.push_str("if (");
            emit_expr(cond, code);
            code.push_str(") {\n");
            for s in then_block {
                emit_stmt(s, code, indent + 1);
            }
            code.push_str(&pad);
            code.push_str("}");
            if !else_block.is_empty() {
                code.push_str(" else {\n");
                for s in else_block {
                    emit_stmt(s, code, indent + 1);
                }
                code.push_str(&pad);
                code.push_str("}\n");
            } else {
                code.push_str("\n");
            }
        }
        ir::Stmt::While { cond, body } => {
            code.push_str(&pad);
            code.push_str("while (");
            emit_expr(cond, code);
            code.push_str(") {\n");
            for s in body {
                emit_stmt(s, code, indent + 1);
            }
            code.push_str(&pad);
            code.push_str("}\n");
        }
    }
}

fn emit_expr(expr: &ir::Expr, code: &mut String) {
    match expr {
        ir::Expr::Literal(s) => code.push_str(s),
        ir::Expr::Variable(v) => code.push_str(v),
        ir::Expr::Binary { op, lhs, rhs } => {
            code.push_str("(");
            emit_expr(lhs, code);
            code.push_str(&format!(" {} ", op));
            emit_expr(rhs, code);
            code.push_str(")");
        }
        ir::Expr::Call { func_name, args } => {
            code.push_str(&mangle_name(func_name));
            code.push_str("(");
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    code.push_str(", ");
                }
                emit_expr(arg, code);
            }
            code.push_str(")");
        }
        ir::Expr::Alloc { struct_name } => {
            code.push_str(&format!("juu_alloc(sizeof({}))", struct_name));
        }
        ir::Expr::MemberAccess {
            object,
            field,
            struct_name,
        } => {
            code.push_str(&format!("(({}*)juu_access(", struct_name));
            emit_expr(object, code);
            code.push_str(&format!("))->{}", field));
        }
        ir::Expr::Raw(s) => code.push_str(s),
    }
}
