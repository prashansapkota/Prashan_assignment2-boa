use sexp::*;
use sexp::Atom::*;
use std::env;
use std::fs::File;
use std::io::prelude::*;
use im::HashMap;

enum Op1 { Add1, Sub1 }
enum Op2 { Plus, Minus, Times }

enum Expr {
    Number(i32),
    Id(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
}

fn parse_bind(s: &Sexp) -> (String, Expr) {
    match s {
        Sexp::List(vec) => match &vec[..] {
            [Sexp::Atom(S(name)), e] => {
                if matches!(name.as_str(), "let" | "add1" | "sub1") {
                    panic!("Invalid: reserved word used as identifier");
                }
                (name.clone(), parse_expr(e))
            }
            _ => panic!("Invalid"),
        },
        _ => panic!("Invalid"),
    }
}

fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => Expr::Number(i32::try_from(*n).expect("Integer overflow")),

        Sexp::Atom(S(name)) => {
            if matches!(name.as_str(), "let" | "add1" | "sub1") {
                panic!("Invalid: reserved word used as expression");
            }
            Expr::Id(name.clone())
        }

        Sexp::List(vec) => match &vec[..] {
            [Sexp::Atom(S(op)), e] if op == "add1" => {
                Expr::UnOp(Op1::Add1, Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), e] if op == "sub1" => {
                Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "+" => {
                Expr::BinOp(Op2::Plus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "-" => {
                Expr::BinOp(Op2::Minus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), e1, e2] if op == "*" => {
                Expr::BinOp(Op2::Times, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            [Sexp::Atom(S(op)), Sexp::List(bindings), body] if op == "let" => {
                if bindings.is_empty() {
                    panic!("Invalid: let requires at least one binding");
                }
                let mut seen = std::collections::HashSet::new();
                let mut parsed_bindings = Vec::new();
                for b in bindings {
                    let (name, expr) = parse_bind(b);
                    if !seen.insert(name.clone()) {
                        panic!("Duplicate binding");
                    }
                    parsed_bindings.push((name, expr));
                }
                Expr::Let(parsed_bindings, Box::new(parse_expr(body)))
            }
            _ => panic!("Invalid"),
        },

        _ => panic!("Invalid"),
    }
}

// Result is left in RAX. offset = 8 * si (bytes below RSP).
fn compile_to_instrs(e: &Expr, si: i32, env: &HashMap<String, i32>) -> Vec<String> {
    match e {
        Expr::Number(n) => vec![format!("  mov rax, {}", n)],

        Expr::Id(name) => match env.get(name) {
            Some(offset) => vec![format!("  mov rax, [rsp - {}]", offset)],
            None => panic!("Unbound variable identifier {name}"),
        },

        Expr::UnOp(op, subexpr) => {
            let mut instrs = compile_to_instrs(subexpr, si, env);
            match op {
                Op1::Add1 => instrs.push("  add rax, 1".to_string()),
                Op1::Sub1 => instrs.push("  sub rax, 1".to_string()),
            }
            instrs
        }

        Expr::BinOp(op, left, right) => {
            let offset = 8 * si;
            let mut instrs = compile_to_instrs(left, si, env);
            instrs.push(format!("  mov [rsp - {}], rax", offset));
            instrs.extend(compile_to_instrs(right, si + 1, env));
            match op {
                Op2::Plus => instrs.push(format!("  add rax, [rsp - {}]", offset)),
                Op2::Minus => {
                    // rax = left - right; rbx holds right while we reload left
                    instrs.push(format!("  mov rbx, rax"));
                    instrs.push(format!("  mov rax, [rsp - {}]", offset));
                    instrs.push("  sub rax, rbx".to_string());
                }
                Op2::Times => instrs.push(format!("  imul rax, [rsp - {}]", offset)),
            }
            instrs
        }

        Expr::Let(bindings, body) => {
            let mut instrs = Vec::new();
            let mut current_env = env.clone();
            let mut current_si = si;
            for (name, expr) in bindings {
                let offset = 8 * current_si;
                instrs.extend(compile_to_instrs(expr, current_si, &current_env));
                instrs.push(format!("  mov [rsp - {}], rax", offset));
                current_env = current_env.update(name.clone(), offset);
                current_si += 1;
            }
            instrs.extend(compile_to_instrs(body, current_si, &current_env));
            instrs
        }
    }
}

fn compile_expr(e: &Expr) -> String {
    compile_to_instrs(e, 2, &HashMap::new()).join("\n")
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 {
        eprintln!("Usage: {} <input.snek> <output.s>", args[0]);
        std::process::exit(1);
    }

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let sexp = parse(&in_contents).expect("Invalid s-expression syntax");
    let expr = parse_expr(&sexp);
    let result = compile_expr(&expr);

    let asm_program = format!(
        "section .text
global our_code_starts_here
our_code_starts_here:
{}
  ret
",
        result
    );

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}
