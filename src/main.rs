mod analysis;
mod ast;
mod command;
mod eval;
mod is_function_type;
mod lex;
mod name_resolution;
mod parse;
mod pretty;
mod typecheck;

use analysis::Analysis;
use name_resolution::NameResolved;
use pretty::Pretty;

use egg::{self, Id, RecExpr};
use std::{env::args, fs, io, path::PathBuf};
use thiserror::Error;

fn get_root<L: egg::Language>(e: &RecExpr<L>) -> Id {
    // Get the root node's Id! (This is a hack~)
    Id::from(e.as_ref().len() - 1)
}

fn run_str(input: &str) {
    let tokens = lex::lex(input);
    let ast = parse::parse(&tokens).unwrap();

    let mut name_resolved_ast = RecExpr::default();
    let root =
        NameResolved::resolve(&ast, get_root(&ast), &["type"], &mut name_resolved_ast).unwrap();
    println!(
        "Name Resolved Ast: {}",
        Pretty::new(&name_resolved_ast, root)
    );

    let type_literal = name_resolved_ast.add(NameResolved::Type);
    let mut typechecker = typecheck::TypeChecker::new(&mut name_resolved_ast, [type_literal]);
    let typ = typechecker.infer(root).unwrap_or_else(|e| {
        panic!("Type error: {}", e);
    });
    println!("Type: {}", Pretty::new(&name_resolved_ast, typ));

    let mut egraph = egg::EGraph::<NameResolved, Analysis>::default();
    // If we were just adding the name-resolved ast, we would get the id of the
    // wrong node. We add our root node again so egg treats it as the root.
    name_resolved_ast.add(name_resolved_ast[root]);
    let root = egraph.add_expr(&name_resolved_ast);
    egraph.rebuild();

    let expr = eval::run_and_extract(&mut egraph, root);
    let root = get_root(&expr);

    println!("Evaluated: {}", Pretty::new(&expr, root));
    println!("Graph: ");
    println!("{:#?}", egraph.dump());
}

fn repl() {
    todo!(concat![
        "Currently no repl support. Run with a single argument in a string ",
        "instead.",
    ]);
}

fn print_help(arg: Option<&str>) {
    match arg {
        None => println!(
            "\
Proof assistant based on CoC using E-Graphs for evaluation

Usage: untiteldexe.exe [COMMAND]

Commands:
    run     Run a file or a string
    repl    Start a REPL session, WIP
    help    Print this help page

Tip: Run help <command> to find out more
"
        ),
        Some("run") => println!(
            "\
Run a file or a string

Usage: unstuanstoan run [FILENAME]
       unstuanstoan run --str [STRING]
"
        ),
        Some("repl") => println!(
            "\
Starts a REPL - an interactive session

Note: Right now does not work. WIP
"
        ),
        Some("help") => print_help(None),
        Some(bad) => println!("Unknown command '{bad}'"),
    }
}

fn get_code(run_mode: command::RunMode) -> io::Result<String> {
    match run_mode {
        command::RunMode::String(code) => Ok(code),
        command::RunMode::File(filename) => fs::read_to_string(filename),
    }
}

fn do_run(run_mode: command::RunMode) -> io::Result<()> {
    let code = get_code(run_mode)?;
    run_str(&code);
    Ok(())
}

type Error = Box<dyn std::error::Error>;

fn f() -> Result<(), Error> {
    use command::Command::*;
    match command::parse(args())? {
        Help(arg) => print_help(arg.as_deref()),
        Repl => repl(),
        Run(run_mode) => do_run(run_mode)?,
    };
    Ok(())
}

fn main() {
    match f() {
        Ok(()) => (),
        Err(e) => {
            eprintln!("{e}");
        }
    }

    /*
    symbols.insert("Prop".into(), None);
    let ast = Expr::Arrow(
        ArrowKind::Value,
        "x".into(),
        Rc::new(Expr::Var("Prop".into())),
        Expr::App(
            Expr::Arrow(
                ArrowKind::Value,
                "y".into(),
                Rc::new(Expr::Var("Prop".into())),
                Rc::new(Expr::Var("y".into())),
            )
            .into(),
            Rc::new(Expr::Var("x".into())),
        )
        .into(),
    );
    let x = ast.eval(&mut symbols);
    dbg!(ast);
    dbg!(x);
    */
}

#[cfg(test)]
mod tests {
    // TODO:
}
