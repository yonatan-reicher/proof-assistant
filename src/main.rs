mod ast;
mod eval;
mod free_variables;
mod lex;
mod name_resolution;
mod parse;
mod typecheck;

use name_resolution::NameResolved;

use egg::{self, RecExpr, Id};
use std::env::args;

fn run_str(input: &str) {
    let tokens = lex::lex(input);
    let ast = parse::parse(&tokens).unwrap();
    // TODO: This is a hack!
    let root = Id::from(ast.as_ref().len() - 1);
    let mut name_resolved = RecExpr::default();
    let root = NameResolved::resolve(&ast, root, &["type"], &mut name_resolved).unwrap();
    let mut t = typecheck::TypeChecker::new(&mut name_resolved, []);
    let typ = t.infer(root).unwrap();
    dbg!(typ);
}

fn run_cli_argument() {
    let input = args().nth(1).unwrap();
    run_str(&input);
}

fn repl() {
    todo!(concat![
        "Currently no repl support. Run with a single argument in a string ",
        "instead.",
    ]);
}

fn main() {
    if args().len() == 2 {
        run_cli_argument();
    } else {
        repl();
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
