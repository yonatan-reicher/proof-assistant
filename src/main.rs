mod ast;
mod pretty;
mod eval;
mod free_variables;
mod lex;
mod name_resolution;
mod parse;
mod typecheck;

use free_variables::FreeVariables;
use name_resolution::NameResolved;
use pretty::Pretty;

use egg::{self, RecExpr, Id};
use std::env::args;

fn get_root<L: egg::Language>(e: &RecExpr<L>) -> Id {
    // Get the root node's Id! (This is a hack~)
    Id::from(e.as_ref().len() - 1)
}

fn run_str(input: &str) {
    let tokens = lex::lex(input);
    let ast = parse::parse(&tokens).unwrap();

    let mut name_resolved_ast = RecExpr::default();
    let root = NameResolved::resolve(&ast, get_root(&ast), &["type"], &mut name_resolved_ast).unwrap();
    println!("Name Resolved Ast: {}", Pretty::new(&name_resolved_ast, root));

    let type_literal = name_resolved_ast.add(NameResolved::Type);
    let mut typechecker = typecheck::TypeChecker::new(
        &mut name_resolved_ast,
        [ type_literal ],
    );
    let typ = typechecker.infer(root).unwrap();
    println!("Type: {}", Pretty::new(&name_resolved_ast, typ));

    let mut egraph = egg::EGraph::<NameResolved, FreeVariables>::default();
    // If we were just adding the name-resolved ast, we would get the id of the
    // wrong node. We add our root node again so egg treats it as the root.
    name_resolved_ast.add(name_resolved_ast[root]);
    println!("full expr: {:#?}", name_resolved_ast);
    let root = egraph.add_expr(&name_resolved_ast);
    egraph.rebuild();

    let expr = eval::run_and_extract(&mut egraph, root);
    let root = get_root(&expr);

    println!("Evaluated: {}", Pretty::new(&expr, root));
    println!("Graph: {:#?}", egraph);
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
