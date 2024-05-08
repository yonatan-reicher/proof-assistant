mod lex;
mod parse;
mod ast;
mod name_resolution;
mod eval;
mod typecheck;

use name_resolution::NameResolved;

use egg::{self, EGraph, RecExpr, Symbol};
use std::collections::{hash_map::Entry, HashMap, HashSet};
use std::rc::Rc;
use std::env::args;

fn main() {
    if args().len() == 2 {
        let input = args().nth(1).unwrap();
        let tokens = lex::lex(&input);
        let ast = parse::parse(&tokens).unwrap();
        dbg!(&ast);
        // TODO: This is a hack!
        let root = egg::Id::from(ast.as_ref().len() - 1);
        let mut name_resolved = RecExpr::default();
        let root = NameResolved::resolve(
            &ast,
            root,
            &["type"],
            &mut name_resolved
        ).unwrap();
        dbg!(name_resolved);
        let typ = typecheck::infer(todo!());
        dbg!(typ);
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
    use super::*;

    /*
    fn func(param: impl Into<Symbol>, typ: Expr, body: Expr) -> Expr {
        Expr::Arrow(ArrowKind::Value, param.into(), Rc::new(typ), Rc::new(body))
    }

    fn app(f: Expr, x: Expr) -> Expr {
        Expr::App(Rc::new(f), Rc::new(x))
    }

    #[test]
    fn test_eval_application() {
        use Expr::*;
        let mut symbols = SymbolTableImpl(HashMap::new());
        symbols.insert("Prop".into(), None);
        let ast = func(
            "x",
            Var("Prop".into()),
            app(
                func("y", Var("Prop".into()), Var("y".into())),
                Var("x".into()),
            ),
        );
        let x = ast.eval(&mut symbols);
        assert_eq!(x, Ok(func("x", Var("Prop".into()), Var("x".into()),)));
    }

    #[test]
    fn test_eval_shadowing() {
        use Expr::*;
        let mut symbols = SymbolTableImpl(HashMap::new());
        symbols.insert("Prop".into(), None);
        let ast = func(
            "x",
            Var("Prop".into()),
            app(
                func("x", Var("Prop".into()), Var("x".into())),
                Var("x".into()),
            ),
        );
        let x = ast.eval(&mut symbols);
        assert_eq!(x, Ok(func("x", Var("Prop".into()), Var("x".into()),)));
    }
    */
}
