//! This module holds the definitions and
//! common functions for working with the
//! AST for the proof assistant.

use egg::{Id, Symbol};

// TODO: Instead of an egg::Language, this should just use Box<Expr> as children
// and be a regular syntax tree.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Expr {
    /// A variable like the `x` in `x + 1`.
    Var(Symbol),
    /// A function. Written `x: t => y`
    Func(Symbol, [Id; 2]),
    /// A function `x: t -> y`.
    /// Sometimes written `t -> y`.
    FuncType(Symbol, [Id; 2]),
    /// An application expression is of the form
    /// `func arg`. It is left-associative so
    /// `func arg1 arg2` is the same as
    /// `(func arg1) arg2`
    App([Id; 2]),
}

impl egg::Language for Expr {
    fn matches(&self, other: &Self) -> bool {
        use Expr::*;
        match (self, other) {
            (Var(x), Var(y)) => x == y,
            (Func(_, _), Func(_, _)) => true,
            (FuncType(_, _), FuncType(_, _)) => true,
            (App(_), App(_)) => true,
            _ => false,
        }
    }

    fn children(&self) -> &[Id] {
        match self {
            Expr::Var(_) => &[],
            Expr::Func(_, children) => children,
            Expr::FuncType(_, children) => children,
            Expr::App(children) => children,
        }
    }

    fn children_mut(&mut self) -> &mut [Id] {
        match self {
            Expr::Var(_) => &mut [],
            Expr::Func(_, children) => children,
            Expr::FuncType(_, children) => children,
            Expr::App(children) => children,
        }
    }
}


// TODO: Read this whole impl from start to finish!
impl Expr {
    /*
    /// `matching_names` is a mapping from `other`'s names to `self`'s names.
    fn alpha_equal_by(
        &self,
        other: &Self,
        matching_names: &mut HashMap<Symbol, Vec<Symbol>>,
    ) -> bool {
        match (self, other) {
            (Expr::Var(a), Expr::Var(b)) => {
                // Two variables expressions match if they have the same name
                // after mapping through `matching_names`.
                Some(a) == matching_names.get(b).and_then(|x| x.last())
            }
            (Expr::App(a, b), Expr::App(x, y)) => {
                a.alpha_equal_by(x, matching_names) && b.alpha_equal_by(y, matching_names)
            }
            (Expr::Func(a, b, c), Expr::Func(x, y, z))
            | (Expr::FuncType(a, b, c), Expr::FuncType(x, y, z)) => {
                if !matching_names.contains_key(x) {
                    matching_names.insert(*x, vec![]);
                }
                matching_names[x].push(*a);
                let ret =
                    b.alpha_equal_by(y, matching_names) && c.alpha_equal_by(z, matching_names);
                matching_names[x].pop();
                if matching_names[x].is_empty() {
                    matching_names.remove(x);
                }
                ret
            }
            _ => false,
        }
        todo!()
    }
    */

    /*
    /// Returns true if"f the two expressions are totally equal, up to
    /// the names of the variables inside functions and function types.
    ///
    /// So, for example, `x => x` and `y => y` are alpha-equal.
    pub fn alpha_equal(&self, other: &Self) -> bool {
        match (self, other) {
            (Expr::Var(a), Expr::Var(b)) => a == b,
            (Expr::Func(a, b, c), Expr::Func(x, y, z)) => {
                a == x && b.alpha_equal(y) && c.alpha_equal(z)
            }
            (Expr::FuncType(a, b, c), Expr::FuncType(x, y, z)) => {
                a == x && b.alpha_equal(y) && c.alpha_equal(z)
            }
            (Expr::App(a, b), Expr::App(x, y)) => a.alpha_equal(x) && b.alpha_equal(y),
            _ => false,
        }
        todo!()
    }
    */

    /*
    fn expr_to_name_resolved(
        &self,
        symbol_mapping: &mut HashMap<Symbol, Vec<Symbol>>,
        known_names: &mut HashMap<Symbol, Id>, // Ids in the RecExpr we are building
        output: &mut RecExpr<NameResolved>,
    ) -> Result<NameResolved, NameResolutionError> {
        match self {
            Expr::Var(name) => {
                let name = symbol_mapping.get(
            }
            Expr::Func(param_name, param_type, body) => {
                let param_type = param_type.resolve_names(known_names, output);
                let arg = body.resolve_names(known_names, output);
                NameResolved::Func(*param_name, [param_type, arg])
            }
            Expr::FuncType(name, func, arg) => {
                let func = func.resolve_names(known_names, output);
                let arg = arg.resolve_names(known_names, output);
                NameResolved::FuncType(*name, [func, arg])
            }
            Expr::App(_, _) => NameResolved::App([0, 0]),
        }
    }

    fn resolve_names(
        &self,
        known_names: &mut HashMap<Symbol, Id>,
        output: &mut RecExpr<NameResolved>,
    ) -> Id {
        match self {
            Expr::Var(name) => {
                let id = output.add(NameResolved::Var(*name));
            }
            Expr::Func(_, _, _) => todo!(),
            Expr::FuncType(_, _, _) => todo!(),
            Expr::App(_, _) => todo!(),
        }
    }
    */
}
