//! This module holds the definitions and
//! common functions for working with the
//! AST for the proof assistant.

use egg::{Id, Symbol};
use std::collections::HashMap;
use std::rc::Rc;

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
    /// Gets the Weak Head Normal Form of the expression, with a adding
    /// `(param_name, value)` to the context.
    fn whnf_with(
        self: &Rc<Expr>,
        param_name: Symbol,
        value: Option<Rc<Expr>>,
        known_names: &mut HashMap<Symbol, Vec<Option<Rc<Expr>>>>,
    ) -> Rc<Expr> {
        /*
        // Make an empty list at param_name if it doesn't exist
        if !known_names.contains_key(&param_name) {
            known_names.insert(param_name.clone(), vec![]);
        }
        known_names[&param_name].push(value);
        let ret = self.whnf(known_names);
        // And make sure to pop it off when we're done!
        known_names[&param_name].pop();
        if known_names[&param_name].is_empty() {
            known_names.remove(&param_name);
        }
        ret
        */
        todo!()
    }

    /// Get the Weak Head Normal Form of the expression.
    /// This is the normal form of an expression. That means that for any two
    /// expressions `a` and `b`, if `a` and `b` are *functionally equivalent*,
    /// (i.e. they match the same outputs for the same inputs) then `a` and `b`
    /// are mapped to the WHNFs that are equal (by alpha-equivalence).
    pub fn whnf(
        self: &Rc<Self>,
        known_names: &mut HashMap<Symbol, Vec<Option<Rc<Expr>>>>,
    ) -> Rc<Self> {
        /*
        match self.as_ref() {
            Expr::Var(symbol) => known_names
                .get(symbol)
                .and_then(|x| x.last())
                .and_then(|x| x.clone())
                .unwrap_or(self.clone()),
            Expr::Func(param_name, param_type, body) => {
                // TODO: Implement this reduction
                // `x => f x` is the same as `f`
                let param_name = *param_name;
                let param_type = param_type.whnf(known_names);
                let body = body.whnf_with(param_name, None, known_names);

                Rc::new(Expr::Func(param_name, param_type, body))
            }
            Expr::FuncType(param_name, param_type, body_type) => {
                let param_name = *param_name;
                let param_type = param_type.whnf(known_names);
                let body_type = body_type.whnf_with(param_name, None, known_names);

                Rc::new(Expr::FuncType(param_name, param_type, body_type))
            }
            Expr::App(func, arg) => {
                let func = func.whnf(known_names);
                let arg = arg.whnf(known_names);

                if let Expr::Func(param_name, param_type, body) = func.as_ref() {
                    body.whnf_with(*param_name, Some(arg), known_names)
                } else {
                    Rc::new(Expr::App(func, arg))
                }
            }
        }
        */
        todo!()
    }

    /// `matching_names` is a mapping from `other`'s names to `self`'s names.
    fn alpha_equal_by(
        &self,
        other: &Self,
        matching_names: &mut HashMap<Symbol, Vec<Symbol>>,
    ) -> bool {
        /*
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
        */
        todo!()
    }

    /// Returns true if"f the two expressions are totally equal, up to
    /// the names of the variables inside functions and function types.
    ///
    /// So, for example, `x => x` and `y => y` are alpha-equal.
    pub fn alpha_equal(&self, other: &Self) -> bool {
        /*
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
        */
        todo!()
    }

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
