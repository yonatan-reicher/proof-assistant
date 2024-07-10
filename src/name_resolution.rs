use crate::ast::Expr;
use egg::{define_language, Id, RecExpr, Symbol};

/// De-Brujin index: An index of a variable term to refer to it's variable
/// based on the order of variable declaration.
///
/// For example, `\x. \y. y x` is `\. \. 0 1`.
pub type DeBrujin = usize;

define_language! {
    #[derive(Copy)]
    pub enum NameResolved {
        // A variable like the `x` in `x + 1`.
        Var(DeBrujin),
        // Adds 1 to every D"B index of every variable.
        "inc-vars" = IncreaseVars(Id),
        // Takes 1 away from every D"B index of every variable.
        // For 0 variables, does not do anything.
        // But DecreaseVars and IncreaseVars cancel each other out.
        "dec-vars" = DecreaseVars(Id),
        // Replaces the variable at 0 of this expression.
        // The second Id is the argument for the first one.
        "set0" = Set0([Id; 2]),
        // A function. Written `x: t => y`
        "lam" = Func([Id; 2]),
        // A function `x: t -> y`.
        // Sometimes written `t -> y`.
        "pi" = FuncType([Id; 2]),
        // An application expression is of the form
        // `func arg`. It is left-associative so
        // `func arg1 arg2` is the same as
        // `(func arg1) arg2`
        "app" = App([Id; 2]),
        // TODO: Can we remove this?
        "type" = Type,

    }
}

#[cfg(test)]
mod language_from_op_tests {
    use super::*;

    #[test]
    fn test_parse_var() {
        let parsed: RecExpr<_> = "10".parse().unwrap();
        let mut expected = RecExpr::default();
        expected.add(NameResolved::Var(10));
        assert_eq!(parsed, expected);
    }

    #[test]
    fn test_parse_offset() {
        let parsed: RecExpr<_> = "(dec-vars (app 0 1))".parse().unwrap();
        let mut expected = RecExpr::default();
        let var0 = expected.add(NameResolved::Var(0));
        let var1 = expected.add(NameResolved::Var(1));
        let app = expected.add(NameResolved::App([var0, var1]));
        expected.add(NameResolved::DecreaseVars(app));
        assert_eq!(parsed, expected);
    }

    /*
    #[test]
    fn test_pattern() {
        let parsed: PatternAst<NameResolved> = "(?O (lam ?T 0))".parse().unwrap();
        let mut expected = PatternAst::<NameResolved>::default();
        let t = expected.add(Var("?T".parse().unwrap()));
        let var0 = expected.add(ENode(NameResolved::Var(0).into()));
        let lam = expected.add(NameResolved::Func([t, var0]));
        let
        expected.add();
        // let var0 = expected.add(NameResolved::Var(0))
    }
    */
}

#[derive(Debug)]
struct SymbolMap {
    // This maps each symbol to the number of symbols that existed before it.
    inner: std::collections::HashMap<Symbol, Vec<usize>>,
    // Can't use the hashmap's len because it has vectors that act as buckets.
    size: usize,
}

impl SymbolMap {
    pub fn new() -> Self {
        Self {
            inner: Default::default(),
            size: 0,
        }
    }

    pub fn add(&mut self, symbol: Symbol) {
        let items_before_insertion = self.size;
        self.size += 1;
        self.inner
            .entry(symbol)
            .or_default()
            .push(items_before_insertion);
    }

    pub fn remove(&mut self, symbol: &Symbol) {
        match self.inner.get_mut(symbol) {
            Some(vec) => {
                vec.pop();
                if vec.is_empty() {
                    self.inner.remove(symbol);
                }
                self.size -= 1;
            }
            None => panic!("Symbol not found in context: {:?}", symbol),
        }
    }

    pub fn get(&self, symbol: &Symbol) -> Option<DeBrujin> {
        let number = self.inner.get(symbol).map(|vec| *vec.last().unwrap());
        // Reminder: number = number of symbols that existed before our symbol.
        number.map(|number| self.inner.len() - number - 1)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NameResolutionError {
    SymbolNotFound(Symbol),
}

/// Resolves Func and FuncType expressions to DeBrujin indexed expressions.
///
/// # Example
/// // TODO:
/// ```rust
/// let expr = RecExpr::default();
/// let a = expr.add(Expr::Var("a".into()));
/// ```
fn resolve_arrow(
    expr: &RecExpr<Expr>,
    parameter: &Symbol,
    parameter_type: Id,
    rhs: Id,
    ctor: fn([Id; 2]) -> NameResolved,
    context: &mut SymbolMap,
    dest: &mut RecExpr<NameResolved>,
) -> Result<Id, NameResolutionError> {
    let parameter_type = resolve(expr, parameter_type, context, dest)?;

    context.add(*parameter);
    let ret_expr = resolve(expr, rhs, context, dest)?;
    context.remove(parameter);

    Ok(dest.add(ctor([parameter_type, ret_expr])))
}

fn resolve(
    expr: &RecExpr<Expr>,
    root: Id,
    context: &mut SymbolMap,
    dest: &mut RecExpr<NameResolved>,
) -> Result<Id, NameResolutionError> {
    match &expr[root] {
        Expr::Var(symbol) => {
            if let Some(debrujin) = context.get(symbol) {
                Ok(dest.add(NameResolved::Var(debrujin)))
            } else {
                Err(NameResolutionError::SymbolNotFound(*symbol))
            }
        }
        Expr::Func(parameter, [parameter_type, ret_expr]) => resolve_arrow(
            expr,
            parameter,
            *parameter_type,
            *ret_expr,
            NameResolved::Func,
            context,
            dest,
        ),
        Expr::FuncType(parameter, [parameter_type, ret_type]) => resolve_arrow(
            expr,
            parameter,
            *parameter_type,
            *ret_type,
            NameResolved::FuncType,
            context,
            dest,
        ),
        Expr::App([func, arg]) => {
            let func = resolve(expr, *func, context, dest)?;
            let arg = resolve(expr, *arg, context, dest)?;
            Ok(dest.add(NameResolved::App([func, arg])))
        }
    }
}

impl NameResolved {
    // TODO: Instead of returning id, return a new RecExpr.
    pub fn resolve(
        expr: &RecExpr<Expr>,
        root: Id,
        names: &[&str],
        name_resolved: &mut RecExpr<NameResolved>,
    ) -> Result<Id, NameResolutionError> {
        let mut context = SymbolMap::new();
        for &name in names {
            context.add(name.into());
        }
        let root = resolve(expr, root, &mut context, name_resolved)?;
        Ok(root)
    }
}
