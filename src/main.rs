mod ast;

use egg::{self, EGraph, RecExpr, Symbol};
use std::collections::{hash_map::Entry, HashMap, HashSet};
use std::rc::Rc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ArrowKind {
    Value,
    Type,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum Expr {
    Var(Symbol),
    Arrow(ArrowKind, Symbol, Rc<Expr>, Rc<Expr>),
    App(Rc<Expr>, Rc<Expr>),
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum EvalError {
    FreeVar(Symbol),
    NotAFunction(Expr),
}

pub type EvalResult<T> = Result<T, EvalError>;

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum TypeError {
    TypeMismatch(Rc<Expr>, Rc<Expr>),
    NotAFunctionType(Expr),
    UnboundVar(Symbol),
}

pub type TypeResult<T> = Result<T, TypeError>;

pub trait SymbolTable<T> {
    fn lookup(&self, s: Symbol) -> Option<&T>;
    fn insert(&mut self, s: Symbol, t: T);
    fn remove(&mut self, s: Symbol);
}

impl Expr {
    pub fn free_vars(&self) -> HashSet<Symbol> {
        match self {
            Expr::Var(v) => [*v].into_iter().collect(),
            Expr::Arrow(_, param, typ, body) => {
                let mut ret = body.free_vars();
                ret.remove(param);
                ret = ret.union(&typ.free_vars()).cloned().collect();
                ret
            }
            Expr::App(f, x) => f.free_vars().union(&x.free_vars()).cloned().collect(),
        }
    }

    pub fn is_free_var(&self, v: &Symbol) -> bool {
        self.free_vars().contains(v)
    }

    pub fn eval(&self, symbols: &mut impl SymbolTable<Option<Expr>>) -> EvalResult<Expr> {
        match self {
            Expr::Var(v) => match symbols.lookup(*v) {
                // Some(Some(e)) => e.clone().eval(symbols),
                // TODO: Is this correct? Should the substitution be evaluated
                // again in the current scope with the current symbols?
                Some(Some(e)) => Ok(e.clone()),
                Some(None) => Ok(self.clone()),
                None => Err(EvalError::FreeVar(*v)),
            },
            Expr::Arrow(kind, param, typ, body) => {
                symbols.insert(*param, None);
                let typ = typ.eval(symbols)?;
                let body = body.eval(symbols)?;
                symbols.remove(*param);
                Ok(Expr::Arrow(*kind, *param, Rc::new(typ), Rc::new(body)))
            }
            Expr::App(f, x) => {
                let f = f.eval(symbols)?;
                let x = x.eval(symbols)?;
                let Expr::Arrow(ArrowKind::Value, param, _, body) = f else {
                    return Err(EvalError::NotAFunction(f));
                };

                symbols.insert(param, Some(x));
                let ret = body.eval(symbols)?;
                symbols.remove(param);
                Ok(ret)
            }
        }
    }

    pub fn expect_function_type(&self, typ: Rc<Expr>) -> TypeResult<()> {
        match self {}
    }

    pub fn typecheck(
        &self,
        against: Rc<Expr>,
        symbols: &mut impl SymbolTable<Rc<Expr>>,
    ) -> TypeResult<()> {
        match self {
            Expr::Var(v) => match symbols.lookup(*v) {
                Some(t) => {
                    if t == &against {
                        Ok(())
                    } else {
                        Err(TypeError::TypeMismatch(against.clone(), t.clone()))
                    }
                }
                None => Err(TypeError::UnboundVar(*v)),
            },
            Expr::Arrow(ArrowKind::Value, param, typ, body) => {
                match against {
                    Expr::Var(_) => todo!(),
                    Expr::Arrow(_, _, _, _) => todo!(),
                    Expr::App(_, _) => todo!(),
                }

                symbols.insert(*param, typ.clone());
                body.typecheck(against, symbols)?;
                symbols.remove(*param);
                Ok(())
            }
        }
    }

    pub fn infer(&self, symbols: &mut impl SymbolTable<Rc<Expr>>) -> TypeResult<Rc<Expr>> {
        match self {
            Expr::Var(v) => match symbols.lookup(*v) {
                Some(t) => Ok(t.clone()),
                None => Err(EvalError::FreeVar(*v)),
            },
            Expr::Arrow(ArrowKind::Value, param, typ, body) => {
                symbols.insert(*param, typ.clone());
                let body_type = body.infer(symbols)?;
                symbols.remove(*param);
                Ok(Rc::new(Expr::Arrow(
                    ArrowKind::Type,
                    *param,
                    typ.clone(),
                    body_type,
                )))
            }
            Expr::Arrow(ArrowKind::Type, param, typ, ret_type) => {
                symbols.insert(*param, typ.clone());
                let ret_type_type = ret_type.infer(symbols)?;
                symbols.remove(*param);
                Ok(Rc::new(Expr::Arrow(
                    ArrowKind::Type,
                    *param,
                    typ.clone(),
                    ret_type.clone(),
                )))
            }
            Expr::App(func, arg) => {
                let func_type = func.infer(symbols)?;
                let Expr::Arrow(ArrowKind::Type, param, param_type, body_type) = *func_type else {
                    return Err(EvalError::NotAFunction(func.clone()));
                };
            }
        }
    }
}

pub struct SymbolTableImpl<T>(HashMap<Symbol, (T, Vec<T>)>);

impl<T> SymbolTable<T> for SymbolTableImpl<T> {
    fn lookup(&self, s: Symbol) -> Option<&T> {
        self.0.get(&s).map(|(t1, ts)| ts.last().unwrap_or(t1))
    }

    fn insert(&mut self, s: Symbol, t: T) {
        match self.0.entry(s) {
            Entry::Occupied(mut e) => e.get_mut().1.push(t),
            Entry::Vacant(e) => {
                e.insert((t, vec![]));
            }
        }
    }

    fn remove(&mut self, s: Symbol) {
        match self.0.entry(s) {
            Entry::Occupied(e) if e.get().1.is_empty() => {
                e.remove();
            }
            Entry::Occupied(mut e) => {
                e.get_mut().1.pop();
            }
            Entry::Vacant(_) => {}
        }
    }
}

fn main() {
    let mut symbols = SymbolTableImpl(HashMap::new());
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
}

#[cfg(test)]
mod tests {
    use super::*;

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
}
