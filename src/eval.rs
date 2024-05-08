use egg::{Id, RecExpr, Rewrite, rewrite, Symbol, EGraph, Subst};
use crate::name_resolution::{NameResolved, DeBrujin};

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum EvalError {
    FreeVar(Symbol),
    NotAFunction(Id),
}

pub type EvalResult<T> = Result<T, EvalError>;

struct Context {
    expr: RecExpr<NameResolved>,
    stack: Vec<Id>,
}

impl Context {
    pub fn get(&self, debrujin: DeBrujin) -> Option<Id> {
        self.stack.iter().rev().nth(debrujin).copied()
    }

    pub fn beta_reduction() -> Rewrite<NameResolved, ()> {
        rewrite!(
            "beta-reduction";
            "(app (func ?T ?body) ?arg)" => "?body"
        )
    }

    pub fn eta_reduction() -> Rewrite<NameResolved, ()> {
        fn var_0_is_free(body: &'static str) -> impl Fn(&mut EGraph<NameResolved, ()>, Id, &Subst) -> bool {
            let body = body.parse().unwrap();

            move |egraph, _, subst| {
                let id = subst[body];
                let class = &egraph[id];
                class.nodes.iter().any(|node| {
                    // TODO: Get the amount of free variables from an analysis.
                    todo!()
                })
            }
        }

        rewrite!(
            "eta-reduction";
            "(func ?T (app ?body 0))" => "?body" if var_0_is_free("?body") 
        )
    }

    pub fn eval(
        &mut self,
        root: Id,
    ) -> EvalResult<Id> {
        match &self.expr[root] {
            NameResolved::Var(debrujin) => {
                todo!()
            }
            NameResolved::Func(_) => todo!(),
            NameResolved::FuncType(_) => todo!(),
            NameResolved::App(_) => todo!(),
            NameResolved::Type => todo!(),
        }
    }
}


