//! Contains the Analysis to get the free variables of an expression.

// TODO: Do we actually need the entire set of free variables? Do we not need only the
// 0 variable for eta reduction?

use crate::name_resolution::NameResolved;
use egg::{Analysis, EGraph, Id};

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct FreeVariables;

type Lang = NameResolved;

type Data = Vec<usize>;

fn offset(egraph: &EGraph<Lang, FreeVariables>, expr: Id, offset: isize) -> Data {
    let inner_free_vars = &egraph[expr].data;
    let mapping = |x: &usize| x.checked_add_signed(offset);
    inner_free_vars.iter().filter_map(mapping).collect()
}

impl Analysis<Lang> for FreeVariables {
    type Data = Data;

    fn make(egraph: &egg::EGraph<Lang, Self>, enode: &NameResolved) -> Data {
        match *enode {
            NameResolved::Var(debrujin) => vec![debrujin],
            NameResolved::IncreaseVars(inner) => offset(egraph, inner, 1),
            NameResolved::DecreaseVars(inner) => offset(egraph, inner, -1),
            NameResolved::Set0([expr, subs]) => {
                // Two cases: If 0 is not free in expr, then subs will not appear and thus
                // will not affect the output. If 0 is free, then subs will appear and all
                // of it's variables will not be bound.
                let expr_free_vars = &egraph[expr].data;
                let subs_free_vars = &egraph[subs].data;
                let has_0 = expr_free_vars.contains(&0);
                if has_0 {
                    let union = expr_free_vars
                        .iter()
                        .filter(|&&x| x != 0)
                        .chain(subs_free_vars)
                        .cloned()
                        .collect();
                    union
                } else {
                    expr_free_vars.clone()
                }
            }
            NameResolved::Func([typ, body]) | NameResolved::FuncType([typ, body]) => {
                let type_free_vars = &egraph[typ].data;
                let body_free_vars = &egraph[body].data;
                let body_free_vars_offset = body_free_vars.iter().filter_map(|x| x.checked_sub(1));
                type_free_vars
                    .iter()
                    .cloned()
                    .chain(body_free_vars_offset)
                    .collect()
            }
            NameResolved::App([left, right]) => {
                let left_free = &egraph[left].data;
                let right_free = &egraph[right].data;
                left_free.iter().chain(right_free).cloned().collect()
            }
            NameResolved::Type => vec![],
        }
    }

    fn merge(&mut self, a: &mut Self::Data, b: Self::Data) -> egg::DidMerge {
        // The merge should be the intersection of a and b.
        let (a_len, b_len) = (a.len(), b.len());
        a.retain(|x| b.contains(x));
        let len = a.len();
        egg::DidMerge(a_len != len, b_len != len)
    }
}
