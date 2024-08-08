//! Contains the Analysis to get the free variables of an expression.

use super::var_folding::Data as VarFoldingData;
use crate::name_resolution::{DeBrujin, NameResolved as Lang};
use egg::{Analysis, EGraph, Id};

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct FreeVariables;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Data(
    pub Vec<DeBrujin>, /* Should this be a hashmap? I don't think
                       because most of the time it is going to be
                       a very small vector */
);

impl AsRef<Data> for Data {
    fn as_ref(&self) -> &Data {
        self
    }
}

impl AsMut<Data> for Data {
    fn as_mut(&mut self) -> &mut Data {
        self
    }
}

impl AsRef<Vec<DeBrujin>> for Data {
    fn as_ref(&self) -> &Vec<DeBrujin> {
        &self.0
    }
}

impl AsMut<Vec<DeBrujin>> for Data {
    fn as_mut(&mut self) -> &mut Vec<DeBrujin> {
        &mut self.0
    }
}

#[derive(Debug, Clone, Copy)]
enum AddOrSub {
    Add,
    Sub,
}

fn offset_vec<'a>(
    var: DeBrujin,
    add_or_sub: AddOrSub,
    vec: impl Iterator<Item = &'a usize> + 'a,
) -> impl Iterator<Item = usize> + 'a {
    vec.map(move |&x| {
        if x >= var {
            match add_or_sub {
                AddOrSub::Add => x + 1,
                AddOrSub::Sub => x - 1,
            }
        } else {
            x
        }
    })
}

fn offset<A>(
    egraph: &EGraph<Lang, A>,
    expr: Id,
    var: DeBrujin,
    add_or_sub: AddOrSub,
) -> Vec<DeBrujin>
where
    A: Analysis<Lang>,
    A::Data: AsRef<Data>,
{
    let inner_free_vars = egraph[expr].data.as_ref();
    offset_vec(var, add_or_sub, inner_free_vars.0.iter()).collect()
}

fn do_let_free_vars(
    var: DeBrujin,
    // Free variables of whatever var is set to
    arg: &[DeBrujin],
    // Free variables of the body the var is set in
    body: &[DeBrujin],
) -> Vec<DeBrujin> {
    let mut v = body.iter()
        .flat_map(|&v| {
            use std::cmp::Ordering::*;
            match v.cmp(&var) {
                Less => vec![v],
                Equal => arg.to_vec(),
                Greater => vec![v - 1],
            }
        })
        .collect::<Vec<_>>();
    v.sort();
    v.dedup();
    v
}

// Returns the free variables of a let expression.
fn let_free_vars<A>(
    egraph: &egg::EGraph<Lang, A>,
    var: DeBrujin,
    arg: Id,
    body: Id,
) -> Vec<DeBrujin>
where
    A: Analysis<Lang>,
    A::Data: AsRef<Data>,
{
    let arg_free_vars = egraph[arg].data.as_ref();
    let body_free_vars = egraph[body].data.as_ref();
    do_let_free_vars(var, &arg_free_vars.0, &body_free_vars.0)
}

impl FreeVariables {
    pub fn make<A>(egraph: &egg::EGraph<Lang, A>, enode: &Lang) -> Data
    where
        A: Analysis<Lang>,
        A::Data: AsRef<Data> + AsRef<VarFoldingData>,
        egg::EGraph<Lang, A>: Clone,
    {
        let expect_var = |id: Id| {
            let vf: &VarFoldingData = egraph[id].data.as_ref();
            if vf.0.clone().and_then(|x| x.ok()).is_none() {
                let mut a = egg::RecExpr::default();
                a.add(Lang::Var(0));
                let mut b = egg::RecExpr::default();
                b.add(Lang::Var(1));
                let x = egraph.clone();
                let mut e = x.clone().explain_equivalence(&a, &b);
                println!("==================\n{:?}", &egraph[id]);
                println!("Explanation:");
                println!("{}", e.get_flat_string());
            }
            vf.0.clone().expect("Expected a variable node").unwrap()
        };

        Data(match *enode {
            Lang::Var(debrujin) => vec![debrujin],
            Lang::IncreaseVars([var, inner]) => {
                offset(egraph, inner, expect_var(var), AddOrSub::Add)
            }
            Lang::Let([var, arg, body]) => let_free_vars(egraph, expect_var(var), arg, body),
            Lang::Func([typ, body]) | Lang::FuncType([typ, body]) => {
                let type_free_vars: &Data = egraph[typ].data.as_ref();
                let body_free_vars: &Data = egraph[body].data.as_ref();
                let body_free_vars_offset =
                    body_free_vars.0.iter().filter_map(|x| x.checked_sub(1));
                type_free_vars
                    .0
                    .iter()
                    .cloned()
                    .chain(body_free_vars_offset)
                    .collect()
            }
            Lang::App([left, right]) => {
                let left_free: &Data = egraph[left].data.as_ref();
                let right_free: &Data = egraph[right].data.as_ref();
                left_free.0.iter().chain(&right_free.0).cloned().collect()
            }
            Lang::Type => vec![],
            // Lang::TypeOf(inner) => egraph[inner].data.clone(),
        })
    }

    pub fn merge(&mut self, a: &mut Data, b: Data) -> egg::DidMerge {
        // TODO: How do we merge free variables of unioned terms?
        //
        // Idea #1: The merge should be the intersection of a and b.
        // This is not correct!
        // Let's say we union the nodes (a + b) and (c + b). Then it is possible
        // that a always equals c and thus the free variables can be *not* just b.
        //
        // Idea #2: Try picking the subset. If no subset, then return the concatenation.

        fn is_subset(left: &Data, right: &Data) -> bool {
            left.0.iter().all(|x| right.0.contains(x))
        }

        if is_subset(a, &b) {
            egg::DidMerge(false, a.0.len() != b.0.len())
        } else if is_subset(&b, a) {
            *a = b;
            egg::DidMerge(true, false)
        } else {
            a.0.extend(b.0);
            a.0.sort();
            a.0.dedup();
            egg::DidMerge(true, true)
        }

        /* This is Idea #1.
        let (a_len, b_len) = (a.len(), b.len());
        a.retain(|x| b.contains(x));
        let len = a.len();
        egg::DidMerge(a_len != len, b_len != len)
        */
    }
}

/*
impl Analysis<Lang> for FreeVariables {
    type Data = Data;

    fn make(egraph: &EGraph<Lang, Self>, enode: &Lang) -> Data {
        Self::make(egraph, enode)
    }

    fn merge(&mut self, a: &mut Data, b: Data) -> egg::DidMerge {
        self.merge(a, b)
    }
}
*/

#[cfg(test)]
mod tests {
    use super::super::{Analysis as A, Data as D};
    use super::*;
    use Lang::*;
    type EGraph = egg::EGraph<Lang, A>;

    #[test]
    pub fn test_free_vars_of_var() {
        let mut e = EGraph::default();
        let var0 = e.add(Var(0));
        assert_eq!(e[var0].data.free_variables.0, vec![0]);
    }

    #[test]
    pub fn test_free_vars_of_increase_vars_above() {
        let mut e = EGraph::default();
        let var0 = e.add(Var(0));
        let var5 = e.add(Var(5));
        let inc = e.add(IncreaseVars([var0, var5]));
        assert_eq!(e[inc].data.free_variables.0, vec![6]);
    }

    #[test]
    pub fn test_free_vars_of_increase_vars_below() {
        let mut e = EGraph::default();
        let var0 = e.add(Var(0));
        let var5 = e.add(Var(5));
        let inc = e.add(IncreaseVars([var5, var0]));
        assert_eq!(e[inc].data.free_variables.0, vec![0]);
    }

    #[test]
    pub fn test_free_vars_of_increase_vars_equal() {
        let mut e = EGraph::default();
        let var5 = e.add(Var(5));
        let inc = e.add(IncreaseVars([var5, var5]));
        assert_eq!(e[inc].data.free_variables.0, vec![6]);
    }

    #[test]
    pub fn test_free_vars_of_lam() {
        let mut e = EGraph::default();
        let var3 = e.add(Var(3));
        let var6 = e.add(Var(6));
        let lam = e.add(Func([var3, var6]));
        assert_eq!(e[lam].data.free_variables.0, vec![3, 5]);
    }

    #[test]
    pub fn test_free_vars_of_pi() {
        let mut e = EGraph::default();
        let var3 = e.add(Var(3));
        let var6 = e.add(Var(6));
        let pi = e.add(FuncType([var3, var6]));
        assert_eq!(e[pi].data.free_variables.0, vec![3, 5]);
    }

    #[test]
    pub fn test_free_vars_of_let_different_var() {
        let mut e = EGraph::default();
        let var3 = e.add(Var(3));
        let var6 = e.add(Var(6));
        let var9 = e.add(Var(9));
        let pi = e.add(Let([var3, var6, var9]));
        assert_eq!(e[pi].data.free_variables.0, vec![8]);
    }

    #[test]
    pub fn test_free_vars_of_let_same_var() {
        let mut e = EGraph::default();
        let var3 = e.add(Var(3));
        let var6 = e.add(Var(6));
        let pi = e.add(Let([var3, var6, var3]));
        assert_eq!(e[pi].data.free_variables.0, vec![6]);
    }

    #[test]
    pub fn test_free_vars_of_let_below_var() {
        let mut e = EGraph::default();
        let var9 = e.add(Var(9));
        let var6 = e.add(Var(6));
        let var3 = e.add(Var(3));
        let pi = e.add(Let([var9, var6, var3]));
        assert_eq!(e[pi].data.free_variables.0, vec![3]);
    }

    #[test]
    pub fn test_free_vars_of_app_in_let() {
        let mut e = EGraph::default();
        // let f = (x => y) in f z
        let var0 = e.add(Var(0));
        let var1 = e.add(Var(1));
        let var2 = e.add(Var(2));
        let typ= e.add(Type);
        let f = e.add(Func([typ, var1]));
        let app = e.add(App([var0, var2]));
        let l = e.add(Let([var0, f, app]));
        println!("{:?}", e.dump());
        assert_eq!(e[l].data.free_variables.0, vec![0, 1]);
    }
}
