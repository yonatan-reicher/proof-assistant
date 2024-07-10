use crate::{free_variables::FreeVariables, name_resolution::NameResolved};
use egg::{rewrite, Analysis, Applier, EGraph, Id, Rewrite, Subst, Symbol};

type Lang = NameResolved;

pub fn rules() -> Vec<Rewrite<Lang, FreeVariables>> {
    let mut v = vec![beta_reduction(), eta_reduction()];
    v.extend(set0_reduction());
    v.extend(inc_dec_vars_reduction());
    v
}

#[derive(Debug, Clone, Copy)]
enum IncOrDec {
    Inc,
    Dec,
}

impl IncOrDec {
    pub const fn offset(self, index: usize) -> Option<usize> {
        match self {
            IncOrDec::Inc => Some(index + 1),
            IncOrDec::Dec => index.checked_sub(1),
        }
    }
}

struct VarApplier {
    var: egg::Var,
    inc_or_dec: IncOrDec,
}

impl<N: Analysis<Lang>> Applier<Lang, N> for VarApplier {
    fn vars(&self) -> Vec<egg::Var> {
        vec![self.var]
    }

    fn apply_one(
        &self,
        egraph: &mut EGraph<Lang, N>,
        eclass: Id,
        subst: &Subst,
        _searcher_ast: Option<&egg::PatternAst<Lang>>,
        _rule_name: Symbol,
    ) -> Vec<Id> {
        let matched_variable_eclass = *subst.get(self.var).expect("Var not found");
        let indices_to_add = egraph[matched_variable_eclass]
            .nodes
            .iter()
            .filter_map(|node| {
                if let &Lang::Var(index) = node {
                    let new_index = self.inc_or_dec.offset(index);
                    Some(new_index)
                } else {
                    None
                }
            })
            // We collect to a vector so we can iterate the nodes while
            // modifying the egraph.
            .collect::<Vec<_>>();
        let mut ret = vec![];
        for new_index in indices_to_add {
            let Some(new_index) = new_index else { continue };
            let new_node = egraph.add(Lang::Var(new_index));
            egraph.union(eclass, new_node);
            ret.push(new_node);
        }
        ret
    }
}

pub fn beta_reduction<T: Analysis<Lang>>() -> Rewrite<Lang, T> {
    rewrite!(
        "beta-reduction";
        "(app (lam ?T ?body) ?arg)"
        => "(dec-vars (set0 ?body (inc-vars ?arg)))"
    )
}

/// Not free = not used / doesn't affect the output.
fn var_0_is_not_free(
    body: &'static str,
) -> impl Fn(&mut EGraph<Lang, FreeVariables>, Id, &Subst) -> bool {
    let body = body.parse().unwrap();

    move |egraph, _, subst| {
        let id = subst[body];
        let class = &egraph[id];
        !class.data.contains(&0)
    }
}

fn is_var(var: &'static str) -> impl Fn(&mut EGraph<Lang, FreeVariables>, Id, &Subst) -> bool {
    let var = var.parse().unwrap();

    move |egraph, _, subst| {
        // TODO: Maybe check if this has a free variable because it must if it
        // is a variable node.
        let id = subst[var];
        egraph[id]
            .nodes
            .iter()
            .any(|node| matches!(node, Lang::Var(_)))
    }
}

pub fn eta_reduction() -> Rewrite<NameResolved, FreeVariables> {
    rewrite!(
        "eta-reduction";
        "(lam ?T (app ?body 0))"
        => "(dec-vars ?body)"
        if var_0_is_not_free("?body")
    )
}

pub fn set0_reduction() -> Vec<Rewrite<NameResolved, FreeVariables>> {
    vec![
        rewrite!(
            "set0-var-0";
            "(set0 0 ?S)"
            => "?S"
        ),
        rewrite!(
            "set0-no-var-0";
            "(set0 ?E ?T)"
            => "?E"
            if var_0_is_not_free("?E")
        ),
        // TODO: set0-offest
        // Why is there no set0-set0? Because it always reduces.
        rewrite!(
            "set0-lam";
            "(set0 (lam ?T ?B) ?S)"
            => "(
                lam
                    (set0 ?T ?S)
                    (inc-vars (set0 (dec-vars ?B) ?S))
            )"
        ),
        rewrite!(
            "set0-pi";
            "(set0 (pi ?T ?B) ?S)"
            => "(
                pi
                    (set0 ?T ?S)
                    (inc-vars (set0 (dec-vars ?B) ?S))
            )"
        ),
        rewrite!(
            "set0-app";
            "(set0 (app ?F ?A) ?S)"
            => "(app (set0 ?F ?S) (set0 ?A ?S))"
        ),
    ]
}

pub fn inc_dec_vars_reduction() -> Vec<Rewrite<Lang, FreeVariables>> {
    let inc_v = VarApplier {
        var: "?V".parse().unwrap(),
        inc_or_dec: IncOrDec::Inc,
    };
    let dec_v = VarApplier {
        var: "?V".parse().unwrap(),
        inc_or_dec: IncOrDec::Dec,
    };
    vec![
        rewrite!(
            "inc-vars-var";
            "(inc-vars ?V)" => inc_v if is_var("?V")
        ),
        rewrite!(
            "dec-vars-var";
            "(dec-vars ?V)" => dec_v if is_var("?V")
        ),
        rewrite!(
            "inc-vars-dec-vars-cancel";
            "(inc-vars (dec-vars ?X))" => "?X"
        ),
        rewrite!(
            "dec-vars-inc-vars-cancel";
            "(dec-vars (inc-vars ?X))" => "?X"
        ),
        // We don't need a rule for set0 because it will reduce anyway.
        // TODO: PI and LAM
        rewrite!(
            "inc-vars-app";
            "(inc-vars (app ?F ?A))"
            => "(app (inc-vars ?F) (inc-vars ?A))"
        ),
        rewrite!(
            "dec-vars-app";
            "(dec-vars (app ?F ?A))"
            => "(app (dec-vars ?F) (dec-vars ?A))"
        ),
        rewrite!(
            "inc-vars-type";
            "(inc-vars type)" => "type"
        ),
        rewrite!(
            "dec-vars-type";
            "(dec-vars type)" => "type"
        ),
    ]
}

/*
pub fn offest_reduction() -> Rewrite<Lang, FreeVariables> {
    rewrite!(
        "offset-reduction";
        "(-1 )"
    )
}
*/

#[cfg(test)]
mod tests {
    use std::io::Write;

    use super::*;

    type EGraph = egg::EGraph<NameResolved, FreeVariables>;
    use NameResolved::*;

    #[test]
    fn test_beta_reduction() {
        let mut egraph = EGraph::default();
        let var0 = egraph.add(Var(0));
        let var1 = egraph.add(Var(1));
        let var2 = egraph.add(Var(2));
        let typ = egraph.add(Type);

        let func0 = egraph.add(Func([typ, var0]));
        let func1 = egraph.add(Func([typ, var1]));
        let func2 = egraph.add(Func([typ, var2]));
        let func00 = egraph.add(Func([typ, func0]));
        let func11 = egraph.add(Func([typ, func1]));
        let func22 = egraph.add(Func([typ, func2]));

        // (x => x) a0
        let app00 = egraph.add(App([func0, var0]));
        // (x => x) a1
        let app01 = egraph.add(App([func0, var1]));
        // (x => x) a2
        let app02 = egraph.add(App([func0, var2]));
        // (x => a0) a0
        let app10 = egraph.add(App([func1, var0]));
        // (x => a0) a1
        let app11 = egraph.add(App([func1, var1]));
        // (x => a0) a2
        let app12 = egraph.add(App([func1, var2]));
        // (x => a1) a0
        let app20 = egraph.add(App([func2, var0]));
        // (x => a1) a1
        let app21 = egraph.add(App([func2, var1]));
        // (x => a1) a2
        let app22 = egraph.add(App([func2, var2]));

        egraph.rebuild();
        let runner = egg::Runner::default().with_egraph(egraph).run(&rules());
        let egraph = runner.egraph;
        dbg!(&egraph.dump());

        assert_eq!(egraph[app00].id, egraph[var0].id);
        assert_eq!(egraph[app01].id, egraph[var1].id);
        assert_eq!(egraph[app02].id, egraph[var2].id);
        assert_eq!(egraph[app10].id, egraph[var0].id);
        assert_eq!(egraph[app11].id, egraph[var0].id);
        assert_eq!(egraph[app12].id, egraph[var0].id);
        assert_eq!(egraph[app20].id, egraph[var1].id);
        assert_eq!(egraph[app21].id, egraph[var1].id);
        assert_eq!(egraph[app22].id, egraph[var1].id);
    }
}
