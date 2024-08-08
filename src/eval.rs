use crate::{analysis::Analysis, name_resolution::NameResolved};
use egg::{rewrite, Applier, EGraph, Id, RecExpr, Rewrite, Subst, Symbol};

type Lang = NameResolved;

pub fn rules() -> Vec<Rewrite<Lang, Analysis>> {
    let mut v = vec![beta_reduction(), eta_reduction()];
    v.extend(let_reduction());
    v.extend(inc_vars_reduction());
    v
}

pub fn run(egraph: &mut EGraph<Lang, Analysis>) {
    let egraph_moved = std::mem::take(egraph);
    let mut runner = egg::Runner::default().with_egraph(egraph_moved);
    runner = runner.run(&rules());
    *egraph = runner.egraph;
}

pub fn run_and_extract(egraph: &mut EGraph<Lang, Analysis>, id: Id) -> RecExpr<NameResolved> {
    run(egraph);
    let extractor = egg::Extractor::new(egraph, egg::AstSize);
    let (_cost, expr) = extractor.find_best(id);
    expr
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
    above: egg::Var,
}

impl Applier<Lang, Analysis> for VarApplier {
    fn vars(&self) -> Vec<egg::Var> {
        vec![self.var]
    }

    fn apply_one(
        &self,
        egraph: &mut EGraph<Lang, Analysis>,
        eclass: Id,
        subst: &Subst,
        _searcher_ast: Option<&egg::PatternAst<Lang>>,
        _rule_name: Symbol,
    ) -> Vec<Id> {
        // Expects a pattern var to be an eclass of a var node
        let expect_var_class = |pattern_var: egg::Var| {
            egraph[subst[pattern_var]]
                .data
                .var
                .0
                .clone()
                .expect("This was not a var")
                .unwrap()
        };

        let var = expect_var_class(self.var);
        let above = expect_var_class(self.above);
        let index_to_add = if var >= above {
            self.inc_or_dec.offset(var)
        } else {
            None
        };
        let Some(new_index) = index_to_add else {
            return vec![];
        };
        let new_node = egraph.add(Lang::Var(new_index));
        if let Some(searcher_ast) = _searcher_ast {
            let mut s = subst.clone();
            s.insert("?X".parse().unwrap(), new_node);
            egraph.union_instantiations(searcher_ast, &"?X".parse().unwrap(), &s, _rule_name);
        } else {
            egraph.union(eclass, new_node);
        }
        vec![new_node]
    }
}

pub fn beta_reduction<T: egg::Analysis<Lang>>() -> Rewrite<Lang, T> {
    rewrite!(
        "beta-reduction";
        "(app (lam ?T ?body) ?arg)"
        // => "(dec-vars (set0 ?body (inc-vars ?arg)))"
        => "(let 0 ?arg ?body)"
    )
}

/// Not free = not used / doesn't affect the output.
fn var_0_is_not_free(
    body: &'static str,
) -> impl Fn(&mut EGraph<Lang, Analysis>, Id, &Subst) -> bool {
    let body = body.parse().unwrap();

    move |egraph, _, subst| {
        let id = subst[body];
        let class = &egraph[id];
        !class.data.free_variables.0.contains(&0)
    }
}

fn is_var(var: &'static str) -> impl Fn(&mut EGraph<Lang, Analysis>, Id, &Subst) -> bool {
    let var = var.parse().unwrap();

    move |egraph, _, subst| egraph[subst[var]].data.var.0.is_some()
}

fn are_different_vars(
    a: &'static str,
    b: &'static str,
) -> impl Fn(&mut EGraph<Lang, Analysis>, Id, &Subst) -> bool {
    let a = a.parse().unwrap();
    let b = b.parse().unwrap();

    move |egraph, _, subst| {
        let a = egraph[subst[a]].data.var.0.as_ref();
        let b = egraph[subst[b]].data.var.0.as_ref();
        a.zip(b).is_some_and(|(a, b)| a != b)
    }
}

pub fn eta_reduction() -> Rewrite<NameResolved, Analysis> {
    rewrite!(
        "eta-reduction";
        "(lam ?T (app ?body 0))"
        // => "(dec-vars ?body)"
        => "(let 0 0 ?body)" // Using let to decrease the variables
        if var_0_is_not_free("?body")
    )
}

fn not_etable(var: &'static str) -> impl Fn(&mut EGraph<Lang, Analysis>, Id, &Subst) -> bool {
    let var = var.parse().unwrap();

    move |egraph, _, subst| {
        let eclass = &egraph[subst[var]];
        let eta_pattern_found = eclass.iter().any(|&n| {
            let Lang::App([l, r]) = n else {
                return false;
            };
            !egraph[l].data.free_variables.0.contains(&0) && egraph[r].data.var.0 == Some(Ok(0))
        });
        !eta_pattern_found
    }
}

pub fn let_reduction() -> Vec<Rewrite<NameResolved, Analysis>> {
    let dec_v = VarApplier {
        var: "?V2".parse().unwrap(),
        inc_or_dec: IncOrDec::Dec,
        // TODO:
        above: "?V1".parse().unwrap(),
    };
    vec![
        rewrite!(
            "let-same-var";
            "(let ?V ?a ?V)" => "?a" if is_var("?V")
        ),
        rewrite!(
            "let-different-var";
            "(let ?V1 ?a ?V2)" => dec_v if are_different_vars("?V1", "?V2")
        ),
        rewrite!(
            "let-type";
            "(let ?V ?a type)" => "type"
        ),
        rewrite!(
            "let-lam";
            "(let ?V ?arg (lam ?T ?B))"
            => "(lam (let ?V ?arg ?T) (let (inc-vars 0 ?V) (inc-vars 0 ?arg) ?B))"
            if is_var("?V")
            if not_etable("?B")
        ),
        rewrite!(
            "let-pi";
            "(let ?V ?arg (pi ?T ?B))"
            => "(pi (let ?V ?arg ?T) (let (inc-vars 0 ?V) (inc-vars 0 ?arg) ?B))"
            if is_var("?V")
        ),
        rewrite!(
            "let-app";
            "(let ?V ?a (app ?func ?arg))"
            => "(app (let ?V ?a ?func) (let ?V ?a ?arg))"
            if is_var("?V")
        ),
    ]
}

pub fn inc_vars_reduction() -> Vec<Rewrite<Lang, Analysis>> {
    let inc_v = VarApplier {
        var: "?V2".parse().unwrap(),
        inc_or_dec: IncOrDec::Inc,
        above: "?V1".parse().unwrap(),
    };
    vec![
        rewrite!(
            "inc-vars-var";
            "(inc-vars ?V1 ?V2)" => inc_v
            if is_var("?V1")
            if is_var("?V2")
        ),
        rewrite!(
            "inc-vars-type";
            "(inc-vars ?V type)" => "type" if is_var("?V")
        ),
        rewrite!(
            "inc-vars-lam";
            "(inc-vars ?V (lam ?T ?B))"
            => "(lam (inc-vars ?V ?T) (inc-vars (inc-vars 0 ?V) ?B))"
        ),
        rewrite!(
            "inc-vars-pi";
            "(inc-vars ?V (pi ?T ?B))"
            => "(pi (inc-vars ?V ?T) (inc-vars (inc-vars 0 ?V) ?B))"
        ),
        rewrite!(
            "inc-vars-app";
            "(inc-vars ?V (app ?func ?arg))"
            => "(app (inc-vars ?V ?func) (inc-vars ?V ?arg))"
        ),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    type EGraph = egg::EGraph<NameResolved, Analysis>;
    use NameResolved::*;

    #[test]
    fn test_beta_reduction_single() {
        let mut egraph = EGraph::default();
        let var0 = egraph.add(Var(0));
        let var2 = egraph.add(Var(2));
        let typ = egraph.add(Type);
        let func = egraph.add(Func([typ, var0]));
        let app = egraph.add(App([func, var2]));
        println!("Added some nody nodes");
        egraph.rebuild();
        println!("Rebuilt!");

        run(&mut egraph);
        println!("Ran :)");

        assert_eq!(egraph.find(app), egraph.find(var2));
    }

    #[test]
    fn test_beta_reduction_a_little() {
        let mut egraph = EGraph::default().with_explanations_enabled();
        //let mut egraph = EGraph::default();
        let var0 = egraph.add(Var(0));
        let var1 = egraph.add(Var(1));
        let var2 = egraph.add(Var(2));
        let typ = egraph.add(Type);

        let func0 = egraph.add(Func([typ, var0]));
        let func1 = egraph.add(Func([typ, var1]));
        let func2 = egraph.add(Func([typ, var2]));
        let _func00 = egraph.add(Func([typ, func0]));
        let _func11 = egraph.add(Func([typ, func1]));
        let _func22 = egraph.add(Func([typ, func2]));
        println!("Adding simple things - Success");

        // (x => x) a0
        let app00 = egraph.add(App([func0, var0]));
        /*
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
        */
        println!("Adding complex things - Success");

        egraph.rebuild();
        println!("Rebuilding - Success");
        let runner = egg::Runner::default()
            .with_egraph(egraph)
            .with_hook(|runner| {
                dbg!(&runner.egraph.dump());
                Ok(())
            })
            .run(&rules());
        println!("Running - Success");
        let egraph = runner.egraph;
        dbg!(&egraph.dump());

        assert_eq!(egraph[app00].id, egraph[var0].id);
    }

    #[test]
    fn test_beta_reduction_many() {
        let mut egraph = EGraph::default().with_explanations_enabled();
        //let mut egraph = EGraph::default();
        let var0 = egraph.add(Var(0));
        let var1 = egraph.add(Var(1));
        let var2 = egraph.add(Var(2));
        let typ = egraph.add(Type);

        let func0 = egraph.add(Func([typ, var0]));
        let func1 = egraph.add(Func([typ, var1]));
        let func2 = egraph.add(Func([typ, var2]));
        let _func00 = egraph.add(Func([typ, func0]));
        let _func11 = egraph.add(Func([typ, func1]));
        let _func22 = egraph.add(Func([typ, func2]));
        println!("Adding simple things - Success");

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
        println!("Adding complex things - Success");

        egraph.rebuild();
        println!("Rebuilding - Success");
        let runner = egg::Runner::default()
            .with_egraph(egraph)
            .with_hook(|runner| {
                dbg!(&runner.egraph.dump());
                Ok(())
            })
            .run(&rules());
        println!("Running - Success");
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
