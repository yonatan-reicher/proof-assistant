use crate::name_resolution::{DeBrujin, NameResolved as Lang};
use egg::{Analysis, EGraph, Id};

mod error;
pub use error::Error;
type Res<T> = Result<T, Error>;

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct VarFolding;

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Data(pub Option<Res<DeBrujin>>);

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

fn increase_var(threshold: DeBrujin, var: DeBrujin) -> DeBrujin {
    if var >= threshold {
        var + 1
    } else {
        var
    }
}

fn increase_var_result(threshold: &Res<DeBrujin>, var: &Res<DeBrujin>) -> Res<DeBrujin> {
    match (threshold, var) {
        (Ok(t), Ok(v)) => Ok(increase_var(*t, *v)),
        (Err(t), Err(v)) => Err(t.clone().combine(v.clone())),
        (Err(e), Ok(_)) | (Ok(_), Err(e)) => Err(e.clone()),
    }
}

// TODO: Rename this enum and it's cases
enum IncVarBaddness {
    ThresholdBad,
    VarNotFolded,
    BothGood(
        Res<DeBrujin>, /* There can still be an error nested inside one of them */
    ),
}

fn increase_var_data(threshold: &Data, var: &Data) -> IncVarBaddness {
    use IncVarBaddness::*;

    match (&threshold.0, &var.0) {
        (None, _) => ThresholdBad,
        (_, None) => VarNotFolded,
        (Some(t), Some(v)) => BothGood(increase_var_result(t, v)),
    }
}

fn make_increase_vars<A>(egraph: &EGraph<Lang, A>, threshold_class_id: Id, var_class_id: Id) -> Data
where
    A: Analysis<Lang>,
    A::Data: AsRef<Data>,
{
    let threshold = egraph[threshold_class_id].data.as_ref();
    let var = egraph[var_class_id].data.as_ref();

    let threshold_err = Error::EClassWasNotAThreshold(threshold_class_id);

    use IncVarBaddness::*;
    let ret = match increase_var_data(threshold, var) {
        ThresholdBad => Some(Err(threshold_err)),
        VarNotFolded => None,
        BothGood(result) => Some(result),
    };
    Data(ret)
}

impl VarFolding {
    pub fn make<A>(egraph: &EGraph<Lang, A>, enode: &Lang) -> Data
    where
        A: Analysis<Lang>,
        A::Data: AsRef<Data>,
    {
        match *enode {
            Lang::Var(v) => Data(Some(Ok(v))),
            Lang::IncreaseVars([threshold, var]) => make_increase_vars(egraph, threshold, var),
            _ => Data(None),
        }
    }

    pub fn merge(a: &mut Data, b: &Data) -> egg::DidMerge {
        let nop = egg::DidMerge(false, false);
        let a_changed = egg::DidMerge(true, false);
        let b_changed = egg::DidMerge(false, true);
        let both = egg::DidMerge(true, true);
        let (new_a, ret) = match (a.0.clone(), b.0.clone()) {
            (Some(a), None) => (Some(a.clone()), b_changed),
            (None, Some(b)) => (Some(b.clone()), a_changed),
            (None, None) => (None, nop),
            (Some(Err(a)), Some(Err(b))) => (Some(Err(a.combine(b))), nop), // should it be both
                                                                             // here?
            (Some(Err(e)), _) => (Some(Err(e)), b_changed),
            (_, Some(Err(e))) => (Some(Err(e)), a_changed),
            (Some(Ok(a)), Some(Ok(b))) => {
                if a != b {
                    (Some(Err(Error::Conflict(a, b))), both)
                } else {
                    (Some(Ok(a)), nop)
                }
            }
        };
        a.0 = new_a;
        ret
    }
}

impl Analysis<Lang> for VarFolding {
    type Data = Data;

    fn make(egraph: &EGraph<Lang, Self>, enode: &Lang) -> Self::Data {
        Self::make(egraph, enode)
    }

    fn merge(&mut self, a: &mut Self::Data, b: Self::Data) -> egg::DidMerge {
        Self::merge(a, &b)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_make_var() {
        let egraph = EGraph::new(VarFolding);
        let data = VarFolding::make(&egraph, &Lang::Var(6));
        assert_eq!(data, Data(Some(Ok(6))));
    }

    #[test]
    fn test_make_inc_var_above() {
        let mut egraph = EGraph::new(VarFolding);
        let var6 = egraph.add(Lang::Var(6));
        let var4 = egraph.add(Lang::Var(4));
        let data = VarFolding::make(&egraph, &Lang::IncreaseVars([var4, var6]));
        assert_eq!(data, Data(Some(Ok(7))));
    }

    #[test]
    fn test_make_inc_var_equal() {
        let mut egraph = EGraph::new(VarFolding);
        let var6 = egraph.add(Lang::Var(6));
        let data = VarFolding::make(&egraph, &Lang::IncreaseVars([var6, var6]));
        assert_eq!(data, Data(Some(Ok(7))));
    }

    #[test]
    fn test_make_inc_var_below() {
        let mut egraph = EGraph::new(VarFolding);
        let var6 = egraph.add(Lang::Var(6));
        let var4 = egraph.add(Lang::Var(4));
        let data = VarFolding::make(&egraph, &Lang::IncreaseVars([var6, var4]));
        assert_eq!(data, Data(Some(Ok(4))));
    }

    #[test]
    fn test_make_inc_var_nested() {
        let mut egraph = EGraph::new(VarFolding);
        let var6 = egraph.add(Lang::Var(6));
        let var0 = egraph.add(Lang::Var(0));
        let inc_var6 = egraph.add(Lang::IncreaseVars([var0, var6]));
        let data = VarFolding::make(&egraph, &Lang::IncreaseVars([var0, inc_var6]));
        assert_eq!(data, Data(Some(Ok(8))));
    }

    #[test]
    fn test_make_inc_var_nested_both_args() {
        let mut egraph = EGraph::new(VarFolding);
        let var6 = egraph.add(Lang::Var(6));
        let var0 = egraph.add(Lang::Var(0));
        let inc_var6 = egraph.add(Lang::IncreaseVars([var0, var6]));
        let inc_var6_deeper = egraph.add(Lang::IncreaseVars([inc_var6, var0]));
        let even_deeper = egraph.add(Lang::IncreaseVars([inc_var6, inc_var6]));
        let fin = Lang::IncreaseVars([inc_var6_deeper, even_deeper]);
        let data = VarFolding::make(&egraph, &fin);
        assert_eq!(data, Data(Some(Ok(9))));
    }

    #[test]
    fn test_with_non_var() {
        let mut egraph = EGraph::new(VarFolding);
        let var0 = egraph.add(Lang::Var(0));
        let func = egraph.add(Lang::Let([var0, var0, var0]));
        assert_eq!(egraph[var0].data.0, Some(Ok(0)));
        assert_eq!(egraph[func].data.0, None);
        egraph.union(var0, func);
        egraph.rebuild();
        assert_eq!(egraph[var0].data.0, Some(Ok(0)));
        assert_eq!(egraph[func].data.0, Some(Ok(0)));
    }
}
