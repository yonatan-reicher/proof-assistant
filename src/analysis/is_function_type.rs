//! This module contains the IsFunctionType analysis. This analysis is a flag
//! that is set for every eclass which contains a term which is FuncType(..).
//! Also stores an Id of the function type.

use crate::name_resolution::NameResolved as Lang;
use egg::{Analysis, EGraph, Id};

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct IsFunctionType;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FuncType {
    pub arg_type: Id,
    pub ret_type: Id,
}

pub type Data = Option<FuncType>;

// impl Analysis<Lang> for IsFunctionType {
//  type Data = Data;
impl IsFunctionType {
    pub fn make<A>(_egraph: &EGraph<Lang, A>, enode: &Lang) -> Data
        where A: Analysis<Lang>
    {
        if let &Lang::FuncType([arg_type, ret_type]) = enode {
            Some(FuncType { arg_type, ret_type })
        } else {
            None
        }
    }

    pub fn merge(&mut self, to: &mut Data, from: Data) -> egg::DidMerge {
        use egg::DidMerge as D;
        match (&to, from) {
            (None, None) => D(false, false),
            (Some(_), None) => D(false, true),
            (None, Some(x)) => {
                *to = Some(x);
                D(true, false)
            }
            (Some(_), Some(_)) => D(false, true),
        }
    }
}
