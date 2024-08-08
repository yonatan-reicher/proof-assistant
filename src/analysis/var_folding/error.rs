use crate::name_resolution::DeBrujin;
use egg::Id;
use std::rc::Rc;
use thiserror::Error;

#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum Error {
    #[error("confliciting variables {0} and {1} in this eclass")]
    Conflict(DeBrujin, DeBrujin),
    #[error("expected the eclass {0} to be a threshold in an IncreaseVars but it does not fold to a variable.")]
    EClassWasNotAThreshold(Id),
    #[error("expected the eclass {0} to be a variable in an IncreaseVars but it does not fold to a variable.")]
    EClassWasNotAVar(Id),
    #[error("multiple errors. First: {}", .0.as_ref()[0])]
    Combine(Rc<[Error; 2]>),
}

impl Error {
    pub fn combine(self, other: Self) -> Self {
        Error::Combine([self, other].into())
    }
}
