//! This module is all about types.
//! This proof assistant is based on CoC, where types are terms and also values.
//! This means that type checking is
//! 1) Undecidable (Can get into an infinite loop).
//! 2) The types will be terms.

use crate::name_resolution::{DeBrujin, NameResolved};
use crate::free_variables::FreeVariables as A; // A for Analysis.
use egg::{Id, RecExpr};

/// Type = Expr = term.
type Expr = NameResolved;
type Type = NameResolved;

type EGraph = egg::EGraph<Expr, A>;

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum TypeError {
    TypeMismatch(Id, Id),
    NotAFunctionType(Id),
    ParameterTypeAnnotationIsNotAType(Id),
}

pub type TypeResult<T> = Result<T, TypeError>;

/// This holds the types of variables.
pub struct Types {
    type_stack: Vec<Id>,
}

// TODO: Unify with the structure from src\eval.rs or src\name_resolution.rs or
//  something.
impl Types {
    pub fn new(types: impl IntoIterator<Item = Id>) -> Self {
        Self {
            type_stack: types.into_iter().collect(),
        }
    }

    pub fn get(&self, debrujin: DeBrujin) -> Id {
        self.type_stack.iter().rev().nth(debrujin).copied().unwrap()
    }

    pub fn push(&mut self, typ: Id) {
        self.type_stack.push(typ);
    }

    pub fn pop(&mut self) -> Option<Id> {
        self.type_stack.pop()
    }
}

/*
pub struct TypeChecker<'a> {
    egraph: &'a mut EGraph,
    types: Types,
    /// An Id that points to an eclass that has `NameResolved::Type`.
    type_literal: Id,
}

impl TypeChecker<'_> {
    pub fn new(
        egraph: &mut EGraph,
        types: impl IntoIterator<Item = Id>,
    ) -> TypeChecker {
        let type_literal = egraph.add(NameResolved::Type);
        TypeChecker {
            egraph,
            type_literal,
            types: Types::new(types),
        }
    }

    pub fn infer(&mut self, root: Id) -> TypeResult<Id> {
        match self.egraph[root] {
            NameResolved::Type => {
                // TODO: This is wrong. To fix this, we need to implement
                // universes.
                Ok(root)
            }
            NameResolved::IncreaseVars(..) => todo!(),
            NameResolved::DecreaseVars(..) => todo!(),
            NameResolved::Var(debrujin) => Ok(self.types.get(debrujin)),
            NameResolved::Func([param_type, ret]) => {
                // TODO: Maybe support doing something like:
                // self.typecheck(...).map_err(hint("This must be a type"))?;
                self.typecheck(param_type, self.type_literal)?;
                self.types.push(param_type);
                let ret_type = self.infer(ret)?;
                self.types.pop();
                Ok(self
                    .egraph
                    .add(NameResolved::FuncType([param_type, ret_type])))
            }
            NameResolved::FuncType(_) => Ok(self.type_literal),
            NameResolved::App([func, arg]) => {
                // I deciced to infer the type of the function and typecheck the
                // argument. You can also do the other way around, and infer the
                // argument and typecheck the function. This would require a
                // special typecheck_function function to typecheck the parameter
                // and then infer the return type.
                // TODO: Do both, and return whichever one works.
                let func_type_id = self.infer(func)?;
                let func_type = &self.egraph[func_type_id];
                let &NameResolved::FuncType([param_type, ret_type]) = func_type else {
                    todo!();
                };
                self.typecheck(arg, param_type)?;
                Ok(ret_type)
            }
            NameResolved::Set0([expr, subst]) => {
                let type_of_0 = self.types.get(0);
                self.typecheck(type_of_0, subst);
                self.infer(expr)
            }
        }
    }

    /// Typechecks `root` against `against`.
    ///
    /// # What is typechecking?
    /// In this bi-directional type system, typechecking is the process of
    /// finding if the type of `root` *might* be the type in id `against`.
    pub fn typecheck(&mut self, root: Id, against: Id) -> TypeResult<()> {
        match self.egraph[root] {
            NameResolved::Var(debrujin) => {
                let typ = self.types.get(debrujin);
                //if typ != against {
                //}
                todo!()
            }
            NameResolved::Func(_) => todo!(),
            NameResolved::FuncType(_) => todo!(),
            NameResolved::App(_) => todo!(),
            NameResolved::Type => todo!(),
            NameResolved::IncreaseVars(..) => todo!(),
            NameResolved::DecreaseVars(..) => todo!(),
            NameResolved::Set0(_) => todo!(),
        }
    }
}
*/
