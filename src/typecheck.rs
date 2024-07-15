//! This module is all about types.
//! This proof assistant is based on CoC, where types are terms and also values.
//! This means that type checking is
//! 1) Undecidable (Can get into an infinite loop).
//! 2) The types will be terms.

use crate::name_resolution::{DeBrujin, NameResolved};
use crate::free_variables::FreeVariables;
use egg::{Id, RecExpr};

/// Type = Expr = term.
type Expr = NameResolved;
type Type = NameResolved;
type Rec = RecExpr<NameResolved>;

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

// TODO: Unify with the structure from src\name_resolution.rs.
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

type EGraph = egg::EGraph<Type, FreeVariables>;

pub struct TypeChecker<'a> {
    // TODO: Seperate this to two expressions: One for the input expression,
    // and one for the output types.
    expr: &'a mut Rec,
    types: Types,
    /// An Id that points to an eclass that has `NameResolved::Type`.
    type_literal: Id,
    /// This EGraph is used for comparing terms which represent types.
    /// Normally, when checking that two types might be equal, we first need to
    /// apply beta and eta reductions to get the normalest form of the term.
    /// Instead, we just chuck it at the egraph and rerun the rules, then we
    /// can check if the eclasses are equal.
    egraph: EGraph,
}

impl<'a> TypeChecker<'a> {
    fn initial_egraph() -> EGraph {
        let mut egraph = EGraph::default();
        let typ = egraph.add(NameResolved::Type);
        let var0 = egraph.add(NameResolved::Var(0));
        egraph.union(typ, var0);
        egraph.rebuild();
        egraph
    }

    pub fn new(
        expr: &'a mut Rec,
        types: impl IntoIterator<Item = Id>,
    ) -> TypeChecker {
        let type_literal = expr.add(NameResolved::Type);
        Self {
            expr,
            type_literal,
            types: Types::new(types),
            egraph: Self::initial_egraph(),
        }
    }

    pub fn infer(&mut self, root: Id) -> TypeResult<Id> {
        match self.expr[root] {
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
                    .expr
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
                let func_type = &self.expr[func_type_id];
                let &NameResolved::FuncType([param_type, ret_type]) = func_type else {
                    todo!();
                };
                self.typecheck(arg, param_type)?;
                Ok(ret_type)
            }
            NameResolved::Set0([expr, subst]) => {
                let type_of_0 = self.types.get(0);
                self.typecheck(type_of_0, subst)?;
                self.infer(expr)
            }
        }
    }

    fn are_types_the_same(&mut self, a: &Rec, b: &Rec) -> bool {
        let a_class = self.egraph.add_expr(a);
        let b_class = self.egraph.add_expr(b);
        self.egraph.rebuild();
        crate::eval::run(&mut self.egraph);

        self.egraph.find(a_class) == self.egraph.find(b_class)
    }

    /// Typechecks `root` against `against`.
    ///
    /// # What is typechecking?
    /// In this bi-directional type system, typechecking is the process of
    /// finding if the type of `root` *might* be the type in id `against`.
    pub fn typecheck(&mut self, root: Id, against: Id) -> TypeResult<()> {
        let root_type = self.infer(root)?;
        
        let mut root_type_rec = self.expr.clone();
        root_type_rec.add(self.expr[root_type]);
        let mut against_rec = self.expr.clone();
        against_rec.add(self.expr[against]);
        let same_type = self.are_types_the_same(&root_type_rec, &against_rec);

        if same_type {
            Ok(())
        } else {
            Err(TypeError::TypeMismatch(root, against))
        }

        /*
        match self.expr[root] {
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
        */
    }
}
