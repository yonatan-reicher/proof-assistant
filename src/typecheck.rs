//! This module is all about types.
//! This proof assistant is based on CoC, where types are terms and also values.
//! This means that type checking is
//! 1) Undecidable (Can get into an infinite loop).
//! 2) The types will be terms.

use crate::analysis::Analysis;
use crate::name_resolution::{DeBrujin, NameResolved};
use crate::pretty::PrettyPrintOwned;
use egg::{Id, RecExpr};
use thiserror::Error;

/// Type = Expr = term.
type Expr = NameResolved;
type Type = NameResolved;
type Rec = RecExpr<NameResolved>;

#[derive(Error, Debug, Clone)]
pub enum TypeError {
    #[error("Type mismatch: {expression} has type {had_type}, but expected {expected_type}")]
    TypeMismatch {
        expression: PrettyPrintOwned,
        had_type: PrettyPrintOwned,
        expected_type: PrettyPrintOwned,
    },
    #[error("Type {0} is not a function type")]
    NotAFunctionType(PrettyPrintOwned),
    #[error("Type {0} is not a type")]
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

    pub fn get(&self, debrujin: DeBrujin, expr: &mut Rec) -> Id {
        let x = self.type_stack.iter().rev().nth(debrujin).copied().unwrap();
        let var0 = expr.add(NameResolved::Var(0));

        let mut curr = x;
        for _ in 0..=debrujin {
            curr = expr.add(NameResolved::IncreaseVars([var0, curr]));
        }
        curr
    }

    pub fn push(&mut self, typ: Id) {
        self.type_stack.push(typ);
    }

    pub fn pop(&mut self) -> Option<Id> {
        self.type_stack.pop()
    }
}

type EGraph = egg::EGraph<Type, Analysis>;

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
        EGraph::default()
    }

    pub fn new(expr: &'a mut Rec, types: impl IntoIterator<Item = Id>) -> TypeChecker {
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
            //NameResolved::DecreaseVars(..) => todo!(),
            NameResolved::Var(debrujin) => Ok(self.types.get(debrujin, self.expr)),
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
                let Some((param_type, ret_type)) = ({
                    // We want to use the egraph to simplify func_type and check
                    // if it is a function type.
                    let mut rec = self.expr.clone();
                    rec.add(*func_type);
                    self.is_type_function_type(&rec)
                }) else {
                    return Err(TypeError::NotAFunctionType(PrettyPrintOwned::new(
                        self.expr.clone(),
                        func_type_id,
                        true,
                    )));
                };
                self.typecheck(arg, param_type)?;
                // TODO: This is wrong. Because ret_type was created in a
                // context where there was one more variable. So we need to set
                // it's 0th variable and decrease it's vars.
                Ok(ret_type)
            }
            NameResolved::Let([var, value, expr]) => {
                todo!()
            }
            /*
            NameResolved::Set0([expr, subst]) => {
                let type_of_0 = self.types.get(0, self.expr);
                self.typecheck(type_of_0, subst)?;
                self.infer(expr)
            }
            */
        }
    }
    
    fn is_type_function_type(&mut self, a: &Rec) -> Option<(Id, Id)> {
        let a_class = self.egraph.add_expr(a);
        self.egraph.rebuild();
        crate::eval::run(&mut self.egraph);

        dbg!(&self.egraph);
        dbg!(&self.egraph[a_class]);
        dbg!(&self.egraph[self.egraph.find(a_class)]
            .data)
            .is_function_type
            .map(|func_type_info| (func_type_info.arg_type, func_type_info.ret_type))
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
            Err(TypeError::TypeMismatch {
                expression: PrettyPrintOwned::new(self.expr.clone(), root, true),
                had_type: PrettyPrintOwned::new(root_type_rec, root_type, true),
                //had_type: root_type_rec,
                expected_type: PrettyPrintOwned::new(against_rec, against, true),
            })
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
