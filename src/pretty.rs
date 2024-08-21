//! This module is all about the pretty printing name resolved expressions!
//!
//! This module exposes the `PrettyPrinter` type, and the `PrettyPrint` trait to create it from egg
//! expressions.

use crate::name_resolution::{DeBrujin, NameResolved};
use egg::{Id, RecExpr};
use std::fmt::{self, Debug, Display, Formatter};

type Rec = RecExpr<NameResolved>;

pub trait PrettyPrint: Sized {
    fn pretty_print(&self) -> PrettyPrinter;

    fn pretty_print_at(&self, root: Id) -> PrettyPrinter;
}

impl PrettyPrint for Rec {
    fn pretty_print(&self) -> PrettyPrinter {
        let root = Id::from(self.as_ref().len() - 1);
        self.pretty_print_at(root)
    }

    fn pretty_print_at(&self, root: Id) -> PrettyPrinter {
        PrettyPrinter {
            expr: self,
            root,
            depth: 0,
            as_atom: false,
        }
    }
}

#[derive(Clone)]
pub struct PrettyPrintOwned {
    expr: Rec,
    id: Id,
    as_atom: bool,
}

impl PrettyPrint for PrettyPrintOwned {
    fn pretty_print(&self) -> PrettyPrinter {
        self.pretty_print_at(self.id)
    }

    fn pretty_print_at(&self, root: Id) -> PrettyPrinter {
        self.expr.pretty_print_at(root).set_as_atom(self.as_atom)
    }
}

impl Debug for PrettyPrintOwned {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        Debug::fmt(&self.pretty_print(), f)
    }
}

impl Display for PrettyPrintOwned {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        Display::fmt(&self.pretty_print(), f)
    }
}

impl PrettyPrintOwned {
    pub fn new(expr: Rec, id: Id, as_atom: bool) -> Self {
        Self { expr, id, as_atom }
    }
}

#[derive(Clone, Copy)]
pub struct PrettyPrinter<'a> {
    expr: &'a Rec,
    root: Id,
    depth: usize,
    as_atom: bool,
}

impl<'a> PrettyPrinter<'a> {
    pub fn as_atom(&self) -> Self {
        Self {
            as_atom: true,
            ..*self
        }
    }

    pub fn not_as_atom(&self) -> Self {
        Self {
            as_atom: false,
            ..*self
        }
    }

    pub fn set_as_atom(&self, v: bool) -> Self {
        if v { self.as_atom() } else { self.not_as_atom() }
    }

    pub fn inc_depth(&self) -> Self {
        Self {
            depth: self.depth + 1,
            ..*self
        }
    }

    pub fn at_root(&self, root: Id) -> Self {
        Self { root, ..*self }
    }

    // Starts at 0. Goes a..za'..z'a''...
    fn generate_name_for_depth(depth: usize) -> String {
        let mut iter = 'a'..='z';
        let size = iter.clone().count();
        if depth < size {
            iter.nth(depth).unwrap().to_string()
        } else {
            Self::generate_name_for_depth(depth - size) + "'"
        }
    }

    // Gets the name at the given DeBrujin index at the current depth.
    fn get_name(&self, index: DeBrujin) -> String {
        if index >= self.depth {
            // Free variables
            Self {
                depth: index + 1,
                ..*self
            }
            .get_name(self.depth)
            .to_uppercase()
        } else {
            let var_depth = self.depth - index - 1;
            Self::generate_name_for_depth(var_depth)
        }
    }

    fn next_var_name(&self) -> String {
        Self::generate_name_for_depth(self.depth)
    }

    fn needs_parenthesis(&self) -> bool {
        use NameResolved::*;
        self.as_atom
            && match &self.expr[self.root] {
                Var(_) | Type => false,
                Func(_) | FuncType(_) | Let(_) | App(_) => true,
                IncreaseVars([_, e]) => self.at_root(*e).needs_parenthesis(),
            }
    }

    fn binder_str(&self) -> Option<&'static str> {
        match self.expr[self.root] {
            NameResolved::Func(_) => Some("=>"),
            NameResolved::FuncType(_) => Some("->"),
            _ => None,
        }
    }

    fn print_expr(&self, f: &mut Formatter) -> fmt::Result {
        use NameResolved::*;
        match self.expr[self.root] {
            Var(index) => {
                let name = self.get_name(index);
                write!(f, "{}", name)?;
            }
            Func([param_type, right]) | FuncType([param_type, right]) => {
                write!(f, "{}: ", self.next_var_name())?;
                self.at_root(param_type).as_atom().print(f)?;
                write!(f, " {} ", self.binder_str().unwrap())?;
                self.inc_depth().at_root(right).print_expr(f)?;
                // Depth + 1 because we added the parameter!
            }
            App([func, arg]) => {
                self.as_atom().at_root(func).print(f)?;
                write!(f, " ")?;
                self.as_atom().at_root(arg).print(f)?;
            }
            Type => write!(f, "type")?,
            IncreaseVars([var, x]) => {
                write!(f, "inc-vars ")?;
                self.as_atom().at_root(var).print(f)?;
                write!(f, " ")?;
                self.at_root(x).print_expr(f)?;
            }
            Let([var, value, expr]) => {
                todo!()
                /*
                // let {var} = {value} in {expr}
                let var = expect_var(var);
                let var_name = var_name_from_depth(var_depth(depth, var));
                write!(out, "let {} = ", var_name).unwrap();
                pretty_atom(expr, *value, out, depth);
                write!(out, " in ").unwrap();
                pretty(expr, *expr, out, depth);
                */
            } // NR::DecreaseVars(_) => todo!(),
        }
        Ok(())
    }

    fn print(&self, f: &mut Formatter) -> fmt::Result {
        if self.needs_parenthesis() {
            write!(f, "(");
        }

        self.print_expr(f)?;

        if self.needs_parenthesis() {
            write!(f, ")");
        }

        Ok(())
    }
}

impl Debug for PrettyPrinter<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Pretty(")?;
        Display::fmt(&self.not_as_atom(), f)?;
        write!(f, ")")?;
        Ok(())
    }
}

impl Display for PrettyPrinter<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.print(f)
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::name_resolution::NameResolved as NR;

    #[test]
    fn test1() {
        // if we have a reference to var 0 at the top level, we want to call it
        // var a (i guess). and then the next new variable should be called var b.
        let mut expr = Rec::default();
        let var0 = expr.add(NR::Var(0));
        let var1 = expr.add(NR::Var(1));
        expr.add(NR::Func([var0, var1]));
        let pretty = expr.pretty_print().as_atom().to_string();
        assert_eq!(pretty, "(a: A => A)");
    }

    fn parse_and_pretty(str: &'static str) -> String {
        use crate::lex::lex;
        use crate::name_resolution::NameResolved;
        use crate::parse::parse;
        let tokens = lex(str);
        let expr = parse(&tokens).unwrap();
        let root = Id::from(expr.as_ref().len() - 1);
        let mut name_resolved = RecExpr::default();
        let root = NameResolved::resolve(&expr, root, &[], &mut name_resolved).unwrap();
        name_resolved.pretty_print_at(root).to_string()
    }

    #[test]
    fn test_large_expression() {
        let pretty = parse_and_pretty(
            "
            (x: (y: type -> type) => x) (a : type => type)
        ",
        );
        let actual = "
            (a: (a: type -> type) => a) (a: type => type)
        ";
        // Var 0 is displayed as 'a'.
        assert_eq!(pretty.trim(), actual.trim());
    }

    #[test]
    fn with_and_without_var() {
        let pretty_a = parse_and_pretty(
            "
            (type -> type) -> type
        ",
        );
        let pretty_b = parse_and_pretty(
            "
            aa: (a: type -> type) -> type
        ",
        );
        assert_eq!(pretty_a, pretty_b);
    }
}
