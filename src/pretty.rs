//! This module is all about the pretty printing name resolved expressions!

use crate::name_resolution::{DeBrujin, NameResolved};
use egg::{Id, RecExpr};
use std::fmt::{self, Display, Formatter, Write};

type Rec = RecExpr<NameResolved>;

fn var_name_from_depth(depth: isize) -> String {
    let mut iter = 'a'..='z';
    let size = iter.clone().count();
    // TODO: Do something about all these conversions.
    if depth < size as _ {
        iter.nth(depth as _).unwrap().to_string()
    } else {
        var_name_from_depth(depth - size as isize) + "'"
    }
}

fn var_depth(depth: usize, index: DeBrujin) -> isize {
    // depth - index but we need to convert to isize.
    depth.abs_diff(index) as isize * (if depth < index { -1 } else { 1 })
}

fn needs_parenthesis(expr: &NameResolved) -> bool {
    use NameResolved as NR;
    match expr {
        NR::Var(_) => false,
        NR::Func(_) => true,
        NR::FuncType(_) => true,
        NR::App(_) => true,
        NR::Type => false,
        NR::IncreaseVars(_) => true,
        NR::DecreaseVars(_) => true,
        NR::Set0(_) => true,
    }
}

fn pretty_atom<W: Write>(expr: &Rec, root: Id, out: &mut W, depth: usize) {
    let node = &expr[root];
    if needs_parenthesis(node) {
        write!(out, "(").unwrap();
        pretty(expr, root, out, depth);
        write!(out, ")").unwrap();
    } else {
        pretty(expr, root, out, depth);
    }
}

pub struct Pretty<'a> {
    expr: &'a Rec,
    root: Id,
    depth: usize,
}

impl<'a> Pretty<'a> {
    pub fn new(expr: &'a Rec, root: Id) -> Self {
        Self {
            expr,
            root,
            depth: 0,
        }
    }
}

impl Display for Pretty<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        pretty(self.expr, self.root, f, self.depth);
        Ok(())
    }
}

fn pretty<W: Write>(expr: &Rec, root: Id, out: &mut W, depth: usize) {
    use NameResolved as NR;
    let node = &expr[root];
    match node {
        NR::Var(index) => {
            let name = var_name_from_depth(var_depth(depth, *index));
            write!(out, "{}", name).unwrap();
        }
        NR::Func([param_type, right]) | NR::FuncType([param_type, right]) => {
            // ({param_name} : {param_type}) {binder} {body}
            let is_func_type = matches!(node, NR::FuncType(_));
            let binder = if is_func_type { "->" } else { "=>" };
            let param_name = var_name_from_depth(var_depth(depth + 1, 0));

            write!(out, "{}: ", param_name).unwrap();
            pretty_atom(expr, *param_type, out, depth);
            write!(out, " {binder} ").unwrap();
            pretty(expr, *right, out, depth + 1);
            // Depth + 1 because we added the parameter!
        }
        NR::App([func, arg]) => {
            // {func} {arg}  but we need parens if not atoms.
            pretty_atom(expr, *func, out, depth);
            write!(out, " ").unwrap();
            pretty_atom(expr, *arg, out, depth);
        }
        NR::Type => write!(out, "type").unwrap(),
        NR::IncreaseVars(_) => todo!(),
        NR::DecreaseVars(_) => todo!(),
        NR::Set0(_) => todo!(),
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
        let root = expr.add(NR::Func([var0, var1]));
        let pretty = Pretty::new(&expr, root).to_string();
        assert_eq!(pretty, "b: a => a");
    }

    fn parse_and_pretty(str: &'static str) -> String {
        use crate::lex::lex;
        use crate::parse::parse;
        use crate::name_resolution::NameResolved;
        let tokens = lex(str);
        let expr = parse(&tokens).unwrap();
        let root = Id::from(expr.as_ref().len() - 1);
        let mut name_resolved = RecExpr::default();
        let root = NameResolved::resolve(&expr, root, &["type"], &mut name_resolved).unwrap();
        let pretty = Pretty::new(&name_resolved, root).to_string();
        pretty
    }

    #[test]
    fn test_pretty_turns_type_to_var0() {
        let pretty = parse_and_pretty("type");
        // Var 0 is displayed as 'a'.
        assert_eq!(pretty, "a");
    }

    #[test]
    fn test_large_expression() {
        let pretty = parse_and_pretty("
            (x: (y: type -> type) => x) (a : type => type)
        ");
        let actual = "
            (b: (b: a -> a) => b) (b: a => a)
        ";
        // Var 0 is displayed as 'a'.
        assert_eq!(pretty.trim(), actual.trim());
    }
}
