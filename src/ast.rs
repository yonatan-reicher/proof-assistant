//! This module holds the definitions and
//! common functions for working with the
//! AST for the proof assistant.

use symbol_table;

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum Expr {
    Var(Symbol),
}
