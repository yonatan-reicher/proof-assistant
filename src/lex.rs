//! This module contains the `lex` function along wiht the `Token` and `LToken` types.
//!
//! # Example
//! ```
//! use nessie_lex::lex;
//! let source = "x: int -> int";
//! let tokens = lex(source);
//! assert_eq!(tokens.len(), 5);
//! assert_eq!(tokens[0].kind, nessie_lex::Keyword::Ident);
//! assert_eq!(tokens[0].text, "x");
//! ```
// TODO: Fix this test ^

use nessie_lex::{Keyword, Symbol, NoNewLine};
pub use nessie_lex::Error;
pub use nessie_lex::range::{Pos, Range, Located, IntoLocated};

#[derive(Keyword, PartialEq, Eq, Clone, Copy, Debug)]
pub enum Keyword {
    #[string("type")]
    Type,
}

#[derive(Symbol, PartialEq, Eq, Clone, Copy, Debug)]
pub enum Symbol {
    #[string("(")]
    LParen,
    #[string(")")]
    RParen,
    #[string("->")]
    ThinArrow,
    #[string("=>")]
    FatArrow,
    #[string(":")]
    Colon,
}

pub type Token<'source> = nessie_lex::Token<'source, Keyword, Symbol, NoNewLine>;
/// A located token - `nessie_lex::Located<Token>`.
pub type LToken<'source> = nessie_lex::LToken<'source, Keyword, Symbol, NoNewLine>;

pub fn lex(source: &str) -> Vec<LToken> {
    nessie_lex::lex(source)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let code = "
            f : type -> type => f int
        ";

        let tokens = lex(code);
        dbg!(&tokens);
        assert_eq!(tokens.len(), 8);
        assert_eq!(tokens[0].0, Token::Ident("f"));
        assert_eq!(tokens[1].0, Token::Symbol(Symbol::Colon));
        assert_eq!(tokens[2].0, Token::Keyword(Keyword::Type));
        assert_eq!(tokens[3].0, Token::Symbol(Symbol::ThinArrow));
        assert_eq!(tokens[4].0, Token::Keyword(Keyword::Type));
        assert_eq!(tokens[5].0, Token::Symbol(Symbol::FatArrow));
        assert_eq!(tokens[6].0, Token::Ident("f"));
        assert_eq!(tokens[7].0, Token::Ident("int"));
    }
}
