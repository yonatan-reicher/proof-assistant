use crate::ast::Expr;
use crate::lex::{Error as LexError, Keyword, LToken, Located, Pos, Symbol, Token};
use egg::{Id, RecExpr};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    UnexpectedEof(Pos),
    UnmatchedParen { open: Pos, expected_close: Pos },
    CouldNotParseToken(LexError),
    MustHaveExpressionAfterParen(Pos),
}

pub type ParseResult<T> = Result<T, Error>;

pub fn parse(tokens: &[LToken]) -> ParseResult<RecExpr<Expr>> {
    let mut dest = RecExpr::default();
    let mut tokens = Tokens::new(tokens);
    match parse_expr(&mut tokens, &mut dest)? {
        Some(_) => Ok(dest),
        None => Err(Error::UnexpectedEof(0)),
    }
}

struct Tokens<'source, 'a> {
    tokens: &'a [LToken<'source>],
    pos: Pos,
}

#[allow(dead_code)]
impl<'source, 'a> Tokens<'source, 'a> {
    pub fn new(tokens: &'a [LToken<'source>]) -> Self {
        Self { tokens, pos: 0 }
    }

    pub fn peek(&self) -> Option<LToken<'source>> {
        self.tokens.get(self.pos).cloned()
    }

    pub fn pop(&mut self) -> Option<LToken<'source>> {
        let token = self.peek();
        if token.is_some() {
            self.pos += 1;
        }
        token
    }

    pub fn back(&mut self) -> bool {
        if self.pos == 0 {
            false
        } else {
            self.pos -= 1;
            true
        }
    }

    pub fn pop_if(&mut self, predicate: impl FnOnce(&LToken) -> bool) -> Option<LToken> {
        let token = self.peek();
        if token.as_ref().map_or(false, predicate) {
            self.pos += 1;
            token
        } else {
            None
        }
    }

    pub fn pop_map_if<T>(&mut self, f: impl FnOnce(LToken<'source>) -> Option<T>) -> Option<T> {
        let ret = self.peek().and_then(f);
        if ret.is_some() {
            self.pos += 1;
        }
        ret
    }

    pub fn pop_eq(&mut self, kind: impl Into<Token<'source>>) -> bool {
        self.pop_if(|token| **token == kind.into()).is_some()
    }

    pub fn pop_symbol(&mut self) -> Option<Located<Symbol>> {
        self.pop_map_if(|Located(token, range)| {
            if let Token::Symbol(symbol) = token {
                Some(Located(symbol, range))
            } else {
                None
            }
        })
    }

    pub fn pop_keyword(&mut self) -> Option<Located<Keyword>> {
        self.pop_map_if(|Located(token, range)| {
            if let Token::Keyword(keyword) = token {
                Some(Located(keyword, range))
            } else {
                None
            }
        })
    }

    pub fn pop_ident(&mut self) -> Option<Located<&'source str>> {
        self.pop_map_if(|Located(token, range)| {
            if let Token::Ident(ident) = token {
                Some(Located(ident, range))
            } else {
                None
            }
        })
    }

    pub fn pop_int(&mut self) -> Option<Located<i32>> {
        self.pop_map_if(|Located(token, range)| {
            if let Token::Int(int) = token {
                Some(Located(int, range))
            } else {
                None
            }
        })
    }

    pub fn pop_string(&mut self) -> Option<Located<String>> {
        self.pop_map_if(|Located(token, range)| {
            if let Token::String(_, string) = token {
                Some(Located(string, range))
            } else {
                None
            }
        })
    }

    pub fn pos(&self) -> Pos {
        self.pos
    }

    pub fn len(&self) -> usize {
        self.tokens.len()
    }

    pub fn is_empty(&self) -> bool {
        self.pos == self.len()
    }

    pub fn slice(&self) -> &[LToken] {
        &self.tokens[self.pos..]
    }
}

fn parse_expr(tokens: &mut Tokens, dest: &mut RecExpr<Expr>) -> ParseResult<Option<Id>> {
    let Some(first) = parse_atom(tokens, dest)? else {
        return Ok(None);
    };

    // Exampl: A -> B
    if tokens.pop_eq(Symbol::ThinArrow) {
        let ty = first;
        let Some(rhs) = parse_expr(tokens, dest)? else {
            return Err(todo!());
        };
        let no_name: egg::Symbol = "$".into();
        return Ok(Some(dest.add(Expr::FuncType(no_name, [ty, rhs]))));
    }

    // Examples: x: A -> B, x: A => B
    if tokens.pop_eq(Symbol::Colon) {
        // TODO: Should this next line be `parse_expr` or `parse_atom`?
        let Some(ty) = parse_application(tokens, dest)? else {
            return Err(todo!());
        };
        let arrow = expect_arrow(tokens)?;
        let Some(rhs) = parse_expr(tokens, dest)? else {
            return Err(todo!());
        };
        let first = read_atom_as_parameter(dest, first)?;
        return Ok(Some(dest.add(arrow(first, [ty, rhs]))));
    }

    parse_application_cont(tokens, first, dest)
}

fn parse_application_cont(tokens: &mut Tokens, first: Id, dest: &mut RecExpr<Expr>) -> ParseResult<Option<Id>> {
    // Example: f x y
    let mut ret = first;
    while let Some(next_term) = parse_atom(tokens, dest)? {
        ret = dest.add(Expr::App([ret, next_term]));
    }
    Ok(Some(ret))
}

fn parse_application(tokens: &mut Tokens, dest: &mut RecExpr<Expr>) -> ParseResult<Option<Id>> {
    let Some(first) = parse_atom(tokens, dest)? else {
        return Ok(None);
    };

    parse_application_cont(tokens, first, dest)
}


type Arrow = fn(egg::Symbol, [Id; 2]) -> Expr;

fn expect_arrow(tokens: &mut Tokens) -> ParseResult<Arrow> {
    if tokens.pop_eq(Symbol::ThinArrow) {
        return Ok(Expr::FuncType);
    }
    if tokens.pop_eq(Symbol::FatArrow) {
        return Ok(Expr::Func);
    }
    return todo!("Give some error!");
}

fn read_atom_as_parameter(expr: &RecExpr<Expr>, root: Id) -> ParseResult<egg::Symbol> {
    match expr[root] {
        Expr::Var(symbol) => Ok(symbol),
        Expr::Func(_, _) => todo!(),
        Expr::FuncType(_, _) => todo!(),
        Expr::App(_) => todo!(),
    }
}

fn parse_atom(tokens: &mut Tokens, dest: &mut RecExpr<Expr>) -> ParseResult<Option<Id>> {
    if let Some(ident) = tokens.pop_ident() {
        return Ok(Some(dest.add(Expr::Var((*ident).into()))));
    }

    // Because `type` is a keyword, we can't just refer to "type" with a
    // variable expression! Because a user might declare a variable called
    // "type" and then we would return the wrong thing!
    // TODO: Either make `type` a true keyword or have a type literal. The latter
    // is preferred, I think.
    if tokens.pop_eq(Keyword::Type) {
        return Ok(Some(dest.add(Expr::Var("type".into()))));
    }

    if tokens.pop_eq(Symbol::LParen) {
        let Some(inner) = parse_expr(tokens, dest)? else {
            return Err(Error::MustHaveExpressionAfterParen(tokens.pos()));
        };
        expect_rparen(tokens)?;
        return Ok(Some(inner))
    }

    if let Some(int) = tokens.pop_int() {
        todo!();
    }

    if let Some(string) = tokens.pop_string() {
        todo!();
    }

    Ok(None)
}

fn expect_rparen(tokens: &mut Tokens) -> ParseResult<()> {
    if !tokens.pop_eq(Symbol::RParen) {
        if let Some(after) = tokens.peek() {
            return Err(Error::UnmatchedParen {
                open: after.1 .0,
                expected_close: after.1 .0,
            });
        } else {
            todo!("Return some other error")
        }
    }
    Ok(())
}
