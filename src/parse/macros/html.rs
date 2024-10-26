use rustc_ast::{Expr, StrLit};
use rustc_ast::{ast, token::Delimiter};
use rustc_ast::ptr::P;
use rustc_ast::token::{BinOpToken, Lit, TokenKind};
use rustc_ast::tokenstream::TokenStream;
use rustc_span::symbol::{self, kw, Ident};
use tracing::{debug, info};

use crate::rewrite::RewriteContext;

pub(crate) enum Html {
    Expr(P<Expr>),
    Comment(StrLit),
    Open {
        tag: Ident,
        attrs: Vec<(Ident, P<Expr>)>
    },
    Close {
        tag: Ident,
    }
}

pub(crate) fn parse_html(
    context: &RewriteContext<'_>,
    ts: TokenStream,
) -> Option<Vec<Html>> {
    let mut result = vec![];
    let mut parser = super::build_parser(context, ts);
    macro_rules! parse_or {
        ($method:ident $(,)* $($arg:expr),*) => {
            match parser.$method($($arg,)*) {
                Ok(val) => {
                    if parser.psess.dcx().has_errors().is_some() {
                        parser.psess.dcx().reset_err_count();
                        return None;
                    } else {
                        val
                    }
                }
                Err(err) => {
                    err.cancel();
                    parser.psess.dcx().reset_err_count();
                    return None;
                }
            }
        }
    }
    macro_rules! parse_eat {
        ($($arg:expr),*) => {
            if !parser.eat($($arg,)*) {
                return None;
            }
        }
    }
    while parser.token.kind != TokenKind::Eof {
        match parser.token.kind {
            TokenKind::OpenDelim(Delimiter::Brace) | TokenKind::Literal(_) | TokenKind::Ident(_, _) => {
                let expr = parse_or!(parse_expr);
                result.push(Html::Expr(expr));
            }
            TokenKind::Lt => {
                parse_eat!(&TokenKind::Lt);
                match parser.token.kind {
                    TokenKind::BinOp(BinOpToken::Slash) => {
                        parse_eat!(&TokenKind::BinOp(BinOpToken::Slash));
                        let id = parse_or!(parse_ident);
                        parse_eat!(&TokenKind::Gt);
                        result.push(Html::Close { tag: id });
                    }
                    TokenKind::Not => {
                        parse_eat!(&TokenKind::Not );
                        parse_eat!(&TokenKind::BinOp(BinOpToken::Minus));
                        parse_eat!(&TokenKind::BinOp(BinOpToken::Minus));
                        let Ok(comment) = parser.parse_str_lit() else {
                            return None;
                        };
                        parse_eat!(&TokenKind::BinOp(BinOpToken::Minus));
                        parse_eat!(&TokenKind::BinOp(BinOpToken::Minus));
                        parse_eat!(&TokenKind::Gt);
                        result.push(Html::Comment(comment));
                    }
                    _ => {
                        let id = parse_or!(parse_ident);
                        let mut attrs = Vec::new();
                        while parser.token.kind != TokenKind::Gt {
                            let id = parse_or!(parse_ident);
                            parse_eat!(&TokenKind::Eq);
                            let expr = parse_or!(parse_expr);
                            attrs.push((id, expr));
                        }
                        parse_eat!(&TokenKind::Gt);
                        result.push(Html::Open { tag: id, attrs });
                    }
                }
            }
            _ => return None
        }
    }

    Some(result)
}
