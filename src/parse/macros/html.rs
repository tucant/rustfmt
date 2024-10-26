use rustc_ast::{ast, token::Delimiter};
use rustc_ast::ptr::P;
use rustc_ast::token::{BinOpToken, Lit, TokenKind};
use rustc_ast::tokenstream::TokenStream;
use rustc_span::symbol::{self, kw};
use tracing::{debug, info};

use crate::rewrite::RewriteContext;

pub(crate) fn parse_html(
    context: &RewriteContext<'_>,
    ts: TokenStream,
) -> Option<Vec<(ast::Visibility, symbol::Ident, P<ast::Ty>, P<ast::Expr>)>> {
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
            TokenKind::OpenDelim(Delimiter::Brace) => {
                let expr = parse_or!(parse_expr);
            }    
            TokenKind::Literal(_) | TokenKind::Ident(_, _) => {
                let expr = parse_or!(parse_expr);
            }
            TokenKind::Lt => {
                parse_eat!(&TokenKind::Lt);
                match parser.token.kind {
                    TokenKind::BinOp(BinOpToken::Slash) => {
                        parse_eat!(&TokenKind::BinOp(BinOpToken::Slash));
                        let id = parse_or!(parse_ident);
                        // attrs

                        parse_eat!(&TokenKind::Gt);

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
                    }
                    _ => {
                        let id = parse_or!(parse_ident);
                        // attrs

                        parse_eat!(&TokenKind::Gt);

                    }
                }
            }
            _ => return None
        }
        /*// Parse a `lazy_static!` item.
        // FIXME: These `eat_*` calls should be converted to `parse_or` to avoid
        // silently formatting malformed lazy-statics.
        let vis = parse_or!(parse_visibility, rustc_parse::parser::FollowedByType::No);
        let _ = parser.eat_keyword(kw::Static);
        let _ = parser.eat_keyword(kw::Ref);
        let id = parse_or!(parse_ident);
        let _ = parser.eat(&TokenKind::Colon);
        let ty = parse_or!(parse_ty);
        let _ = parser.eat(&TokenKind::Eq);
        let expr = parse_or!(parse_expr);
        let _ = parser.eat(&TokenKind::Semi);
        result.push((vis, id, ty, expr));*/
    }

    Some(result)
}
