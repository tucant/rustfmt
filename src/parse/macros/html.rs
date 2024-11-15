use rustc_ast::ptr::P;
use rustc_ast::token::{BinOpToken, Lit, TokenKind};
use rustc_ast::tokenstream::TokenStream;
use rustc_ast::{Expr, StrLit};
use rustc_ast::{ast, token::Delimiter};
use rustc_span::symbol::{self, Ident, kw};
use tracing::{debug, info, warn};

use crate::rewrite::RewriteContext;

pub(crate) enum HtmlAttributeValue {
    Expr(P<Expr>),
    Literal(StrLit),
    Ident(Ident),
}

pub(crate) enum Html {
    Expr(P<Expr>),
    Literal(StrLit),
    Ident(Ident),
    Comment(StrLit),
    Open {
        tag: Ident,
        attrs: Vec<(Ident, HtmlAttributeValue)>,
    },
    Close {
        tag: Ident,
    },
}

pub(crate) fn parse_html(context: &RewriteContext<'_>, ts: TokenStream) -> Option<Vec<Html>> {
    eprintln!("parsing token stream {:?}", ts);
    let mut result = vec![];
    let mut parser = super::build_parser(context, ts);
    macro_rules! parse_or {
        ($method:ident $(,)* $($arg:expr),*) => {
            match parser.$method($($arg,)*) {
                Ok(val) => {
                    if parser.psess.dcx().has_errors().is_some() {
                        parser.psess.dcx().reset_err_count();
                        panic!("{:?} {} {}", parser.token, file!(), line!());
                        return None;
                    } else {
                        val
                    }
                }
                Err(err) => {
                    err.cancel();
                    parser.psess.dcx().reset_err_count();
                    panic!("{:?} {} {}", parser.token, file!(), line!());
                    return None;
                }
            }
        }
    }
    macro_rules! parse_eat {
        ($($arg:expr),*) => {
            if !parser.eat($($arg,)*) {
                panic!();
                return None;
            }
        }
    }
    while parser.token.kind != TokenKind::Eof {
        match &parser.token.kind {
            TokenKind::OpenDelim(Delimiter::Brace) => {
                eprintln!("parsing inner expr");
                let expr = match parser.parse_expr() {
                    Ok(expr) => expr,
                    Err(error) => {
                        panic!("{:?} {:?}", error, parser.parse_tokens());
                    }
                };
                result.push(Html::Expr(expr))
            }
            TokenKind::Literal(_) => {
                let Ok(literal) = parser.parse_str_lit() else {
                    return None;
                };
                result.push(Html::Literal(literal))
            }
            token_kind @ TokenKind::Ident(_, _) => {
                //eprintln!("parsing ident {:?}", parser.token);
                //let id = parse_or!(parse_ident);
                let ident = parser.token.ident().unwrap().0;
                parser.eat(&token_kind.clone());
                result.push(Html::Ident(ident))
            }
            TokenKind::Lt => {
                eprintln!("parsing lt");
                parse_eat!(&TokenKind::Lt);
                match parser.token.kind {
                    TokenKind::BinOp(BinOpToken::Slash) => {
                        eprintln!("parsing slash");
                        parse_eat!(&TokenKind::BinOp(BinOpToken::Slash));
                        eprintln!("parsing ident");
                        let id = parse_or!(parse_ident);
                        eprintln!("parsing gt");
                        parse_eat!(&TokenKind::Gt);
                        result.push(Html::Close { tag: id });
                    }
                    TokenKind::Not => {
                        eprintln!("parsing not");
                        parse_eat!(&TokenKind::Not);
                        parse_eat!(&TokenKind::BinOp(BinOpToken::Minus));
                        parse_eat!(&TokenKind::BinOp(BinOpToken::Minus));
                        let Ok(comment) = parser.parse_str_lit() else {
                            return None;
                        };
                        parse_eat!(&TokenKind::BinOp(BinOpToken::Minus));
                        println!("{:?}", parser.token);
                        parse_eat!(&TokenKind::RArrow);
                        result.push(Html::Comment(comment));
                    }
                    _ => {
                        eprintln!("parsing ident");
                        let id = parse_or!(parse_ident);
                        let mut attrs = Vec::new();
                        while parser.token.kind != TokenKind::Gt {
                            eprintln!("parsing ident");
                            let id = parse_or!(parse_ident);
                            eprintln!("parsing eq");
                            parse_eat!(&TokenKind::Eq);
                            eprintln!("parsing literal or expr");
                            info!("{:?}", parser.token.kind);
                            match &parser.token.kind {
                                TokenKind::OpenDelim(Delimiter::Brace) => {
                                    eprintln!("parsing inner expr");
                                    let expr = match parser.parse_expr() {
                                        Ok(expr) => expr,
                                        Err(error) => {
                                            panic!("{:?} {:?}", error, parser.parse_tokens());
                                        }
                                    };
                                    attrs.push((id, HtmlAttributeValue::Expr(expr)));
                                }
                                TokenKind::Literal(_) => {
                                    let Ok(literal) = parser.parse_str_lit() else {
                                        return None;
                                    };
                                    attrs.push((id, HtmlAttributeValue::Literal(literal)));
                                }
                                token_kind @ TokenKind::Ident(_, _) => {
                                    //eprintln!("parsing ident {:?}", parser.token);
                                    //let id = parse_or!(parse_ident);
                                    let ident = parser.token.ident().unwrap().0;
                                    parser.eat(&token_kind.clone());
                                    attrs.push((id, HtmlAttributeValue::Ident(ident)))
                                }
                                _ => panic!()
                            }
                        }
                        eprintln!("parsing gt");
                        parse_eat!(&TokenKind::Gt);
                        result.push(Html::Open { tag: id, attrs });
                    }
                }
            }
            _ => return None,
        }
    }

    Some(result)
}
