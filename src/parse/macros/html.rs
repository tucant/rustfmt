use crate::rewrite::RewriteContext;
use rustc_ast::ptr::P;
use rustc_ast::token;
use rustc_ast::token::{BinOpToken, Lit, TokenKind};
use rustc_ast::tokenstream::TokenStream;
use rustc_ast::{Expr, StrLit};
use rustc_ast::{ast, token::Delimiter};
use rustc_parse::exp;
use rustc_parse::parser::ExpTokenPair;
use rustc_parse::parser::TokenType;
use rustc_span::symbol::{self, Ident, kw};
use tracing::{debug, info, warn};
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
        attrs: Vec<(Ident, Vec<(TokenKind, Ident)>, HtmlAttributeValue)>,
    },
    Close {
        tag: Ident,
    },
}

pub(crate) fn parse_html(context: &RewriteContext<'_>, ts: TokenStream) -> Option<Vec<Html>> {
    let ts_string = format!("{:?}", ts);
    //eprintln!("parsing token stream {:?}", ts);
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
                panic!("{:?} {} {}", parser.token, file!(), line!());
                return None;
            }
        }
    }
    while parser.token.kind != TokenKind::Eof {
        match &parser.token.kind {
            TokenKind::OpenDelim(Delimiter::Brace) => {
                //eprintln!("parsing inner expr");
                parser.eat(exp!(OpenBrace));
                let expr = match parser.parse_expr() {
                    Ok(expr) => expr,
                    Err(error) => {
                        panic!("{:?} {:?}", error, parser.parse_tokens());
                    }
                };
                parser.eat(exp!(CloseBrace));
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
                parser.bump();
                result.push(Html::Ident(ident))
            }
            TokenKind::Lt => {
                //eprintln!("parsing lt");
                parse_eat!(exp!(Lt));
                match parser.token.kind {
                    TokenKind::BinOp(BinOpToken::Slash) => {
                        //eprintln!("parsing slash");
                        parser.bump();
                        //eprintln!("parsing ident");
                        let id = parser.token.ident().unwrap().0;
                        parser.bump();
                        //eprintln!("parsing gt");
                        parse_eat!(exp!(Gt));
                        result.push(Html::Close { tag: id });
                    }
                    TokenKind::Not => {
                        //eprintln!("parsing not");
                        parse_eat!(exp!(Not));
                        parse_eat!(exp!(Minus));
                        parse_eat!(exp!(Minus));
                        let Ok(comment) = parser.parse_str_lit() else {
                            return None;
                        };
                        parse_eat!(exp!(Minus));
                        parse_eat!(exp!(RArrow));
                        result.push(Html::Comment(comment));
                    }
                    _ => {
                        //eprintln!("parsing ident");
                        let id = parser.token.ident().expect(&ts_string).0;
                        parser.bump();
                        let mut attrs: Vec<(Ident, Vec<(TokenKind, Ident)>, HtmlAttributeValue)> =
                            Vec::new();
                        while parser.token.kind != TokenKind::Gt {
                            //eprintln!("parsing ident");
                            let base_id = parser.token.ident().expect(&ts_string).0;
                            parser.bump();
                            let mut rest_id = Vec::new();
                            // also minus?
                            while parser.token.kind == TokenKind::Colon
                                || parser.token.kind == TokenKind::BinOp(BinOpToken::Minus)
                            {
                                let delimiter = parser.token.kind.clone();
                                parser.bump();
                                let i = parser.token.ident().unwrap().0;
                                parser.bump();
                                rest_id.push((delimiter, i));
                            }
                            //eprintln!("parsing eq");
                            parse_eat!(exp!(Eq)); // here
                            //eprintln!("parsing literal or expr");
                            match &parser.token.kind {
                                TokenKind::OpenDelim(Delimiter::Brace) => {
                                    parser.eat(exp!(OpenBrace));
                                    //eprintln!("parsing inner expr");
                                    let expr = match parser.parse_expr() {
                                        Ok(expr) => expr,
                                        Err(error) => {
                                            panic!("{:?} {:?}", error, parser.parse_tokens());
                                        }
                                    };
                                    parser.eat(exp!(CloseBrace));
                                    attrs.push((base_id, rest_id, HtmlAttributeValue::Expr(expr)));
                                }
                                TokenKind::Literal(_) => {
                                    let Ok(literal) = parser.parse_str_lit() else {
                                        return None;
                                    };
                                    attrs.push((
                                        base_id,
                                        rest_id,
                                        HtmlAttributeValue::Literal(literal),
                                    ));
                                }
                                token_kind @ TokenKind::Ident(_, _) => {
                                    //eprintln!("parsing ident {:?}", parser.token);
                                    //let id = parse_or!(parse_ident);
                                    let ident = parser.token.ident().unwrap().0;
                                    parser.bump();
                                    attrs.push((base_id, rest_id, HtmlAttributeValue::Ident(ident)))
                                }
                                _ => panic!(),
                            }
                        }
                        //eprintln!("parsing gt");
                        parse_eat!(exp!(Gt));
                        result.push(Html::Open { tag: id, attrs });
                    }
                }
            }
            _ => return None,
        }
    }

    Some(result)
}
