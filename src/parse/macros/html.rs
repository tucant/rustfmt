use crate::rewrite::RewriteContext;
use rustc_ast::Block;
use rustc_ast::ptr::P;
use rustc_ast::token::Delimiter;
use rustc_ast::token::TokenKind;
use rustc_ast::tokenstream::TokenStream;
use rustc_ast::{Expr, StrLit};
use rustc_parse::exp;
use rustc_parse::parser::Parser;
use rustc_span::symbol::Ident;

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
    If {
        conditional: P<Expr>,
        body: Vec<Html>,
        variable: Ident,
        result_expr: P<Expr>,
        else_: Option<(Vec<Html>, P<Expr>)>,
    },
    While {
        conditional: P<Expr>,
        body: Vec<Html>,
        variable: Ident,
        result_expr: P<Expr>,
    },
    Let {
        variable: Ident,
        expr: P<Expr>,
    },
    Use(P<Expr>),
    Extern(P<Block>),
}

pub(crate) fn parse_single_html(
    context: &RewriteContext<'_>,
    ts_string: &str,
    parser: &mut Parser<'_>,
) -> Option<Vec<Html>> {
    macro_rules! parse_eat {
        ($($arg:expr),*) => {
            if !parser.eat($($arg,)*) {
                panic!("{:?} {} {}", parser.token, file!(), line!());
            }
        }
    }
    let mut result = vec![];
    match &parser.token.kind {
        TokenKind::Ident(symbol, _) if symbol.as_str() == "use" => {
            assert!(parser.eat_keyword(exp!(Use)));
            let expr = match parser.parse_expr() {
                Ok(expr) => expr,
                Err(error) => {
                    panic!("{:?} {:?}", error, parser.parse_tokens());
                }
            };
            assert!(parser.eat(exp!(Semi)));
            result.push(Html::Use(expr));
        }
        TokenKind::Ident(symbol, _) if symbol.as_str() == "extern" => {
            assert!(parser.eat_keyword(exp!(Extern)));
            let block = match parser.parse_block() {
                Ok(block) => block,
                Err(error) => {
                    panic!("{:?} {:?}", error, parser.parse_tokens());
                }
            };
            result.push(Html::Extern(block));
        }
        TokenKind::Ident(symbol, _) if symbol.as_str() == "let" => {
            assert!(parser.eat_keyword(exp!(Let)));
            let variable = parser.token.ident().unwrap().0;
            parser.bump();
            assert!(parser.eat(exp!(Eq)));

            match &parser.token.kind {
                TokenKind::Ident(symbol, _) if symbol.as_str() == "if" => {
                    assert!(parser.eat_keyword(exp!(If)));
                    let conditional = match parser.parse_expr_cond() {
                        Ok(expr) => expr,
                        Err(error) => {
                            panic!("{:?} {:?}", error, parser.parse_tokens());
                        }
                    };
                    assert!(parser.eat(exp!(OpenBrace)));
                    let mut body = Vec::new();
                    while parser.token.kind != TokenKind::CloseDelim(Delimiter::Brace) {
                        if let Some(some_htmls) = parse_single_html(context, ts_string, parser) {
                            body.extend(some_htmls);
                        } else {
                            panic!();
                        }
                    }
                    assert!(parser.eat(exp!(CloseBrace)));
                    assert!(parser.eat(exp!(FatArrow)));

                    let result_expr = match parser.parse_expr() {
                        Ok(expr) => expr,
                        Err(error) => {
                            panic!("{:?} {:?}", error, parser.parse_tokens());
                        }
                    };

                    let else_ = if parser.eat_keyword(exp!(Else)) {
                        assert!(parser.eat(exp!(OpenBrace)));
                        let mut body = Vec::new();
                        while parser.token.kind != TokenKind::CloseDelim(Delimiter::Brace) {
                            if let Some(some_htmls) = parse_single_html(context, ts_string, parser)
                            {
                                body.extend(some_htmls);
                            } else {
                                panic!();
                            }
                        }
                        assert!(parser.eat(exp!(CloseBrace)));
                        assert!(parser.eat(exp!(FatArrow)));

                        let result_expr = match parser.parse_expr() {
                            Ok(expr) => expr,
                            Err(error) => {
                                panic!("{:?} {:?}", error, parser.parse_tokens());
                            }
                        };
                        Some((body, result_expr))
                    } else {
                        None
                    };
                    assert!(parser.eat(exp!(Semi)));

                    result.push(Html::If {
                        conditional,
                        body,
                        variable,
                        result_expr,
                        else_,
                    })
                }
                TokenKind::Ident(symbol, _) if symbol.as_str() == "while" => {
                    assert!(parser.eat_keyword(exp!(While)));
                    let conditional = match parser.parse_expr_cond() {
                        Ok(expr) => expr,
                        Err(error) => {
                            panic!("{:?} {:?}", error, parser.parse_tokens());
                        }
                    };
                    assert!(parser.eat(exp!(OpenBrace)));
                    let mut body = Vec::new();
                    while parser.token.kind != TokenKind::CloseDelim(Delimiter::Brace) {
                        if let Some(some_htmls) = parse_single_html(context, ts_string, parser) {
                            body.extend(some_htmls);
                        } else {
                            panic!();
                        }
                    }
                    assert!(parser.eat(exp!(CloseBrace)));
                    assert!(parser.eat(exp!(FatArrow)));
                    let result_expr = match parser.parse_expr() {
                        Ok(expr) => expr,
                        Err(error) => {
                            panic!("{:?} {:?}", error, parser.parse_tokens());
                        }
                    };
                    assert!(parser.eat(exp!(Semi)));
                    result.push(Html::While {
                        conditional,
                        body,
                        variable,
                        result_expr,
                    })
                }
                _ => {
                    let expr = match parser.parse_expr() {
                        Ok(expr) => expr,
                        Err(error) => {
                            panic!("{:?} {:?}", error, parser.parse_tokens());
                        }
                    };
                    assert!(parser.eat(exp!(Semi)));
                    result.push(Html::Let { variable, expr })
                }
            }
        }
        TokenKind::OpenDelim(Delimiter::Brace) => {
            //eprintln!("parsing inner expr");
            assert!(parser.eat(exp!(OpenBrace)));
            let expr = match parser.parse_expr() {
                Ok(expr) => expr,
                Err(error) => {
                    panic!("{:?} {:?}", error, parser.parse_tokens());
                }
            };
            assert!(parser.eat(exp!(CloseBrace)));
            result.push(Html::Expr(expr))
        }
        TokenKind::Literal(_) => {
            let Ok(literal) = parser.parse_str_lit() else {
                panic!();
            };
            result.push(Html::Literal(literal))
        }
        TokenKind::Ident(_, _) => {
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
                TokenKind::Slash => {
                    //eprintln!("parsing slash");
                    parser.bump();
                    //eprintln!("parsing ident");
                    let id = parser.token.ident().unwrap().0;
                    parser.bump();
                    //eprintln!("parsing gt");
                    parse_eat!(exp!(Gt));
                    result.push(Html::Close { tag: id });
                }
                TokenKind::Bang => {
                    //eprintln!("parsing not");
                    parse_eat!(exp!(Bang));
                    parse_eat!(exp!(Minus));
                    parse_eat!(exp!(Minus));
                    let Ok(comment) = parser.parse_str_lit() else {
                        panic!();
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
                            || parser.token.kind == TokenKind::Minus
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
                                assert!(parser.eat(exp!(OpenBrace)));
                                //eprintln!("parsing inner expr");
                                let expr = match parser.parse_expr() {
                                    Ok(expr) => expr,
                                    Err(error) => {
                                        panic!("{:?} {:?}", error, parser.parse_tokens());
                                    }
                                };
                                assert!(parser.eat(exp!(CloseBrace)));
                                attrs.push((base_id, rest_id, HtmlAttributeValue::Expr(expr)));
                            }
                            TokenKind::Literal(_) => {
                                let Ok(literal) = parser.parse_str_lit() else {
                                    panic!();
                                };
                                attrs.push((
                                    base_id,
                                    rest_id,
                                    HtmlAttributeValue::Literal(literal),
                                ));
                            }
                            TokenKind::Ident(_, _) => {
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
        other => panic!("unexpected token {:?} {}", other, ts_string),
    }
    Some(result)
}

pub(crate) fn parse_html(context: &RewriteContext<'_>, ts: TokenStream) -> Option<Vec<Html>> {
    let ts_string = format!("{:?}", ts);
    //eprintln!("parsing token stream {:?}", ts);
    let mut result = vec![];
    let mut parser = super::build_parser(context, ts);
    while parser.token.kind != TokenKind::Eof {
        if let Some(val) = parse_single_html(context, &ts_string, &mut parser) {
            result.extend(val);
        } else {
            panic!();
        }
    }

    Some(result)
}
