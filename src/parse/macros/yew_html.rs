use crate::rewrite::Rewrite as _;
use crate::rewrite::RewriteContext;
use crate::rewrite::RewriteResult;
use crate::shape::Shape;
use crate::Indent;
use itertools::Itertools as _;
use rustc_ast::Block;
use rustc_ast::ptr::P;
use rustc_ast::token::Delimiter;
use rustc_ast::token::TokenKind;
use rustc_ast::tokenstream::TokenStream;
use rustc_ast::{Expr, StrLit};
use rustc_parse::exp;
use rustc_parse::parser::Parser;
use rustc_span::symbol::Ident;
use rustc_span::Span;

enum HtmlAttributeValue {
    Expr(P<Expr>),
    Literal(StrLit),
    Ident(Ident),
}

enum Html {
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

fn parse_single_html(
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

fn parse_html(context: &RewriteContext<'_>, ts: TokenStream) -> Option<Vec<Html>> {
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

fn format_yew_html_inner(
    context: &RewriteContext<'_>,
    shape: Shape,
    indent: &mut Indent,
    result: &mut String,
    html: &Html,
) -> RewriteResult {
    let nested_shape = shape
        .block_indent(context.config.tab_spaces())
        .with_max_width(context.config);

    match html {
        Html::Literal(literal) => {
            result.push_str(&indent.to_string_with_newline(context.config));
            result.push_str("\"");
            result.push_str(literal.symbol.as_str().trim_ascii());
            result.push_str("\"");
        }
        Html::Ident(ident) => {
            if ident.as_str() != "_" {
                result.push_str(&indent.to_string_with_newline(context.config));
                result.push_str(ident.as_str());
            }
        }
        Html::Expr(p) => {
            result.push_str(&indent.to_string_with_newline(context.config));
            result.push_str("{");
            result.push_str(
                &p.rewrite_result(
                    context,
                    nested_shape
                        .sub_width(1, p.span)
                        .unwrap_or_else(|_| panic!("Something went horribly wrong!")),
                )
                .unwrap(),
            );
            result.push_str("}");
        }
        Html::Comment(str_lit) => {}
        Html::Open { tag, attrs } => {
            result.push_str(&indent.to_string_with_newline(context.config));
            result.push_str("<");
            result.push_str(tag.as_str());
            for (base_ident, rest_ident, value) in attrs {
                result.push_str(" ");
                result.push_str(base_ident.as_str());
                result.push_str(
                    &rest_ident
                        .iter()
                        .map(|(delimiter, ident)| {
                            match delimiter {
                                TokenKind::Colon => ":",
                                TokenKind::Minus => "-",
                                _ => panic!(),
                            }
                            .to_owned()
                                + ident.as_str()
                        })
                        .join(""),
                );
                result.push_str("=");
                match &value {
                    HtmlAttributeValue::Expr(p) => {
                        result.push_str("{");
                        result.push_str(
                            &p.rewrite_result(
                                context,
                                Shape::indented(*indent, context.config)
                                    .sub_width(1, p.span)
                                    .unwrap_or_else(|_| panic!("Something went horribly wrong!")),
                            )
                            .unwrap(),
                        );
                        result.push_str("}");
                    }
                    HtmlAttributeValue::Literal(str_lit) => {
                        result.push_str("\"");
                        result.push_str(str_lit.symbol.as_str());
                        result.push_str("\"");
                    }
                    HtmlAttributeValue::Ident(ident) => result.push_str(ident.as_str()),
                }
            }
            result.push_str(">");
            *indent = indent.block_indent(context.config);
        }
        Html::Close { tag } => {
            *indent = indent.block_unindent(context.config);
            if ![
                "area", "base", "br", "col", "command", "embed", "hr", "img", "input", "keygen",
                "link", "meta", "param", "source", "track", "wbr",
            ]
            .contains(&tag.as_str())
            {
                result.push_str(&indent.to_string_with_newline(context.config));
            }
            result.push_str("</");
            result.push_str(tag.as_str());
            result.push_str(">");
        }
        Html::If {
            conditional,
            body,
            variable,
            result_expr,
            else_,
        } => {
            result.push_str(&indent.to_string_with_newline(context.config));
            result.push_str("let ");
            result.push_str(variable.as_str());
            result.push_str(" = ");
            result.push_str("if ");
            result.push_str(
                &conditional
                    .rewrite_result(
                        context,
                        Shape::indented(*indent, context.config)
                            .sub_width(1, conditional.span)
                            .unwrap_or_else(|_| panic!("Something went horribly wrong!")),
                    )
                    .unwrap(),
            );
            result.push_str(" {");
            *indent = indent.block_indent(context.config);
            let indent_after = &mut indent.clone();
            format_yew_html_vec(context, shape, indent_after, result, body).unwrap();
            *indent = indent.block_unindent(context.config);
            result.push_str(&indent.to_string_with_newline(context.config));
            result.push_str("} ");
            result.push_str("=> ");
            result.push_str(
                &result_expr
                    .rewrite_result(
                        context,
                        Shape::indented(*indent, context.config)
                            .sub_width(1, result_expr.span)
                            .unwrap_or_else(|_| panic!("Something went horribly wrong!")),
                    )
                    .unwrap(),
            );
            if let Some((body, result_expr)) = else_ {
                result.push_str(" else {");
                *indent = indent.block_indent(context.config);
                format_yew_html_vec(context, shape, &mut indent.clone(), result, body).unwrap();
                *indent = indent.block_unindent(context.config);
                result.push_str(&indent.to_string_with_newline(context.config));
                result.push_str("} => ");
                result.push_str(
                    &result_expr
                        .rewrite_result(
                            context,
                            Shape::indented(*indent, context.config)
                                .sub_width(1, result_expr.span)
                                .unwrap_or_else(|_| panic!("Something went horribly wrong!")),
                        )
                        .unwrap(),
                );
            }
            result.push_str(";");
            *indent = *indent_after;
            *indent = indent.block_unindent(context.config);
        }
        Html::While {
            conditional,
            body,
            variable,
            result_expr,
        } => {
            result.push_str(&indent.to_string_with_newline(context.config));
            result.push_str("let ");
            result.push_str(variable.as_str());
            result.push_str(" = ");
            result.push_str("while ");
            result.push_str(
                &conditional
                    .rewrite_result(
                        context,
                        Shape::indented(*indent, context.config)
                            .sub_width(1, conditional.span)
                            .unwrap_or_else(|_| panic!("Something went horribly wrong!")),
                    )
                    .unwrap(),
            );
            result.push_str(" {");
            *indent = indent.block_indent(context.config);
            format_yew_html_vec(context, shape, &mut indent.clone(), result, body).unwrap();
            *indent = indent.block_unindent(context.config);
            result.push_str(&indent.to_string_with_newline(context.config));
            result.push_str("} => ");
            result.push_str(
                &result_expr
                    .rewrite_result(
                        context,
                        Shape::indented(*indent, context.config)
                            .sub_width(1, result_expr.span)
                            .unwrap_or_else(|_| panic!("Something went horribly wrong!")),
                    )
                    .unwrap(),
            );
            result.push_str(";");
        }
        Html::Let { variable, expr } => {
            result.push_str(&indent.to_string_with_newline(context.config));
            result.push_str("let ");
            result.push_str(variable.as_str());
            result.push_str(" = ");
            result.push_str(
                &expr
                    .rewrite_result(
                        context,
                        Shape::indented(*indent, context.config)
                            .sub_width(1, expr.span)
                            .unwrap_or_else(|_| panic!("Something went horribly wrong!")),
                    )
                    .unwrap(),
            );
            result.push_str(";");
        }
        Html::Use(expr) => {
            result.push_str(&indent.to_string_with_newline(context.config));
            result.push_str("use ");
            result.push_str(
                &expr
                    .rewrite_result(
                        context,
                        Shape::indented(*indent, context.config)
                            .sub_width(1, expr.span)
                            .unwrap_or_else(|_| panic!("Something went horribly wrong!")),
                    )
                    .unwrap(),
            );
            result.push_str(";");
        }
        Html::Extern(block) => {
            result.push_str(&indent.to_string_with_newline(context.config));
            result.push_str("extern ");
            result.push_str(
                &block
                    .rewrite_result(
                        context,
                        Shape::indented(*indent, context.config)
                            .sub_width(1, block.span)
                            .unwrap_or_else(|_| panic!("Something went horribly wrong!")),
                    )
                    .unwrap(),
            );
        }
    }
    Ok(result.to_string())
}

fn format_yew_html_vec(
    context: &RewriteContext<'_>,
    shape: Shape,
    indent: &mut Indent,
    mut result: &mut String,
    elems: &Vec<Html>,
) -> RewriteResult {
    let mut min_indent = 0;
    let mut indent_amount = 0;

    for elem in elems.iter() {
        indent_amount += match elem {
            Html::Expr(_) => 0,
            Html::Literal(_) => 0,
            Html::Ident(_) => 0,
            Html::Comment(_) => 0,
            Html::Open { tag: _, attrs: _ } => -1,
            Html::Close { tag: _ } => 1,
            Html::If {
                conditional: _,
                body: _,
                variable: _,
                result_expr: _,
                else_: _,
            } => 0,
            Html::While {
                conditional: _,
                body: _,
                variable: _,
                result_expr: _,
            } => 0,
            Html::Let { variable, expr } => 0,
            Html::Use(expr) => 0,
            Html::Extern(block) => 0,
        };
        min_indent = std::cmp::max(min_indent, indent_amount);
    }

    for _ in 0..min_indent {
        *indent = indent.block_indent(context.config);
    }

    for html in elems {
        format_yew_html_inner(context, shape, indent, result, html).unwrap();
    }

    for _ in 0..min_indent {
        *indent = indent.block_unindent(context.config);
    }

    Ok(result.clone())
}

pub(crate) fn format_yew_html(
    context: &RewriteContext<'_>,
    shape: Shape,
    ts: TokenStream,
    span: Span,
) -> RewriteResult {
    let mut result = String::new();

    result.push_str("html_extractor::html! {");

    let parsed_elems = parse_html(context, ts).unwrap();
    let mut indent = shape.indent.block_indent(context.config);
    format_yew_html_vec(context, shape, &mut indent, &mut result, &parsed_elems)?;

    result.push_str(&shape.indent.to_string_with_newline(context.config));
    result.push('}');

    Ok(result)
}