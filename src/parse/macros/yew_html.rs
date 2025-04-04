use crate::Indent;
use crate::rewrite::MacroErrorKind;
use crate::rewrite::Rewrite as _;
use crate::rewrite::RewriteContext;
use crate::rewrite::RewriteError;
use crate::rewrite::RewriteResult;
use crate::shape::Shape;
use itertools::Either;
use itertools::Itertools as _;
use rustc_ast::Block;
use rustc_ast::ptr::P;
use rustc_ast::token::Delimiter;
use rustc_ast::token::TokenKind;
use rustc_ast::tokenstream::TokenStream;
use rustc_ast::{Expr, StrLit};
use rustc_parse::exp;
use rustc_parse::parser::Parser;
use rustc_span::Span;
use rustc_span::symbol::Ident;
use rustc_span::Symbol;

enum HtmlAttributeValue {
    Expr(P<Expr>),
    Literal(StrLit),
    Ident(Ident),
}

struct HtmlIf {
    conditional: P<Expr>,
    body: Vec<Html>,
    else_: Option<Either<Vec<Html>, Vec<Html>>>,
}

enum Html {
    Expr(P<Expr>),
    Literal(StrLit),
    Ident(Ident),
    Open {
        tag: Ident,
        attrs: Vec<(Ident, Vec<(TokenKind, Ident)>, HtmlAttributeValue)>,
    },
    Close {
        tag: Ident,
    },
    If(HtmlIf)
}

fn parse_single_html(
    context: &RewriteContext<'_>,
    ts_string: &str,
    parser: &mut Parser<'_>,
) -> Result<Vec<Html>, RewriteError> {
    macro_rules! check {
        ($arg:expr) => {
            if !$arg {
                return Err(RewriteError::MacroFailure { kind: MacroErrorKind::ParseFailure, span: parser.token.span })
            }
        }
    }
    let mut result = vec![];
    match &parser.token.kind {
        TokenKind::Ident(symbol, _) if symbol.as_str() == "if" => {
            check!(parser.eat_keyword(exp!(If)));
            let conditional = match parser.parse_expr_cond() {
                Ok(expr) => expr,
                Err(error) => {
                    panic!("{:?} {:?}", error, parser.parse_tokens());
                }
            };
            check!(parser.eat(exp!(OpenBrace)));
            let mut body = Vec::new();
            while parser.token.kind != TokenKind::CloseDelim(Delimiter::Brace) {
                body.extend(parse_single_html(context, ts_string, parser)?);
            }
            check!(parser.eat(exp!(CloseBrace)));

            let else_ = if parser.eat_keyword(exp!(Else)) {
                if parser.token.is_ident_named(Symbol::intern("if")) {
                    Some(Either::Left(parse_single_html(context, ts_string, parser)?))
                } else {
                    check!(parser.eat(exp!(OpenBrace)));
                    let mut body = Vec::new();
                    while parser.token.kind != TokenKind::CloseDelim(Delimiter::Brace) {
                        body.extend(parse_single_html(context, ts_string, parser)?);
                    }
                    check!(parser.eat(exp!(CloseBrace)));
                    Some(Either::Right(body))
                }
            } else {
                None
            };
            check!(parser.eat(exp!(Semi)));

            result.push(Html::If(HtmlIf {
                conditional,
                body,
                else_,
            }))
        }
        TokenKind::OpenDelim(Delimiter::Brace) => {
            //eprintln!("parsing inner expr");
            check!(parser.eat(exp!(OpenBrace)));
            let expr = match parser.parse_expr() {
                Ok(expr) => expr,
                Err(error) => {
                    panic!("{:?} {:?}", error, parser.parse_tokens());
                }
            };
            check!(parser.eat(exp!(CloseBrace)));
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
            check!(parser.eat(exp!(Lt)));
            match parser.token.kind {
                TokenKind::Slash => {
                    //eprintln!("parsing slash");
                    parser.bump();
                    //eprintln!("parsing ident");
                    let id = parser.token.ident().unwrap().0;
                    parser.bump();
                    //eprintln!("parsing gt");
                    check!(parser.eat(exp!(Gt)));
                    result.push(Html::Close { tag: id });
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
                        check!(parser.eat(exp!(Eq))); // here
                        //eprintln!("parsing literal or expr");
                        match &parser.token.kind {
                            TokenKind::OpenDelim(Delimiter::Brace) => {
                                check!(parser.eat(exp!(OpenBrace)));
                                //eprintln!("parsing inner expr");
                                let expr = match parser.parse_expr() {
                                    Ok(expr) => expr,
                                    Err(error) => {
                                        panic!("{:?} {:?}", error, parser.parse_tokens());
                                    }
                                };
                                check!(parser.eat(exp!(CloseBrace)));
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
                    check!(parser.eat(exp!(Gt)));
                    result.push(Html::Open { tag: id, attrs });
                }
            }
        }
        other => {
            return Err(RewriteError::MacroFailure {
                kind: MacroErrorKind::ParseFailure,
                span: parser.token.span,
            });
        }
    }
    Ok(result)
}

fn parse_html(context: &RewriteContext<'_>, ts: TokenStream) -> Result<Vec<Html>, RewriteError> {
    let ts_string = format!("{:?}", ts);
    //eprintln!("parsing token stream {:?}", ts);
    let mut result = vec![];
    let mut parser = super::build_parser(context, ts);
    while parser.token.kind != TokenKind::Eof {
        result.extend(parse_single_html(context, &ts_string, &mut parser)?);
    }

    Ok(result)
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
        Html::If(HtmlIf {
            conditional,
            body,
            else_,
        }) => {
            result.push_str(&indent.to_string_with_newline(context.config));
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
            match else_ {
                None => {}
                Some(Either::Left(left)) => {
                    result.push_str(" else ");
                    format_yew_html_vec(context, shape, &mut indent.clone(), result, left).unwrap();
                    *indent = indent.block_unindent(context.config);
                    result.push_str(&indent.to_string_with_newline(context.config));
                    result.push_str("}");
                },
                Some(Either::Right(right)) => {
                    result.push_str(" else {");
                    *indent = indent.block_indent(context.config);
                    format_yew_html_vec(context, shape, &mut indent.clone(), result, right).unwrap();
                    *indent = indent.block_unindent(context.config);
                    result.push_str(&indent.to_string_with_newline(context.config));
                    result.push_str("}");
                }
            }
            *indent = *indent_after;
            *indent = indent.block_unindent(context.config);
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
            Html::Open { tag: _, attrs: _ } => -1,
            Html::Close { tag: _ } => 1,
            Html::If(HtmlIf {
                conditional: _,
                body: _,
                else_: _,
            }) => 0,
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

    result.push_str("::yew::html! {");

    let parsed_elems = parse_html(context, ts)?;
    let mut indent = shape.indent.block_indent(context.config);
    format_yew_html_vec(context, shape, &mut indent, &mut result, &parsed_elems)?;

    result.push_str(&shape.indent.to_string_with_newline(context.config));
    result.push('}');

    Ok(result)
}
