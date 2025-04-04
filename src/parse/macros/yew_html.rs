use crate::Indent;
use crate::rewrite::MacroErrorKind;
use crate::rewrite::Rewrite as _;
use crate::rewrite::RewriteContext;
use crate::rewrite::RewriteError;
use crate::rewrite::RewriteResult;
use crate::shape::Shape;
use crate::utils::mk_sp;
use itertools::Either;
use itertools::Itertools as _;
use rustc_ast::Block;
use rustc_ast::ptr::P;
use rustc_ast::token::Delimiter;
use rustc_ast::token::TokenKind;
use rustc_ast::token::TokenKind::Lt;
use rustc_ast::token::TokenKind::Slash;
use rustc_ast::tokenstream::TokenStream;
use rustc_ast::{Expr, StrLit};
use rustc_parse::exp;
use rustc_parse::parser::Parser;
use rustc_span::Span;
use rustc_span::Symbol;
use rustc_span::symbol::Ident;

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
        start_span: Span,
        tag: Option<Ident>,
        attrs: Vec<(Ident, Vec<(TokenKind, Ident)>, HtmlAttributeValue)>,
        self_closing: bool,
        end_span: Span,
    },
    Close {
        start_span: Span,
        tag: Option<Ident>,
        end_span: Span,
    },
    If {
        start_span: Span,
        before_body: Span,
        inner: HtmlIf,
        after_body: Span,
        end_span: Span,
    },
    // TODO 1. <>
    // TODO 2. if let
    // TODO 3. { for
    // TODO match
    // TODO self closing
    // TODO attribute values can contain multiple statements?
    // TODO
    // TODO <@{"div"} dynamic> tagnames </@>
    // TODO .. spread syntax
    // TODO generic type at tagname
    // TODO { for data.veranstaltungen_or_module.iter().map(|entry| {
}

fn parse_single_html(
    context: &RewriteContext<'_>,
    ts_string: &str,
    parser: &mut Parser<'_>,
) -> Result<Vec<Html>, RewriteError> {
    macro_rules! check {
        ($arg:expr) => {
            if !$arg {
                return Err(RewriteError::MacroFailure {
                    kind: MacroErrorKind::ParseFailure,
                    span: parser.token.span,
                });
            }
        };
    }
    let mut result = vec![];
    match &parser.token.kind {
        TokenKind::Ident(symbol, _) if symbol.as_str() == "if" => {
            let start_span = parser.token.span;
            check!(parser.eat_keyword(exp!(If)));
            let conditional = match parser.parse_expr_cond() {
                Ok(expr) => expr,
                Err(error) => {
                    panic!("{:?} {:?}", error, parser.parse_tokens());
                }
            };
            check!(parser.eat(exp!(OpenBrace)));
            let before_body = parser.token.span;
            let mut body = Vec::new();
            while parser.token.kind != TokenKind::CloseDelim(Delimiter::Brace) {
                body.extend(parse_single_html(context, ts_string, parser)?);
            }
            let after_body = parser.token.span;
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

            // TODO FIXME if a comment is immediately following this span is for the token after the comment
            let end_span = parser.token.span;

            result.push(Html::If {
                start_span,
                before_body,
                inner: HtmlIf {
                    conditional,
                    body,
                    else_,
                },
                after_body,
                end_span,
            })
        }
        TokenKind::OpenDelim(Delimiter::Brace) => {
            //eprintln!("parsing inner expr");
            check!(parser.eat(exp!(OpenBrace)));
            let expr = match parser.parse_expr() {
                Ok(expr) => expr,
                Err(error) => {
                    return Err(RewriteError::MacroFailure {
                        kind: MacroErrorKind::ParseFailure,
                        span: error.span.primary_span().unwrap(),
                    });
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
            let start_span = parser.token.span;
            check!(parser.eat(exp!(Lt)));
            match parser.token.kind {
                TokenKind::Slash => {
                    //eprintln!("parsing slash");
                    parser.bump();
                    //eprintln!("parsing ident");
                    if parser.token.kind == TokenKind::Gt {
                        //eprintln!("parsing gt");
                        let end_span = parser.token.span;
                        check!(parser.eat(exp!(Gt)));
                        result.push(Html::Close {
                            start_span,
                            tag: None,
                            end_span,
                        });
                    } else {
                        let id = parser.token.ident().unwrap().0;
                        parser.bump();
                        //eprintln!("parsing gt");
                        let end_span = parser.token.span;
                        check!(parser.eat(exp!(Gt)));
                        result.push(Html::Close {
                            start_span,
                            tag: Some(id),
                            end_span,
                        });
                    }
                }
                TokenKind::Gt => {
                    let end_span = parser.token.span;
                    check!(parser.eat(exp!(Gt)));
                    result.push(Html::Open {
                        start_span,
                        tag: None,
                        attrs: Vec::new(),
                        self_closing: false,
                        end_span,
                    });
                }
                _ => {
                    //eprintln!("parsing ident");
                    let Some((id, _)) = parser.token.ident() else {
                        return Err(RewriteError::MacroFailure {
                            kind: MacroErrorKind::ParseFailure,
                            span: parser.token.span,
                        });
                    };
                    parser.bump();
                    let mut attrs: Vec<(Ident, Vec<(TokenKind, Ident)>, HtmlAttributeValue)> =
                        Vec::new();
                    while parser.token.kind != TokenKind::Gt && parser.token.kind != Slash {
                        //eprintln!("parsing ident");
                        let Some((base_id, _)) = parser.token.ident() else {
                            return Err(RewriteError::MacroFailure {
                                kind: MacroErrorKind::ParseFailure,
                                span: parser.token.span,
                            });
                        };
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
                    if parser.token.kind == Slash {
                        //eprintln!("parsing gt");
                        parser.bump();
                        let end_span = parser.token.span;
                        check!(parser.eat(exp!(Gt)));
                        result.push(Html::Open {
                            start_span,
                            tag: Some(id),
                            attrs,
                            self_closing: true,
                            end_span,
                        });
                    } else {
                        //eprintln!("parsing gt");
                        let end_span = parser.token.span;
                        check!(parser.eat(exp!(Gt)));
                        result.push(Html::Open {
                            start_span,
                            tag: Some(id),
                            attrs,
                            self_closing: false,
                            end_span,
                        });
                    }
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
        Html::Open {
            start_span,
            tag,
            attrs,
            self_closing,
            end_span,
        } => {
            result.push_str(&indent.to_string_with_newline(context.config));
            result.push_str("<");
            if let Some(tag) = tag {
                result.push_str(tag.as_str());
            }
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
            if *self_closing {
                result.push_str(" /");
            }
            result.push_str(">");
            *indent = indent.block_indent(context.config);
        }
        Html::Close {
            start_span,
            tag,
            end_span,
        } => {
            *indent = indent.block_unindent(context.config);
            if let Some(tag) = tag {
                if ![
                    "area", "base", "br", "col", "command", "embed", "hr", "img", "input",
                    "keygen", "link", "meta", "param", "source", "track", "wbr",
                ]
                .contains(&tag.as_str())
                {
                    result.push_str(&indent.to_string_with_newline(context.config));
                }
            }
            if result.ends_with("\n") {
                result.push_str(&indent.to_string(context.config));
            }
            result.push_str("</");
            if let Some(tag) = tag {
                result.push_str(tag.as_str());
            }
            result.push_str(">");
        }
        Html::If {
            start_span,
            before_body,
            inner:
                HtmlIf {
                    conditional,
                    body,
                    else_,
                },
            after_body,
            end_span,
        } => {
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
            format_yew_html_vec(
                *before_body,
                *after_body,
                context,
                shape,
                indent_after,
                result,
                body,
            )
            .unwrap();
            *indent = indent.block_unindent(context.config);
            result.push_str(&indent.to_string_with_newline(context.config));
            result.push_str("} ");
            match else_ {
                None => {}
                Some(Either::Left(left)) => {
                    result.push_str(" else ");
                    format_yew_html_vec(
                        *after_body,
                        *end_span,
                        context,
                        shape,
                        &mut indent.clone(),
                        result,
                        left,
                    )
                    .unwrap();
                    *indent = indent.block_unindent(context.config);
                    result.push_str(&indent.to_string_with_newline(context.config));
                    result.push_str("}");
                }
                Some(Either::Right(right)) => {
                    result.push_str(" else {");
                    *indent = indent.block_indent(context.config);
                    format_yew_html_vec(
                        *after_body,
                        *end_span,
                        context,
                        shape,
                        &mut indent.clone(),
                        result,
                        right,
                    )
                    .unwrap();
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
    start_span: Span,
    end_span: Span,
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
            Html::Open {
                start_span: _,
                tag: _,
                attrs: _,
                self_closing: _,
                end_span: _,
            } => -1,
            Html::Close {
                start_span: _,
                tag: _,
                end_span: _,
            } => 1,
            Html::If {
                start_span,
                before_body,
                inner:
                    HtmlIf {
                        conditional: _,
                        body: _,
                        else_: _,
                    },
                after_body,
                end_span,
            } => 0,
        };
        min_indent = std::cmp::max(min_indent, indent_amount);
    }

    for _ in 0..min_indent {
        *indent = indent.block_indent(context.config);
    }

    let low_spans: Vec<_> = std::iter::once(start_span).chain(elems
        .iter()
        .map(|elem| match elem {
            Html::Expr(p) => p.span.shrink_to_hi(),
            Html::Literal(str_lit) => str_lit.span.shrink_to_hi(),
            Html::Ident(ident) => ident.span.shrink_to_hi(),
            Html::Open {
                start_span,
                tag,
                attrs,
                self_closing,
                end_span,
            } => end_span.shrink_to_hi(),
            Html::Close {
                start_span,
                tag,
                end_span,
            } => end_span.shrink_to_hi(),
            Html::If {
                start_span,
                before_body,
                after_body,
                inner,
                end_span,
            } => end_span.shrink_to_hi(),
        }))
        .collect();
    let high_spans: Vec<_> = elems
        .iter()
        .map(|elem| match elem {
            Html::Expr(p) => p.span.shrink_to_lo(),
            Html::Literal(str_lit) => str_lit.span.shrink_to_lo(),
            Html::Ident(ident) => ident.span.shrink_to_lo(),
            Html::Open {
                start_span,
                tag,
                attrs,
                self_closing,
                end_span,
            } => start_span.shrink_to_lo(),
            Html::Close {
                start_span,
                tag,
                end_span,
            } => start_span.shrink_to_lo(),
            Html::If {
                start_span,
                before_body,
                after_body,
                inner,
                end_span,
            } => start_span.shrink_to_lo(),
        })
        .chain(std::iter::once(end_span))
        .collect();
    for i in 0..elems.len() {
        let html = &elems[i];
        let span_between_elem = mk_sp(low_spans[i].hi(), high_spans[i].lo());
        //let snippet = context.snippet(span_between_elem);
        let comment = crate::comment::recover_missing_comment_in_span(
            span_between_elem,
            Shape::indented(*indent, context.config),
            context,
            0,
        )?;
        result.push_str(&comment);
        if !comment.is_empty() {
            result.push_str("\n");
        }
        format_yew_html_inner(context, shape, indent, result, html).unwrap();
    }
    let span_between_elem = mk_sp(low_spans[elems.len()].hi(), high_spans[elems.len()].lo());
    let comment = crate::comment::recover_missing_comment_in_span(
        span_between_elem,
        Shape::indented(*indent, context.config),
        context,
        0,
    )?;
    result.push_str(&comment);

    for _ in 0..min_indent {
        *indent = indent.block_unindent(context.config);
    }

    Ok(result.clone())
}

pub(crate) fn format_yew_html(
    start_span: Span,
    end_span: Span,
    context: &RewriteContext<'_>,
    shape: Shape,
    ts: TokenStream,
    span: Span,
) -> RewriteResult {
    let mut result = String::new();

    result.push_str("::yew::html! {");

    let parsed_elems = parse_html(context, ts)?;
    let mut indent = shape.indent.block_indent(context.config);
    format_yew_html_vec(
        start_span.shrink_to_hi(),
        end_span.shrink_to_lo(),
        context,
        shape,
        &mut indent,
        &mut result,
        &parsed_elems,
    )?;

    result.push_str(&shape.indent.to_string_with_newline(context.config));
    result.push('}');

    Ok(result)
}
