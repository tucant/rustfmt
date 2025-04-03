use itertools::Itertools;
use rustc_ast::token::TokenKind;

use crate::{parse::macros::yew_html::{Html, HtmlAttributeValue}, rewrite::{Rewrite as _, RewriteContext, RewriteResult}, shape::Shape, Indent};

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

fn format_yew_html(
    context: &RewriteContext<'_>,
    shape: Shape,
    ts: TokenStream,
    span: Span,
) -> RewriteResult {
    let mut result = String::new();

    result.push_str("html_extractor::html! {");

    let parsed_elems = parse_yew_html(context, ts).unwrap();
    let mut indent = shape.indent.block_indent(context.config);
    format_yew_html_vec(context, shape, &mut indent, &mut result, &parsed_elems)?;

    result.push_str(&shape.indent.to_string_with_newline(context.config));
    result.push('}');

    Ok(result)
}