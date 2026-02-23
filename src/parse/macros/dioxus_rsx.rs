use crate::expr::rewrite_literal;
use crate::rewrite::MacroErrorKind;
use crate::rewrite::Rewrite;
use crate::rewrite::RewriteContext;
use crate::rewrite::RewriteError;
use crate::rewrite::RewriteResult;
use crate::shape::Shape;
use rustc_ast::Block;
use rustc_ast::Pat;
use rustc_ast::Ty;
use rustc_ast::token::IdentIsRaw;
use rustc_ast::token::TokenKind;
use rustc_ast::tokenstream::TokenStream;
use rustc_ast::{Expr, StrLit};
use rustc_parse::exp;
use rustc_parse::parser::LetChainsPolicy;
use rustc_parse::parser::Parser;
use rustc_span::symbol::Ident;

pub(crate) enum HtmlAttrName {
    Ident((Ident, IdentIsRaw)),
    Literal(StrLit),
}

pub(crate) enum Html {
    Expr(Box<Block>),
    Literal(StrLit),
    Element {
        ty: Box<Ty>,
        attrs: Vec<(HtmlAttrName, Option<Box<Expr>>)>,
        body: Vec<Html>,
    },
    If {
        conditional: Box<Expr>,
        body: Vec<Html>,
        else_: Option<Vec<Html>>,
    },
    For {
        pattern: Box<Pat>,
        expr: Box<Expr>,
        body: Vec<Html>,
    },
}

pub(crate) fn parse_single(
    context: &RewriteContext<'_>,
    ts_string: &str,
    parser: &mut Parser<'_>,
) -> Result<Vec<Html>, RewriteError> {
    macro_rules! parse_eat {
        ($($arg:expr),*) => {
            if !parser.eat($($arg,)*) {
                panic!("{:?} {} {} {}", parser.token, file!(), line!(), ts_string);
            }
        }
    }
    let mut result = vec![];
    match &parser.token.kind {
        TokenKind::Ident(symbol, _) if symbol.as_str() == "if" => {
            assert!(parser.eat_keyword(exp!(If)));
            let conditional = match parser.parse_expr_cond(LetChainsPolicy::AlwaysAllowed) {
                Ok(expr) => expr,
                Err(error) => {
                    panic!("{:?} {:?}", error, parser.parse_tokens());
                }
            };
            assert!(parser.eat(exp!(OpenBrace)));
            let mut body = Vec::new();
            while parser.token.kind != TokenKind::CloseBrace {
                let some_htmls = parse_single(context, ts_string, parser)?;
                body.extend(some_htmls);
            }
            assert!(parser.eat(exp!(CloseBrace)));

            let else_ = if parser.eat_keyword(exp!(Else)) {
                assert!(parser.eat(exp!(OpenBrace)));
                let mut body = Vec::new();
                while parser.token.kind != TokenKind::CloseBrace {
                    let some_htmls = parse_single(context, ts_string, parser)?;
                    body.extend(some_htmls);
                }
                assert!(parser.eat(exp!(CloseBrace)));

                Some(body)
            } else {
                None
            };

            result.push(Html::If {
                conditional,
                body,
                else_,
            })
        }
        TokenKind::Ident(symbol, _) if symbol.as_str() == "for" => {
            assert!(parser.eat_keyword(exp!(For)));
            let (pattern, expr) = match parser.parse_for_head() {
                Ok(expr) => expr,
                Err(error) => {
                    panic!("{:?} {:?}", error, parser.parse_tokens());
                }
            };
            assert!(parser.eat(exp!(OpenBrace)));
            let mut body = Vec::new();
            while parser.token.kind != TokenKind::CloseBrace {
                let some_htmls = parse_single(context, ts_string, parser)?;
                body.extend(some_htmls);
            }
            assert!(parser.eat(exp!(CloseBrace)));

            result.push(Html::For {
                pattern: Box::new(pattern),
                expr,
                body,
            })
        }
        TokenKind::OpenBrace => {
            let expr = match parser.parse_block() {
                Ok(expr) => expr,
                Err(error) => {
                    panic!("{:?} {:?}", error, parser.parse_tokens());
                }
            };
            result.push(Html::Expr(expr))
        }
        TokenKind::Literal(_) => {
            let Ok(literal) = parser.parse_str_lit() else {
                panic!();
            };
            result.push(Html::Literal(literal))
        }
        TokenKind::Ident(_, _) => {
            let ty = parser.parse_ty().map_err(|d| {
                let err = RewriteError::MacroFailure {
                    kind: MacroErrorKind::ParseFailure,
                    span: d.span.primary_span().unwrap(),
                };
                d.cancel();
                err
            })?;
            parse_eat!(exp!(OpenBrace));
            let mut attrs = Vec::new();
            while parser.token.kind != TokenKind::CloseBrace
                && parser.look_ahead(1, |token| {
                    token.kind == TokenKind::Colon
                        || token.kind == TokenKind::Comma
                        || (!matches!(parser.token.kind, TokenKind::Literal(_))
                            && token.kind == TokenKind::CloseBrace)
                })
            {
                // parse attribute
                let attr_name = if parser.token.is_ident() {
                    let name = HtmlAttrName::Ident(parser.token.ident().unwrap());
                    parser.bump(); // TODO FIXME
                    name
                } else if let TokenKind::Literal(_lit) = parser.token.kind {
                    // TODO FIXME this needs to have a colon value.
                    let name = HtmlAttrName::Literal(parser.parse_str_lit().unwrap());
                    name
                } else {
                    panic!("{:?}", parser.token.kind);
                };
                let value = if parser.token.kind == TokenKind::Colon {
                    assert!(parser.eat(exp!(Colon)));
                    let expr = match parser.parse_expr() {
                        Ok(expr) => expr,
                        Err(error) => {
                            panic!("{:?} {:?}", error, parser.parse_tokens());
                        }
                    };
                    Some(expr)
                } else {
                    None
                };
                attrs.push((attr_name, value));
                if parser.token.kind == TokenKind::Comma {
                    assert!(parser.eat(exp!(Comma)));
                } else {
                    break;
                }
            }
            let mut body = Vec::new();
            while parser.token.kind != TokenKind::CloseBrace {
                let some_htmls = parse_single(context, ts_string, parser)?;
                body.extend(some_htmls);
                // optional commas are allowed
                if parser.token.kind == TokenKind::Comma {
                    assert!(parser.eat(exp!(Comma)));
                }
            }
            assert!(parser.eat(exp!(CloseBrace)));
            result.push(Html::Element { ty, attrs, body });
        }
        other => panic!("unexpected token {:?} {}", other, ts_string),
    }
    Ok(result)
}

pub(crate) fn parse(
    context: &RewriteContext<'_>,
    ts: TokenStream,
) -> Result<Vec<Html>, RewriteError> {
    let ts_string = format!("{:?}", ts);
    let mut result = vec![];
    let mut parser = super::build_parser(context, ts);
    while parser.token.kind != TokenKind::Eof {
        let val = parse_single(context, &ts_string, &mut parser).expect(&ts_string);
        result.extend(val);
    }
    Ok(result)
}

fn format_inner(context: &RewriteContext<'_>, mut shape: Shape, html: &Html) -> RewriteResult {
    let mut result = String::new();
    match html {
        Html::Literal(literal) => {
            result.push_str(&shape.indent.to_string_with_newline(context.config));
            result.push_str(&rewrite_literal(
                context,
                literal.as_token_lit(),
                literal.span,
                Shape::indented(shape.indent, context.config),
            )?);
        }
        Html::Expr(p) => {
            result.push_str(&shape.indent.to_string_with_newline(context.config));
            result.push_str(&p.rewrite_result(
                context,
                Shape::indented(shape.indent, context.config).sub_width(1, p.span)?,
            )?);
        }
        Html::Element { ty, attrs, body } => {
            result.push_str(&shape.indent.to_string_with_newline(context.config));
            result.push_str(&ty.rewrite_result(
                context,
                Shape::indented(shape.indent, context.config).sub_width(1, ty.span)?,
            )?);
            result.push_str(" {");
            shape.indent = shape.indent.block_indent(context.config);
            for (name, value) in attrs {
                result.push_str(&shape.indent.to_string_with_newline(context.config));
                match name {
                    HtmlAttrName::Ident((ident, ident_is_raw)) => match ident_is_raw {
                        IdentIsRaw::No => result.push_str(ident.as_str()),
                        IdentIsRaw::Yes => {
                            result.push_str("r#");
                            result.push_str(ident.as_str())
                        }
                    },
                    HtmlAttrName::Literal(lit) => result.push_str(&rewrite_literal(
                        context,
                        lit.as_token_lit(),
                        lit.span,
                        Shape::indented(shape.indent, context.config),
                    )?),
                }
                if let Some(value) = value {
                    result.push_str(": ");
                    result.push_str(&value.rewrite_result(
                        context,
                        Shape::indented(shape.indent, context.config).sub_width(1, value.span)?,
                    )?);
                }
                result.push_str(",");
            }
            for child in body {
                result.push_str(&format_inner(context, shape, child)?);
            }
            shape.indent = shape.indent.block_unindent(context.config);
            result.push_str(&shape.indent.to_string_with_newline(context.config));
            result.push_str("}");
        }
        Html::If {
            conditional,
            body,
            else_,
        } => {
            result.push_str(&shape.indent.to_string_with_newline(context.config));
            result.push_str("if ");
            result.push_str(&conditional.rewrite_result(
                context,
                Shape::indented(shape.indent, context.config).sub_width(1, conditional.span)?,
            )?);
            result.push_str(" {");
            shape.indent = shape.indent.block_indent(context.config);
            let inner_result = format_vec(context, shape, body)?;
            result.push_str(&inner_result);
            shape.indent = shape.indent.block_unindent(context.config);
            result.push_str(&shape.indent.to_string_with_newline(context.config));
            result.push_str("}");
            if let Some(body) = else_ {
                result.push_str(" else {");
                shape.indent = shape.indent.block_indent(context.config);
                let inner_result = format_vec(context, shape, body)?;
                result.push_str(&inner_result);
                shape.indent = shape.indent.block_unindent(context.config);
                result.push_str(&shape.indent.to_string_with_newline(context.config));
                result.push_str("}");
            }
        }
        Html::For {
            pattern,
            expr,
            body,
        } => {
            result.push_str(&shape.indent.to_string_with_newline(context.config));
            result.push_str("for ");
            result.push_str(&pattern.rewrite_result(
                context,
                Shape::indented(shape.indent, context.config).sub_width(1, pattern.span)?,
            )?);
            result.push_str(" in ");
            result.push_str(&expr.rewrite_result(
                context,
                Shape::indented(shape.indent, context.config).sub_width(1, expr.span)?,
            )?);
            result.push_str(" {");
            shape.indent = shape.indent.block_indent(context.config);
            let inner_result = format_vec(context, shape, body)?;
            result.push_str(&inner_result);
            shape.indent = shape.indent.block_unindent(context.config);
            result.push_str(&shape.indent.to_string_with_newline(context.config));
            result.push_str("}");
        }
    }
    Ok(result.to_string())
}

fn format_vec(context: &RewriteContext<'_>, shape: Shape, elems: &Vec<Html>) -> RewriteResult {
    let mut result = String::new();

    for html in elems {
        let inner_result = format_inner(context, shape, html)?;
        result.push_str(&inner_result);
    }

    Ok(result.clone())
}

pub(crate) fn format(
    context: &RewriteContext<'_>,
    mut shape: Shape,
    ts: TokenStream,
) -> RewriteResult {
    let mut result = String::new();

    result.push_str("rsx! {");
    shape.indent = shape.indent.block_indent(context.config);

    let parsed_elems = parse(context, ts)?;

    result.push_str(&format_vec(context, shape, &parsed_elems)?);

    shape.indent = shape.indent.block_unindent(context.config);
    result.push_str(&shape.indent.to_string_with_newline(context.config));
    result.push('}');

    Ok(result)
}
