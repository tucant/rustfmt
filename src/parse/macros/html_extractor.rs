use crate::config::lists::DefinitiveListTactic;
use crate::config::lists::ListTactic;
use crate::config::lists::SeparatorTactic;
use crate::expr::rewrite_literal;
use crate::lists::ListFormatting;
use crate::lists::ListItem;
use crate::lists::Separator;
use crate::lists::definitive_tactic;
use crate::lists::itemize_list;
use crate::lists::write_list;
use crate::rewrite::Rewrite;
use crate::rewrite::RewriteContext;
use crate::rewrite::RewriteResult;
use crate::shape::Indent;
use crate::shape::Shape;
use crate::vertical::AlignedItem;
use itertools::Itertools;
use rustc_ast::Block;
use rustc_ast::token::TokenKind;
use rustc_ast::tokenstream::TokenStream;
use rustc_ast::{Expr, StrLit};
use rustc_parse::exp;
use rustc_parse::parser::LetChainsPolicy;
use rustc_parse::parser::Parser;
use rustc_span::symbol::Ident;

pub(crate) enum HtmlAttributeValue {
    Expr(Box<Expr>),
    Literal(StrLit),
    Ident(Ident),
}

pub(crate) struct HtmlAttr {
    base_ident: Ident,
    rest_ident: Vec<(TokenKind, Ident)>,
    value: HtmlAttributeValue,
}

impl AlignedItem for HtmlAttr {
    fn skip(&self) -> bool {
        false
    }

    fn get_span(&self) -> rustc_span::Span {
        self.base_ident.span
    }

    fn rewrite_prefix(&self, _context: &RewriteContext<'_>, _shape: Shape) -> RewriteResult {
        let mut result = String::new();
        result.push_str(self.base_ident.as_str());
        result.push_str(
            &self
                .rest_ident
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
        Ok(result)
    }

    fn rewrite_aligned_item(
        &self,
        context: &RewriteContext<'_>,
        shape: Shape,
        _prefix_max_width: usize,
    ) -> RewriteResult {
        let mut result = String::new();
        result.push_str(self.base_ident.as_str());
        result.push_str(
            &self
                .rest_ident
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
        match &self.value {
            HtmlAttributeValue::Expr(p) => {
                result.push_str("{");
                result.push_str(
                    &p.rewrite_result(
                        context,
                        Shape::indented(shape.indent.block_indent(context.config), context.config)
                            .sub_width(result.len() + 3, p.span)?,
                    )
                    .unwrap(),
                );
                result.push_str("}");
            }
            HtmlAttributeValue::Literal(str_lit) => {
                result.push_str(&rewrite_literal(
                    context,
                    str_lit.as_token_lit(),
                    str_lit.span,
                    shape,
                )?);
            }
            HtmlAttributeValue::Ident(ident) => result.push_str(ident.as_str()),
        }
        Ok(result)
    }
}

pub(crate) enum Html {
    Expr(Box<Expr>),
    Literal(StrLit),
    Ident(Ident),
    Open {
        tag: Ident,
        attrs: Vec<HtmlAttr>,
    },
    Close {
        tag: Ident,
    },
    If {
        conditional: Box<Expr>,
        body: Vec<Html>,
        variable: Ident,
        result_expr: Box<Expr>,
        else_: Option<(Vec<Html>, Box<Expr>)>,
    },
    While {
        conditional: Box<Expr>,
        body: Vec<Html>,
        variable: Ident,
        result_expr: Box<Expr>,
    },
    Let {
        variable: Ident,
        expr: Box<Expr>,
    },
    Use(Box<Expr>),
    Extern(Box<Block>),
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
                    let conditional = match parser.parse_expr_cond(LetChainsPolicy::AlwaysAllowed) {
                        Ok(expr) => expr,
                        Err(error) => {
                            panic!("{:?} {:?}", error, parser.parse_tokens());
                        }
                    };
                    assert!(parser.eat(exp!(OpenBrace)));
                    let mut body = Vec::new();
                    while parser.token.kind != TokenKind::CloseBrace {
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
                        while parser.token.kind != TokenKind::CloseBrace {
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
                    let conditional = match parser.parse_expr_cond(LetChainsPolicy::AlwaysAllowed) {
                        Ok(expr) => expr,
                        Err(error) => {
                            panic!("{:?} {:?}", error, parser.parse_tokens());
                        }
                    };
                    assert!(parser.eat(exp!(OpenBrace)));
                    let mut body = Vec::new();
                    while parser.token.kind != TokenKind::CloseBrace {
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
        TokenKind::OpenBrace => {
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
            let ident = parser.token.ident().unwrap().0;
            parser.bump();
            result.push(Html::Ident(ident))
        }
        TokenKind::Lt => {
            parse_eat!(exp!(Lt));
            match parser.token.kind {
                TokenKind::Slash => {
                    parser.bump();
                    let id = parser.token.ident().unwrap().0;
                    parser.bump();
                    parse_eat!(exp!(Gt));
                    result.push(Html::Close { tag: id });
                }
                _ => {
                    let id = parser.token.ident().expect(&ts_string).0;
                    parser.bump();
                    let mut attrs: Vec<HtmlAttr> = Vec::new();
                    while parser.token.kind != TokenKind::Gt {
                        let base_id = parser.token.ident().expect(&ts_string).0;
                        parser.bump();
                        let mut rest_id = Vec::new();
                        while parser.token.kind == TokenKind::Colon
                            || parser.token.kind == TokenKind::Minus
                        {
                            let delimiter = parser.token.kind.clone();
                            parser.bump();
                            let i = parser.token.ident().unwrap().0;
                            parser.bump();
                            rest_id.push((delimiter, i));
                        }
                        parse_eat!(exp!(Eq));
                        match &parser.token.kind {
                            TokenKind::OpenBrace => {
                                assert!(parser.eat(exp!(OpenBrace)));
                                let expr = match parser.parse_expr() {
                                    Ok(expr) => expr,
                                    Err(error) => {
                                        panic!("{:?} {:?}", error, parser.parse_tokens());
                                    }
                                };
                                assert!(parser.eat(exp!(CloseBrace)));
                                attrs.push(HtmlAttr {
                                    base_ident: base_id,
                                    rest_ident: rest_id,
                                    value: HtmlAttributeValue::Expr(expr),
                                });
                            }
                            TokenKind::Literal(_) => {
                                let Ok(literal) = parser.parse_str_lit() else {
                                    panic!();
                                };
                                attrs.push(HtmlAttr {
                                    base_ident: base_id,
                                    rest_ident: rest_id,
                                    value: HtmlAttributeValue::Literal(literal),
                                });
                            }
                            TokenKind::Ident(_, _) => {
                                let ident = parser.token.ident().unwrap().0;
                                parser.bump();
                                attrs.push(HtmlAttr {
                                    base_ident: base_id,
                                    rest_ident: rest_id,
                                    value: HtmlAttributeValue::Ident(ident),
                                })
                            }
                            _ => panic!(),
                        }
                    }
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

fn format_html_extractor_html_inner(
    context: &RewriteContext<'_>,
    shape: Shape,
    indent: &mut Indent,
    result: &mut String,
    html: &Html,
) -> RewriteResult {
    match html {
        Html::Literal(literal) => {
            result.push_str(&indent.to_string_with_newline(context.config));
            result.push_str(&rewrite_literal(
                context,
                literal.as_token_lit(),
                literal.span,
                Shape::indented(*indent, context.config),
            )?);
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
                    Shape::indented(*indent, context.config).sub_width(1, p.span)?,
                )
                .unwrap(),
            );
            result.push_str("}");
        }
        Html::Open { tag, attrs } => {
            result.push_str(&indent.to_string_with_newline(context.config));
            result.push_str("<");
            result.push_str(tag.as_str());
            if attrs.len() > 0 {
                let item_shape = Shape::indented(*indent, context.config);

                let mut items = itemize_list(
                    context.snippet_provider,
                    attrs.iter(),
                    "\n",
                    "",
                    |field| field.get_span().lo(),
                    |field| field.get_span().hi(),
                    |field| field.rewrite_aligned_item(context, item_shape, 0),
                    tag.span.lo(),
                    tag.span.hi(),
                    false,
                )
                .collect::<Vec<_>>();

                let tactic = definitive_tactic(
                    &items,
                    ListTactic::HorizontalVertical,
                    Separator::Space,
                    item_shape.width.saturating_sub(1),
                );

                if tactic == DefinitiveListTactic::Horizontal {
                    // since the items fits on a line, there is no need to align them
                    let do_rewrite = |field: &HtmlAttr| -> RewriteResult {
                        field.rewrite_aligned_item(context, item_shape, 0)
                    };
                    attrs.iter().zip(items.iter_mut()).for_each(
                        |(field, list_item): (&HtmlAttr, &mut ListItem)| {
                            if list_item.item.is_ok() {
                                list_item.item = do_rewrite(field);
                            }
                        },
                    );
                }

                let fmt = ListFormatting::new(
                    Shape::indented(indent.block_indent(context.config), context.config),
                    context.config,
                )
                .separator("")
                .tactic(tactic)
                .trailing_separator(SeparatorTactic::Never)
                .ends_with_newline(true)
                .preserve_newline(true);
                let the_result = write_list(&items, &fmt)?;
                if the_result.contains("\n") {
                    result.push_str(
                        &indent
                            .block_indent(context.config)
                            .to_string_with_newline(context.config),
                    );
                } else {
                    result.push_str(" ");
                }
                result.push_str(&the_result);
                if the_result.contains("\n") {
                    result.push_str(&indent.to_string_with_newline(context.config));
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
                &conditional.rewrite_result(
                    context,
                    Shape::indented(*indent, context.config)
                        // TODO FIXME calculate rest of line?
                        .sub_width(
                            10 + variable.as_str().len() + indent.block_indent,
                            conditional.span,
                        )?,
                )?,
            );
            result.push_str(" {");
            *indent = indent.block_indent(context.config);
            let indent_after = &mut indent.clone();
            format_html_extractor_html_vec(context, shape, indent_after, result, body)?;
            *indent = indent.block_unindent(context.config);
            result.push_str(&indent.to_string_with_newline(context.config));
            result.push_str("} ");
            result.push_str("=> ");
            result.push_str(&result_expr.rewrite_result(
                context,
                Shape::indented(*indent, context.config).sub_width(1, result_expr.span)?,
            )?);
            if let Some((body, result_expr)) = else_ {
                result.push_str(" else {");
                *indent = indent.block_indent(context.config);
                format_html_extractor_html_vec(context, shape, &mut indent.clone(), result, body)?;
                *indent = indent.block_unindent(context.config);
                result.push_str(&indent.to_string_with_newline(context.config));
                result.push_str("} => ");
                result.push_str(
                    &result_expr
                        .rewrite_result(
                            context,
                            Shape::indented(*indent, context.config)
                                .sub_width(1, result_expr.span)?,
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
                        Shape::indented(*indent, context.config).sub_width(1, conditional.span)?,
                    )
                    .unwrap(),
            );
            result.push_str(" {");
            *indent = indent.block_indent(context.config);
            format_html_extractor_html_vec(context, shape, &mut indent.clone(), result, body)?;
            *indent = indent.block_unindent(context.config);
            result.push_str(&indent.to_string_with_newline(context.config));
            result.push_str("} => ");
            result.push_str(
                &result_expr
                    .rewrite_result(
                        context,
                        Shape::indented(*indent, context.config).sub_width(1, result_expr.span)?,
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
                        Shape::indented(*indent, context.config).sub_width(1, expr.span)?,
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
                        Shape::indented(*indent, context.config).sub_width(1, expr.span)?,
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
                        Shape::indented(*indent, context.config).sub_width(1, block.span)?,
                    )
                    .unwrap(),
            );
        }
    }
    Ok(result.to_string())
}

fn format_html_extractor_html_vec(
    context: &RewriteContext<'_>,
    shape: Shape,
    indent: &mut Indent,
    result: &mut String,
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
            Html::Let {
                variable: _,
                expr: _,
            } => 0,
            Html::Use(_expr) => 0,
            Html::Extern(_block) => 0,
        };
        min_indent = std::cmp::max(min_indent, indent_amount);
    }

    for _ in 0..min_indent {
        *indent = indent.block_indent(context.config);
    }

    for html in elems {
        format_html_extractor_html_inner(context, shape, indent, result, html)?;
    }

    for _ in 0..min_indent {
        *indent = indent.block_unindent(context.config);
    }

    Ok(result.clone())
}

pub(crate) fn format(context: &RewriteContext<'_>, shape: Shape, ts: TokenStream) -> RewriteResult {
    let mut result = String::new();

    result.push_str("html_extractor::html! {");

    let parsed_elems = parse_html(context, ts).unwrap();
    let mut indent = shape.indent.block_indent(context.config);
    format_html_extractor_html_vec(context, shape, &mut indent, &mut result, &parsed_elems)?;

    result.push_str(&shape.indent.to_string_with_newline(context.config));
    result.push('}');

    Ok(result)
}
