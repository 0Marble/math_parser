use super::expr::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Num(f32),
    Plus,
    Minus,
    Multiply,
    Divide,
    Identifier(String),
    OpenBracket,
    CloseBracket,
    Coma,
}

pub fn tokenize(mut src: &str) -> Option<Vec<Token>> {
    let mut res = vec![];
    loop {
        src = src.trim_start();

        if let Some(next) = src.strip_prefix('(') {
            src = next;
            res.push(Token::OpenBracket);
        } else if let Some(next) = src.strip_prefix(')') {
            src = next;
            res.push(Token::CloseBracket);
        } else if let Some(next) = src.strip_prefix(',') {
            src = next;
            res.push(Token::Coma);
        } else if let Some(next) = src.strip_prefix('+') {
            src = next;
            res.push(Token::Plus);
        } else if let Some(next) = src.strip_prefix('-') {
            src = next;
            res.push(Token::Minus);
        } else if let Some(next) = src.strip_prefix('*') {
            src = next;
            res.push(Token::Multiply);
        } else if let Some(next) = src.strip_prefix('/') {
            src = next;
            res.push(Token::Divide);
        } else if let Some((num, next)) = read_number(src) {
            src = next;
            res.push(Token::Num(num));
        } else if let Some((identifier, next)) = read_identifier(src) {
            src = next;
            res.push(Token::Identifier(identifier));
        } else if src.is_empty() {
            return Some(res);
        } else {
            return None;
        }
    }
}

fn read_number(src: &str) -> Option<(f32, &str)> {
    let src = src.trim_start();
    let (before_dot, before_dot_str_size) = src
        .char_indices()
        .map_while(|(i, c)| c.to_digit(10).map(|d| (d, i)))
        .fold((0, 0), |(acc, _), (d, i)| (acc * 10 + d, i + 1));
    if before_dot_str_size == 0 {
        return None;
    }

    if let Some(next) = src[before_dot_str_size..].strip_prefix('.') {
        let (after_dot, after_dot_divisor, after_dot_str_size) = next
            .char_indices()
            .map_while(|(i, c)| c.to_digit(10).map(|d| (d, i)))
            .fold((0, 1, 0), |(acc, divisor, _), (d, i)| {
                (acc * 10 + d, divisor * 10, i + 1)
            });
        if after_dot_str_size == 0 {
            return None;
        }

        Some((
            before_dot as f32 + (after_dot as f32) / (after_dot_divisor as f32),
            &next[after_dot_str_size..],
        ))
    } else {
        Some((before_dot as f32, &src[before_dot_str_size..]))
    }
}

const RESERVED_SYMBOLS: [char; 7] = ['+', '-', '*', '/', ',', '(', ')'];

fn read_identifier(src: &str) -> Option<(String, &str)> {
    let src = src.trim_start();

    let (identifier, len) = src
        .char_indices()
        .take_while(|(_, c)| !c.is_whitespace() && RESERVED_SYMBOLS.iter().all(|sym| c != sym))
        .fold(("".to_string(), 0), |(mut acc, _), (i, c)| {
            acc.push(c);
            (acc, i + 1)
        });

    if len == 0 || identifier.starts_with(|c: char| c.is_ascii_digit()) {
        None
    } else {
        Some((identifier, &src[len..]))
    }
}

#[test]
fn tokenizer() {
    let expr = "122+904-23.23*(72-x/4)+pow(2,y)";
    let expr_tokenized = vec![
        Token::Num(122.0),
        Token::Plus,
        Token::Num(904.0),
        Token::Minus,
        Token::Num(23.23),
        Token::Multiply,
        Token::OpenBracket,
        Token::Num(72.0),
        Token::Minus,
        Token::Identifier("x".to_string()),
        Token::Divide,
        Token::Num(4.0),
        Token::CloseBracket,
        Token::Plus,
        Token::Identifier("pow".to_string()),
        Token::OpenBracket,
        Token::Num(2.0),
        Token::Coma,
        Token::Identifier("y".to_string()),
        Token::CloseBracket,
    ];

    assert_eq!(tokenize(expr), Some(expr_tokenized));
}

/*
    expr = expr ('+' | '-') term | term
    term = term ('*' | '/' ) factor | -term | implicit_multiplication | factor
    factor = number | variable | func '(' arglist ')' | '(' expr ')'
    implicit_multiplication = factor term
    arglist = expr (',' expr)*

    variable = identifier
    func = identifier
*/

pub fn parse_expr<'a>(
    tokens: &[Token],
    language: &'a dyn Language,
) -> Option<Box<dyn Expression + 'a>> {
    [Token::Plus, Token::Minus]
        .iter()
        .find_map(|op| {
            tokens.iter().enumerate().find_map(|(i, t)| {
                if t.eq(op) {
                    let expr: Box<dyn Expression> = match op {
                        Token::Plus => Box::new(BasicOp::Plus(
                            parse_expr(&tokens[..i], language)?,
                            parse_term(&tokens[i + 1..], language)?,
                        )),
                        Token::Minus => Box::new(BasicOp::Minus(
                            parse_expr(&tokens[..i], language)?,
                            parse_term(&tokens[i + 1..], language)?,
                        )),
                        _ => unreachable!(),
                    };
                    Some(expr)
                } else {
                    None
                }
            })
        })
        .or_else(|| parse_term(tokens, language))
}

fn parse_term<'a>(
    tokens: &[Token],
    language: &'a dyn Language,
) -> Option<Box<dyn Expression + 'a>> {
    [Token::Multiply, Token::Divide]
        .iter()
        .find_map(|op| {
            tokens.iter().enumerate().find_map(|(i, t)| {
                if t.eq(op) {
                    let expr: Box<dyn Expression> = match op {
                        Token::Multiply => Box::new(BasicOp::Multiply(
                            parse_term(&tokens[..i], language)?,
                            parse_factor(&tokens[i + 1..], language)?,
                        )),
                        Token::Divide => Box::new(BasicOp::Divide(
                            parse_term(&tokens[..i], language)?,
                            parse_factor(&tokens[i + 1..], language)?,
                        )),
                        _ => unreachable!(),
                    };
                    Some(expr)
                } else {
                    None
                }
            })
        })
        .or_else(|| {
            tokens.first().and_then(|t| match t {
                Token::Minus if tokens.len() > 1 => Some(Box::new(BasicOp::Negate(parse_factor(
                    &tokens[1..],
                    language,
                )?))
                    as Box<dyn Expression>),
                _ => None,
            })
        })
        .or_else(|| parse_implicit_multiplication(tokens, language))
        .or_else(|| parse_factor(tokens, language))
}

fn parse_implicit_multiplication<'a>(
    tokens: &[Token],
    language: &'a dyn Language,
) -> Option<Box<dyn Expression + 'a>> {
    None
}

fn parse_factor<'a>(
    tokens: &[Token],
    language: &'a dyn Language,
) -> Option<Box<dyn Expression + 'a>> {
    match tokens.first()? {
        Token::Num(num) if tokens.len() == 1 => Some(Box::new(*num) as Box<dyn Expression>),
        Token::Identifier(id)
            if tokens.get(1) == Some(&Token::OpenBracket)
                && tokens.last() == Some(&Token::CloseBracket)
                && tokens.len() > 3
                && language.find_func(id).is_some() =>
        {
            Some(FunctionExpression::new_expression(
                language,
                parse_arglist(&tokens[2..tokens.len() - 1], language)?,
                id.to_owned(),
            ))
        }
        Token::Identifier(id) if tokens.len() == 1 && language.find_func(id).is_none() => {
            Some(Variable::new_expression(id.to_owned()))
        }
        Token::OpenBracket if Some(&Token::CloseBracket) == tokens.last() => {
            parse_expr(&tokens[1..tokens.len() - 1], language)
        }
        _ => None,
    }
}

fn parse_arglist<'a>(
    tokens: &[Token],
    language: &'a dyn Language,
) -> Option<Vec<Box<dyn Expression + 'a>>> {
    let mut args = vec![];
    let mut coma_iterator = tokens
        .iter()
        .enumerate()
        .scan(0, |state, (i, t)| {
            match t {
                Token::CloseBracket => *state -= 1,
                Token::OpenBracket => *state += 1,
                _ => {}
            }

            Some((i, t, *state))
        })
        .filter_map(|(i, t, bracket_level)| {
            if t.eq(&Token::Coma) && bracket_level == 0 {
                Some(i)
            } else {
                None
            }
        });

    let mut arg_start = 0;
    loop {
        let next_coma = coma_iterator.next();
        if let Some(i) = next_coma {
            args.push(parse_expr(&tokens[arg_start..i], language)?);
            arg_start = i + 1;
        } else {
            args.push(parse_expr(&tokens[arg_start..], language)?);
            return Some(args);
        }
    }
}
