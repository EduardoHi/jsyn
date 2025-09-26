use super::Expr;
use super::Program;
use nom::bytes::complete::take_while1;
use nom::character::complete::char;
use nom::character::complete::multispace0;
use nom::combinator::all_consuming;
use nom::combinator::cut;
use nom::error::convert_error;
use nom::error::VerboseError;
use nom::error::VerboseErrorKind;
use nom::multi::many0;
use nom::sequence::delimited;
use nom::sequence::tuple;
use nom::IResult;
use serde_json::Value;
use thiserror::Error;

type NomResult<'a, T> = IResult<&'a str, T, VerboseError<&'a str>>;

#[derive(Debug, Error)]
pub enum ExprParseError {
    #[error("failed to parse expression S-expression: {0}")]
    Invalid(String),
}

pub fn parse_program_sexpr(input: &str) -> Result<Program, ExprParseError> {
    let expr = parse_expr_sexpr(input)?;
    Ok(Program { body: expr })
}

pub fn parse_expr_sexpr(input: &str) -> Result<Expr, ExprParseError> {
    match all_consuming(ws(expr))(input) {
        Ok((_, value)) => Ok(value),
        Err(nom::Err::Failure(err)) | Err(nom::Err::Error(err)) => {
            Err(ExprParseError::Invalid(convert_error(input, err)))
        }
        Err(nom::Err::Incomplete(_)) => Err(ExprParseError::Invalid(
            "incomplete input while parsing expression".to_string(),
        )),
    }
}

fn expr(input: &str) -> NomResult<'_, Expr> {
    delimited(ws(char('(')), cut(list_expr), ws(char(')')))(input)
}

fn list_expr(input: &str) -> NomResult<'_, Expr> {
    let (input, head) = ws(symbol)(input)?;
    match head.as_str() {
        "const" => const_expr(input),
        "id" => no_arg_expr(input, Expr::Id),
        "keys" => no_arg_expr(input, Expr::Keys),
        "elements" => no_arg_expr(input, Expr::Elements),
        "flatten" => no_arg_expr(input, Expr::Flatten),
        "tolist" => no_arg_expr(input, Expr::ToList),
        "get" => unary_expr(input, Expr::Get),
        "map" => unary_expr(input, Expr::Map),
        "not" => unary_expr(input, Expr::Not),
        "equal" => binary_expr(input, Expr::Equal),
        "and" => binary_expr(input, Expr::And),
        "or" => binary_expr(input, Expr::Or),
        "pipe" => binary_expr(input, Expr::Pipe),
        "union" => binary_expr(input, Expr::Union),
        "concat" => binary_expr(input, Expr::Concat),
        "construct" => construct_expr(input),
        _ => Err(nom::Err::Failure(VerboseError {
            errors: vec![(input, VerboseErrorKind::Context("unknown expression head"))],
        })),
    }
}

fn no_arg_expr(input: &str, expr: Expr) -> NomResult<'_, Expr> {
    Ok((input, expr))
}

fn unary_expr<F>(input: &str, constructor: F) -> NomResult<'_, Expr>
where
    F: Fn(Box<Expr>) -> Expr,
{
    let (input, value) = ws(expr)(input)?;
    Ok((input, constructor(Box::new(value))))
}

fn binary_expr<F>(input: &str, constructor: F) -> NomResult<'_, Expr>
where
    F: Fn(Box<Expr>, Box<Expr>) -> Expr,
{
    let (input, lhs) = ws(expr)(input)?;
    let (input, rhs) = ws(expr)(input)?;
    Ok((input, constructor(Box::new(lhs), Box::new(rhs))))
}

fn const_expr(input: &str) -> NomResult<'_, Expr> {
    let (input, value) = ws(json_value)(input)?;
    Ok((input, Expr::Const(value)))
}

fn construct_expr(input: &str) -> NomResult<'_, Expr> {
    let (input, fields) = many0(ws(construct_field))(input)?;
    Ok((input, Expr::Construct(fields)))
}

fn construct_field(input: &str) -> NomResult<'_, (Expr, Expr)> {
    delimited(
        ws(char('(')),
        cut(tuple((ws(expr), ws(expr)))),
        ws(char(')')),
    )(input)
}

fn symbol(input: &str) -> NomResult<'_, String> {
    let (input, value) =
        take_while1(|ch: char| !ch.is_whitespace() && ch != '(' && ch != ')')(input)?;
    Ok((input, value.to_string()))
}

fn ws<'a, F, O>(mut inner: F) -> impl FnMut(&'a str) -> NomResult<'a, O>
where
    F: FnMut(&'a str) -> NomResult<'a, O>,
{
    move |input| {
        let (input, _) = multispace0(input)?;
        let (input, output) = inner(input)?;
        let (input, _) = multispace0(input)?;
        Ok((input, output))
    }
}

fn json_value(input: &str) -> NomResult<'_, Value> {
    match take_json_raw(input) {
        Some((raw, rest)) => match serde_json::from_str::<Value>(raw) {
            Ok(value) => Ok((rest, value)),
            Err(_) => Err(nom::Err::Failure(VerboseError {
                errors: vec![(input, VerboseErrorKind::Context("invalid JSON literal"))],
            })),
        },
        None => Err(nom::Err::Failure(VerboseError {
            errors: vec![(input, VerboseErrorKind::Context("invalid JSON literal"))],
        })),
    }
}

fn take_json_raw(input: &str) -> Option<(&str, &str)> {
    let mut chars = input.char_indices();
    let (start, first) = chars.next()?;
    debug_assert_eq!(start, 0);

    match first {
        '"' => {
            let end = scan_string(input)?;
            Some(input.split_at(end))
        }
        '{' => {
            let end = scan_bracketed(input, '{', '}')?;
            Some(input.split_at(end))
        }
        '[' => {
            let end = scan_bracketed(input, '[', ']')?;
            Some(input.split_at(end))
        }
        '-' | '0'..='9' => {
            let end = scan_number(input);
            Some(input.split_at(end))
        }
        't' => {
            if input.starts_with("true") {
                Some(input.split_at(4))
            } else {
                None
            }
        }
        'f' => {
            if input.starts_with("false") {
                Some(input.split_at(5))
            } else {
                None
            }
        }
        'n' => {
            if input.starts_with("null") {
                Some(input.split_at(4))
            } else {
                None
            }
        }
        _ => None,
    }
}

fn scan_string(input: &str) -> Option<usize> {
    let mut escaped = false;
    for (idx, ch) in input.char_indices().skip(1) {
        if escaped {
            escaped = false;
            continue;
        }
        match ch {
            '\\' => escaped = true,
            '"' => return Some(idx + ch.len_utf8()),
            _ => {}
        }
    }
    None
}

fn scan_bracketed(input: &str, open: char, close: char) -> Option<usize> {
    let mut depth = 0usize;
    let mut escaped = false;
    let mut in_string = false;
    for (idx, ch) in input.char_indices() {
        if in_string {
            if escaped {
                escaped = false;
                continue;
            }
            match ch {
                '\\' => escaped = true,
                '"' => in_string = false,
                _ => {}
            }
            continue;
        }

        match ch {
            '"' => in_string = true,
            ch if ch == open => depth += 1,
            ch if ch == close => {
                depth = depth.checked_sub(1)?;
                if depth == 0 {
                    return Some(idx + ch.len_utf8());
                }
            }
            _ => {}
        }
    }
    None
}

fn scan_number(input: &str) -> usize {
    let mut end = 0;
    for (idx, ch) in input.char_indices() {
        match ch {
            '0'..='9' | '-' | '+' | '.' | 'e' | 'E' => {
                end = idx + ch.len_utf8();
            }
            _ => break,
        }
    }
    if end == 0 {
        1
    } else {
        end
    }
}

#[cfg(test)]
mod tests {
    use super::parse_expr_sexpr;
    use crate::dsl::Expr;
    use serde_json::json;

    #[test]
    fn parses_const_number() {
        let expr = parse_expr_sexpr("(const 42)").expect("parse");
        assert_eq!(expr, Expr::Const(json!(42)));
    }

    #[test]
    fn parses_construct() {
        let program =
            "(construct ((const \"name\") (get (const \"name\"))) ((const \"age\") (const 30)))";
        let expr = parse_expr_sexpr(program).expect("parse");
        if let Expr::Construct(fields) = expr {
            assert_eq!(fields.len(), 2);
        } else {
            panic!("expected construct");
        }
    }
}
