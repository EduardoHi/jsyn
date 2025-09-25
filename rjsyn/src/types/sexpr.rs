use super::ValTy;
use indexmap::IndexMap;
use nom::branch::alt;
use nom::bytes::complete::{escaped_transform, is_not, tag};
use nom::character::complete::{char, multispace0};
use nom::combinator::{all_consuming, cut, map, opt, value};
use nom::error::{convert_error, VerboseError};
use nom::multi::many0;
use nom::sequence::{delimited, preceded, tuple};
use nom::IResult;
use std::fmt::{self, Write};
use thiserror::Error;

const INDENT: usize = 2;

type NomResult<'a, T> = IResult<&'a str, T, VerboseError<&'a str>>;

#[derive(Debug, Error)]
pub enum ValTyParseError {
    #[error("failed to parse ValTy S-expression: {0}")]
    Invalid(String),
}

pub fn format_valty_compact(value: &ValTy) -> String {
    let mut buf = String::new();
    write_valty(value, 0, false, &mut buf).expect("writing to string cannot fail");
    buf
}

pub fn format_valty_pretty(value: &ValTy) -> String {
    let mut buf = String::new();
    write_valty(value, 0, true, &mut buf).expect("writing to string cannot fail");
    buf
}

fn write_valty(value: &ValTy, indent: usize, pretty: bool, buf: &mut String) -> fmt::Result {
    match value {
        ValTy::Value => buf.write_str("value"),
        ValTy::String => buf.write_str("string"),
        ValTy::Number => buf.write_str("number"),
        ValTy::Bool => buf.write_str("bool"),
        ValTy::Null => buf.write_str("null"),
        ValTy::Array(inner) => {
            if pretty {
                buf.write_str("(array")?;
                buf.push('\n');
                indent_line(buf, indent + INDENT);
                write_valty(inner, indent + INDENT, true, buf)?;
                buf.push(')');
                Ok(())
            } else {
                buf.write_str("(array ")?;
                write_valty(inner, indent, false, buf)?;
                buf.push(')');
                Ok(())
            }
        }
        ValTy::Object(fields) => {
            if fields.is_empty() {
                return buf.write_str("(object)");
            }

            buf.write_str("(object")?;
            for (key, ty) in fields.iter() {
                if pretty {
                    buf.push('\n');
                    indent_line(buf, indent + INDENT);
                    write_object_field_pretty(key, ty, indent + INDENT, buf)?;
                } else {
                    buf.push(' ');
                    write_object_field_compact(key, ty, buf)?;
                }
            }
            if pretty {
                buf.push('\n');
                indent_line(buf, indent);
            }
            buf.push(')');
            Ok(())
        }
    }
}

fn write_object_field_compact(key: &str, ty: &ValTy, buf: &mut String) -> fmt::Result {
    buf.push('(');
    push_string_literal(key, buf);
    buf.push(' ');
    write_valty(ty, 0, false, buf)?;
    buf.push(')');
    Ok(())
}

fn write_object_field_pretty(
    key: &str,
    ty: &ValTy,
    indent: usize,
    buf: &mut String,
) -> fmt::Result {
    buf.push('(');
    push_string_literal(key, buf);
    buf.push('\n');
    indent_line(buf, indent + INDENT);
    write_valty(ty, indent + INDENT, true, buf)?;
    buf.push(')');
    Ok(())
}

fn indent_line(buf: &mut String, indent: usize) {
    for _ in 0..indent {
        buf.push(' ');
    }
}

fn push_string_literal(value: &str, buf: &mut String) {
    buf.push('"');
    for ch in value.chars() {
        match ch {
            '\\' => buf.push_str("\\\\"),
            '"' => buf.push_str("\\\""),
            '\n' => buf.push_str("\\n"),
            '\r' => buf.push_str("\\r"),
            '\t' => buf.push_str("\\t"),
            other => buf.push(other),
        }
    }
    buf.push('"');
}

pub fn parse_valty_sexpr(input: &str) -> Result<ValTy, ValTyParseError> {
    match all_consuming(ws(valty_expr))(input) {
        Ok((_, value)) => Ok(value),
        Err(nom::Err::Failure(err)) | Err(nom::Err::Error(err)) => {
            Err(ValTyParseError::Invalid(convert_error(input, err)))
        }
        Err(nom::Err::Incomplete(_)) => Err(ValTyParseError::Invalid(
            "incomplete input while parsing ValTy".to_string(),
        )),
    }
}

fn valty_expr(input: &str) -> NomResult<ValTy> {
    alt((object_expr, array_expr, atom_expr))(input)
}

fn atom_expr(input: &str) -> NomResult<ValTy> {
    ws(alt((
        value(ValTy::Value, tag("value")),
        value(ValTy::String, tag("string")),
        value(ValTy::Number, tag("number")),
        value(ValTy::Bool, tag("bool")),
        value(ValTy::Null, tag("null")),
    )))(input)
}

fn array_expr(input: &str) -> NomResult<ValTy> {
    let (input, inner) = delimited(
        ws(char('(')),
        preceded(ws(tag("array")), cut(valty_expr)),
        ws(char(')')),
    )(input)?;
    Ok((input, ValTy::Array(Box::new(inner))))
}

fn object_expr(input: &str) -> NomResult<ValTy> {
    let (input, entries) = delimited(
        ws(char('(')),
        preceded(ws(tag("object")), many0(ws(object_field))),
        ws(char(')')),
    )(input)?;

    match entries_to_map(input, entries) {
        Ok(map) => Ok((input, ValTy::Object(map))),
        Err(err) => Err(err),
    }
}

fn entries_to_map(
    input: &str,
    entries: Vec<(String, ValTy)>,
) -> Result<IndexMap<String, ValTy>, nom::Err<VerboseError<&str>>> {
    let mut map = IndexMap::new();
    for (key, ty) in entries {
        if map.insert(key.clone(), ty).is_some() {
            let mut error = VerboseError { errors: Vec::new() };
            error.errors.push((
                input,
                nom::error::VerboseErrorKind::Context("duplicate object key"),
            ));
            return Err(nom::Err::Failure(error));
        }
    }
    Ok(map)
}

fn object_field(input: &str) -> NomResult<(String, ValTy)> {
    let (input, (key, value)) = delimited(
        ws(char('(')),
        cut(tuple((ws(string_literal), valty_expr))),
        ws(char(')')),
    )(input)?;
    Ok((input, (key, value)))
}

fn string_literal(input: &str) -> NomResult<String> {
    let escaped = escaped_transform(
        is_not("\\\""),
        '\\',
        alt((
            value("\\", tag("\\")),
            value("\"", tag("\"")),
            value("\n", tag("n")),
            value("\r", tag("r")),
            value("\t", tag("t")),
        )),
    );
    let (input, _) = char('"')(input)?;
    let (input, content) = opt(map(escaped, String::from))(input)?;
    let (input, _) = char('"')(input)?;
    Ok((input, content.unwrap_or_default()))
}

fn ws<'a, F, O>(mut inner: F) -> impl FnMut(&'a str) -> NomResult<'a, O>
where
    F: FnMut(&'a str) -> NomResult<'a, O>,
{
    move |input: &'a str| {
        let (input, _) = multispace0(input)?;
        let (input, value) = inner(input)?;
        let (input, _) = multispace0(input)?;
        Ok((input, value))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn compact_scalars() {
        assert_eq!(format_valty_compact(&ValTy::Value), "value");
        assert_eq!(format_valty_compact(&ValTy::String), "string");
        assert_eq!(format_valty_compact(&ValTy::Number), "number");
        assert_eq!(format_valty_compact(&ValTy::Bool), "bool");
        assert_eq!(format_valty_compact(&ValTy::Null), "null");
    }

    #[test]
    fn pretty_object_roundtrip() {
        let mut map = IndexMap::new();
        map.insert("name".to_string(), ValTy::String);
        map.insert(
            "items".to_string(),
            ValTy::Array(Box::new(ValTy::Object(IndexMap::new()))),
        );
        let ty = ValTy::Object(map);
        let pretty = format_valty_pretty(&ty);
        let reparsed = parse_valty_sexpr(&pretty).expect("pretty ValTy should parse");
        assert_eq!(ty, reparsed);
    }

    #[test]
    fn compact_roundtrip_nested() {
        let ty = ValTy::Array(Box::new(ValTy::Object(IndexMap::from([
            ("a".to_string(), ValTy::Number),
            ("b".to_string(), ValTy::Array(Box::new(ValTy::Bool))),
        ]))));
        let text = format_valty_compact(&ty);
        let reparsed = parse_valty_sexpr(&text).expect("compact string should parse");
        assert_eq!(ty, reparsed);
    }

    #[test]
    fn parse_with_whitespace() {
        let input = "\n  (array\n    (object\n      (\"foo\" string)\n      (\"bar\" number)))  \n";
        let parsed = parse_valty_sexpr(input).expect("should parse with whitespace");
        let mut expected = IndexMap::new();
        expected.insert("foo".to_string(), ValTy::String);
        expected.insert("bar".to_string(), ValTy::Number);
        assert_eq!(parsed, ValTy::Array(Box::new(ValTy::Object(expected))));
    }

    #[test]
    fn string_escape_roundtrip() {
        let mut map = IndexMap::new();
        map.insert("line\nbreak".to_string(), ValTy::String);
        let ty = ValTy::Object(map);
        let text = format_valty_compact(&ty);
        assert_eq!(text, "(object (\"line\\nbreak\" string))");
        let reparsed = parse_valty_sexpr(&text).expect("escaped string should parse");
        assert_eq!(ty, reparsed);
    }

    #[test]
    fn empty_key_roundtrip() {
        let mut map = IndexMap::new();
        map.insert(String::new(), ValTy::Null);
        let ty = ValTy::Object(map);
        let text = format_valty_compact(&ty);
        assert_eq!(text, "(object (\"\" null))");
        let reparsed = parse_valty_sexpr(&text).expect("empty key should parse");
        assert_eq!(ty, reparsed);
    }

    #[test]
    fn duplicate_key_error() {
        let input = "(object (\"key\" string) (\"key\" number))";
        let error = parse_valty_sexpr(input).unwrap_err();
        let msg = match error {
            ValTyParseError::Invalid(msg) => msg,
        };
        assert!(msg.contains("duplicate object key"));
    }
}
