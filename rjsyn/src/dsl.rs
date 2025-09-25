use crate::json_example::JsonExample;
use serde_json::Value;
use std::fmt;
use thiserror::Error;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Const(Value),
    Id,
    Keys,
    Elements,
    Flatten,
    ToList,
    Get(Box<Expr>),
    Map(Box<Expr>),
    Equal(Box<Expr>, Box<Expr>),
    Not(Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Construct(Vec<(Expr, Expr)>),
    Union(Box<Expr>, Box<Expr>),
    Pipe(Box<Expr>, Box<Expr>),
    Concat(Box<Expr>, Box<Expr>),
}

impl Expr {
    pub fn constant_string<S: Into<String>>(s: S) -> Self {
        Expr::Const(Value::String(s.into()))
    }
}

#[derive(Debug, Error, Clone, PartialEq)]
pub enum EvalError {
    #[error("expected {expected} in {ctx}, got {value}")]
    ExpectedType {expected: String, ctx: String, value: String},
    #[error("missing key '{key}' in object {object}")]
    MissingKey { key: String, object: String },
}

impl EvalError {
    fn expected_bool(ctx: &str, value: &Value) -> Self {
        EvalError::ExpectedType {
            expected: "boolean".to_string(),
            ctx: ctx.to_owned(),
            value: value.to_string(),
        }
    }
    fn expected_array(ctx: &str, value: &Value) -> Self {
        EvalError::ExpectedType {
            expected: "string".to_string(),
            ctx: ctx.to_owned(),
            value: value.to_string(),
        }
    }
    fn expected_object(ctx: &str, value: &Value) -> Self {
        EvalError::ExpectedType {
            expected: "object".to_string(),
            ctx: ctx.to_owned(),
            value: value.to_string(),
        }
    }
    fn expected_string(ctx: &str, value: &Value) -> Self {
        EvalError::ExpectedType {
            expected: "string".to_string(),
            ctx: ctx.to_owned(),
            value: value.to_string(),
        }
    }
}

pub fn eval(expr: &Expr, value: &Value) -> Result<Value, EvalError> {
    match expr {
        Expr::Const(v) => Ok(v.clone()),
        Expr::Id => Ok(value.clone()),
        Expr::Get(key_expr) => get(value, key_expr),
        Expr::Equal(lhs, rhs) => {
            let left = eval(lhs, value)?;
            let right = eval(rhs, value)?;
            Ok(Value::Bool(left == right))
        }
        Expr::Not(inner) => {
            let v = eval(inner, value)?;
            Ok(Value::Bool(!expect_bool("not", &v)?))
        }
        Expr::And(lhs, rhs) => {
            let lv = eval(lhs, value)?;
            let rv = eval(rhs, value)?;
            Ok(Value::Bool(
                expect_bool("&& lhs", &lv)? && expect_bool("&& rhs", &rv)?,
            ))
        }
        Expr::Or(lhs, rhs) => {
            let lv = eval(lhs, value)?;
            let rv = eval(rhs, value)?;
            Ok(Value::Bool(
                expect_bool("|| lhs", &lv)? || expect_bool("|| rhs", &rv)?,
            ))
        }
        Expr::Construct(fields) => construct(value, fields),
        Expr::Pipe(lhs, rhs) => {
            let intermediate = eval(lhs, value)?;
            eval(rhs, &intermediate)
        }
        Expr::Keys => keys(value),
        Expr::Elements => elements(value),
        Expr::Union(lhs, rhs) => union(value, lhs, rhs),
        Expr::Map(inner) => map_array(value, inner),
        Expr::Concat(lhs, rhs) => concat_arrays(value, lhs, rhs),
        Expr::ToList => Ok(Value::Array(vec![value.clone()])),
        Expr::Flatten => flatten(value),
    }
}

fn expect_bool(ctx: &str, value: &Value) -> Result<bool, EvalError> {
    match value {
        Value::Bool(b) => Ok(*b),
        other => Err(EvalError::expected_bool(ctx, other)),
    }
}

fn expect_array<'a>(ctx: &str, value: &'a Value) -> Result<&'a Vec<Value>, EvalError> {
    match value {
        Value::Array(arr) => Ok(arr),
        other => Err(EvalError::expected_array(ctx, other)),
    }
}

fn expect_object<'a>(
    ctx: &str,
    value: &'a Value,
) -> Result<&'a serde_json::Map<String, Value>, EvalError> {
    match value {
        Value::Object(obj) => Ok(obj),
        other => Err(EvalError::expected_object(ctx, other)),
    }
}

fn expect_string<'a>(ctx: &str, value: &'a Value) -> Result<&'a str, EvalError> {
    match value {
        Value::String(s) => Ok(s),
        other => Err(EvalError::expected_string(ctx, other)),
    }
}

fn keys(value: &Value) -> Result<Value, EvalError> {
    let obj = expect_object("keys", value)?;
    Ok(Value::Array(
        obj.keys()
            .map(|k| Value::String(k.clone()))
            .collect::<Vec<_>>(),
    ))
}

fn elements(value: &Value) -> Result<Value, EvalError> {
    let obj = expect_object("elements", value)?;
    Ok(Value::Array(obj.values().cloned().collect::<Vec<_>>()))
}

fn get(value: &Value, key_expr: &Expr) -> Result<Value, EvalError> {
    let key_val = eval(key_expr, value)?;
    let key = expect_string("get", &key_val)?;
    let obj = expect_object("get", value)?;
    match obj.get(key) {
        Some(res) => Ok(res.clone()),
        None => Err(EvalError::MissingKey {
            key: key.to_owned(),
            object: value.to_string(),
        }),
    }
}

fn construct(value: &Value, fields: &[(Expr, Expr)]) -> Result<Value, EvalError> {
    let mut map = serde_json::Map::new();
    for (key_expr, val_expr) in fields.iter() {
        let key_val = eval(key_expr, value)?;
        let key = expect_string("construct key", &key_val)?;
        let val = eval(val_expr, value)?;
        map.insert(key.to_owned(), val);
    }
    Ok(Value::Object(map))
}

fn union(value: &Value, lhs: &Expr, rhs: &Expr) -> Result<Value, EvalError> {
    let left = eval(lhs, value)?;
    let right = eval(rhs, value)?;
    let left_obj = expect_object("union lhs", &left)?;
    let right_obj = expect_object("union rhs", &right)?;
    let mut combined = left_obj.clone();
    for (k, v) in right_obj.iter() {
        combined.insert(k.clone(), v.clone());
    }
    Ok(Value::Object(combined))
}

fn map_array(value: &Value, inner: &Expr) -> Result<Value, EvalError> {
    let arr = expect_array("map", value)?;
    let mut result = Vec::with_capacity(arr.len());
    for item in arr.iter() {
        result.push(eval(inner, item)?);
    }
    Ok(Value::Array(result))
}

fn concat_arrays(value: &Value, lhs: &Expr, rhs: &Expr) -> Result<Value, EvalError> {
    let left = eval(lhs, value)?;
    let right = eval(rhs, value)?;
    let left_arr = expect_array("concat lhs", &left)?;
    let right_arr = expect_array("concat rhs", &right)?;
    let mut combined = Vec::with_capacity(left_arr.len() + right_arr.len());
    combined.extend(left_arr.iter().cloned());
    combined.extend(right_arr.iter().cloned());
    Ok(Value::Array(combined))
}

fn flatten(value: &Value) -> Result<Value, EvalError> {
    let outer = expect_array("flatten", value)?;
    let mut flattened = Vec::new();
    for item in outer.iter() {
        let inner = expect_array("flatten element", item)?;
        flattened.extend(inner.iter().cloned());
    }
    Ok(Value::Array(flattened))
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub body: Expr,
}

impl Program {
    pub fn to_js(&self) -> String {
        let inline = to_js_inline("x", &self.body);
        format!("function program(x) {{\n    return {};\n}}", inline)
    }
}

fn to_js_inline(current: &str, expr: &Expr) -> String {
    match expr {
        Expr::Const(v) => serde_json::to_string(v).unwrap_or_else(|_| "null".into()),
        Expr::Id => current.to_owned(),
        Expr::Keys => format!("Object.keys({})", current),
        Expr::Elements => format!("Object.values({})", current),
        Expr::Get(key_expr) => {
            if let Some(raw) = as_const_string(key_expr) {
                if is_valid_key(raw) {
                    return format!("{}.{}", current, raw);
                }
            }
            let key = to_js_inline(current, key_expr);
            format!("{}[{}]", current, key)
        }
        Expr::Equal(lhs, rhs) => format!(
            "{} == {}",
            to_js_inline(current, lhs),
            to_js_inline(current, rhs)
        ),
        Expr::Not(inner) => format!("!{}", to_js_inline(current, inner)),
        Expr::And(lhs, rhs) => format!(
            "{} && {}",
            to_js_inline(current, lhs),
            to_js_inline(current, rhs)
        ),
        Expr::Or(lhs, rhs) => format!(
            "{} || {}",
            to_js_inline(current, lhs),
            to_js_inline(current, rhs)
        ),
        Expr::Construct(fields) => {
            let pieces = fields
                .iter()
                .map(|(k, v)| {
                    let key_inline = to_js_inline(current, k);
                    let value_inline = to_js_inline(current, v);
                    if let Some(raw) = as_const_string(k) {
                        if is_valid_key(raw) {
                            return format!("{}:{}", key_inline, value_inline);
                        }
                    }
                    format!("[{}]:{}", key_inline, value_inline)
                })
                .collect::<Vec<_>>()
                .join(", ");
            format!("{{{}}}", pieces)
        }
        Expr::Union(lhs, rhs) => format!(
            "Object.assign({}, {})",
            to_js_inline(current, lhs),
            to_js_inline(current, rhs)
        ),
        Expr::Pipe(lhs, rhs) => {
            let intermediate = to_js_inline(current, lhs);
            to_js_inline(&intermediate, rhs)
        }
        Expr::Map(inner) => {
            let body = to_js_inline("x", inner);
            format!("{}.map(x => {{ return {}; }})", current, body)
        }
        Expr::Concat(lhs, rhs) => {
            format!(
                "{} + {}",
                to_js_inline(current, lhs),
                to_js_inline(current, rhs)
            )
        }
        Expr::ToList => format!("[{}]", current),
        Expr::Flatten => format!("({}).flat()", current),
    }
}

fn as_const_string(expr: &Expr) -> Option<&str> {
    match expr {
        Expr::Const(Value::String(s)) => Some(s.as_str()),
        _ => None,
    }
}

fn is_valid_key(s: &str) -> bool {
    !s.chars().any(|ch| ch == ' ' || ch == '\"')
}

pub fn consistent(examples: &[JsonExample], expr: &Expr) -> bool {
    examples.iter().all(|ex| match eval(expr, &ex.input) {
        Ok(result) => result == ex.output,
        Err(_) => false,
    })
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_js())
    }
}
