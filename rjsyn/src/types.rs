use crate::json_example::JsonExample;
use indexmap::IndexMap;
use serde_json::Value;
use std::fmt;

pub mod sexpr;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Ty {
    Arrow(Box<Ty>, Box<Ty>),
    Val(ValTy),
}

impl Ty {
    pub fn tarrow(input: ValTy, output: ValTy) -> Self {
        Ty::Arrow(Box::new(Ty::Val(input)), Box::new(Ty::Val(output)))
    }

    pub fn to_sexpr(&self) -> String {
        match self {
            Ty::Arrow(lhs, rhs) => format!("(arrow {} {})", lhs.to_sexpr(), rhs.to_sexpr()),
            Ty::Val(val) => val.to_sexpr(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub enum ValTy {
    #[default]
    Value,
    Object(IndexMap<String, ValTy>),
    Array(Box<ValTy>),
    String,
    Number,
    Bool,
    Null,
}

impl ValTy {
    pub fn to_sexpr(&self) -> String {
        sexpr::format_valty_compact(self)
    }
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ty::Arrow(a, b) => write!(f, "{} -> {}", a, b),
            Ty::Val(v) => write!(f, "{}", v),
        }
    }
}

impl fmt::Display for ValTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ValTy::Value => write!(f, "Value"),
            ValTy::Object(fields) => {
                let mut first = true;
                write!(f, "{{")?;
                for (k, v) in fields.iter() {
                    if !first {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", k, v)?;
                    first = false;
                }
                write!(f, "}}")
            }
            ValTy::Array(item) => write!(f, "[{}]", item),
            ValTy::String => write!(f, "String"),
            ValTy::Number => write!(f, "Number"),
            ValTy::Bool => write!(f, "Bool"),
            ValTy::Null => write!(f, "Null"),
        }
    }
}

pub fn infer_vt_examples(examples: &[JsonExample]) -> (ValTy, ValTy) {
    let inputs = examples
        .iter()
        .map(|ex| infer_value_type(&ex.input))
        .collect::<Vec<_>>();
    let outputs = examples
        .iter()
        .map(|ex| infer_value_type(&ex.output))
        .collect::<Vec<_>>();
    (infer_array(&inputs), infer_array(&outputs))
}

fn infer_array(types: &[ValTy]) -> ValTy {
    match types.len() {
        0 => ValTy::Value,
        1 => types[0].clone(),
        _ => types
            .iter()
            .skip(1)
            .fold(types[0].clone(), |acc, ty| type_union(&acc, ty)),
    }
}

pub fn infer_value_type(value: &Value) -> ValTy {
    match value {
        Value::Object(map) => {
            let mut obj = IndexMap::new();
            for (k, v) in map.iter() {
                obj.insert(k.clone(), infer_value_type(v));
            }
            ValTy::Object(obj)
        }
        Value::Array(arr) => {
            let inferred = arr.iter().map(infer_value_type).collect::<Vec<_>>();
            ValTy::Array(Box::new(infer_array(&inferred)))
        }
        Value::String(_) => ValTy::String,
        Value::Number(_) => ValTy::Number,
        Value::Bool(_) => ValTy::Bool,
        Value::Null => ValTy::Null,
    }
}

pub fn type_union(a: &ValTy, b: &ValTy) -> ValTy {
    match (a, b) {
        (ValTy::Object(lhs), ValTy::Object(rhs)) => {
            let mut map = IndexMap::new();
            for (key, left_ty) in lhs.iter() {
                if let Some(right_ty) = rhs.get(key) {
                    map.insert(key.clone(), type_union(left_ty, right_ty));
                }
            }
            ValTy::Object(map)
        }
        (ValTy::Array(lhs), ValTy::Array(rhs)) => ValTy::Array(Box::new(type_union(lhs, rhs))),
        (ValTy::String, ValTy::String) => ValTy::String,
        (ValTy::Number, ValTy::Number) => ValTy::Number,
        (ValTy::Bool, ValTy::Bool) => ValTy::Bool,
        (ValTy::Null, ValTy::Null) => ValTy::Null,
        _ => ValTy::Value,
    }
}
