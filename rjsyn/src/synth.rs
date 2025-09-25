use crate::dsl::{consistent, Expr, Program};
use crate::json_example::JsonExample;
use crate::types::{infer_vt_examples, Ty, ValTy};
use serde_json::Value;
use std::collections::VecDeque;
use std::time::{Duration, Instant};

#[derive(Debug, Clone, PartialEq)]
pub enum SynthResult {
    Program(Program),
    ProgramNotFound,
    Timeout,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HExpr {
    Get(String),
    Equal(Box<HExpr>, Box<HExpr>),
    Not(Box<HExpr>),
    And(Box<HExpr>, Box<HExpr>),
    Or(Box<HExpr>, Box<HExpr>),
    Construct(Vec<(String, HExpr)>),
    Pipe(Box<HExpr>, Box<HExpr>, ValTy),
    Map(Box<HExpr>, ValTy),
    Concat(Box<HExpr>, Box<HExpr>),
    ToList,
    Flatten,
    Hole(Ty),
}

type Context = ValTy;

pub fn run_synth(time_limit: Duration, examples: &[JsonExample]) -> SynthResult {
    let deadline = Instant::now() + time_limit;
    let (input_ty, output_ty) = infer_vt_examples(examples);
    let mut queue: VecDeque<HExpr> = VecDeque::from(inductive_gen(&input_ty, &output_ty));
    let mut visited: Vec<HExpr> = Vec::new();

    while let Some(candidate) = queue.pop_front() {
        if Instant::now() > deadline {
            return SynthResult::Timeout;
        }

        if visited.iter().any(|v| v == &candidate) {
            continue;
        }
        visited.push(candidate.clone());

        if is_closed(&candidate) {
            if let Some(expr) = h_expr_to_expr(&candidate) {
                if consistent(examples, &expr) {
                    return SynthResult::Program(Program { body: expr });
                }
            }
            continue;
        }

        for expanded in expand(&input_ty, &candidate) {
            if !visited.iter().any(|v| v == &expanded) && !queue.iter().any(|q| q == &expanded) {
                queue.push_back(expanded);
            }
        }
    }

    SynthResult::ProgramNotFound
}

fn is_closed(expr: &HExpr) -> bool {
    match expr {
        HExpr::Get(_) | HExpr::ToList | HExpr::Flatten => true,
        HExpr::Hole(_) => false,
        HExpr::Not(inner) | HExpr::Map(inner, _) => is_closed(inner),
        HExpr::Equal(lhs, rhs)
        | HExpr::And(lhs, rhs)
        | HExpr::Or(lhs, rhs)
        | HExpr::Concat(lhs, rhs)
        | HExpr::Pipe(lhs, rhs, _) => is_closed(lhs) && is_closed(rhs),
        HExpr::Construct(fields) => fields.iter().all(|(_, v)| is_closed(v)),
    }
}

fn h_expr_to_expr(expr: &HExpr) -> Option<Expr> {
    match expr {
        HExpr::Get(key) => Some(Expr::Get(Box::new(Expr::Const(Value::String(key.clone()))))),
        HExpr::Equal(lhs, rhs) => Some(Expr::Equal(
            Box::new(h_expr_to_expr(lhs)?),
            Box::new(h_expr_to_expr(rhs)?),
        )),
        HExpr::Not(inner) => Some(Expr::Not(Box::new(h_expr_to_expr(inner)?))),
        HExpr::And(lhs, rhs) => Some(Expr::And(
            Box::new(h_expr_to_expr(lhs)?),
            Box::new(h_expr_to_expr(rhs)?),
        )),
        HExpr::Or(lhs, rhs) => Some(Expr::Or(
            Box::new(h_expr_to_expr(lhs)?),
            Box::new(h_expr_to_expr(rhs)?),
        )),
        HExpr::Construct(fields) => {
            let mut converted = Vec::with_capacity(fields.len());
            for (key, value) in fields.iter() {
                let key_expr = Expr::Const(Value::String(key.clone()));
                let value_expr = h_expr_to_expr(value)?;
                converted.push((key_expr, value_expr));
            }
            Some(Expr::Construct(converted))
        }
        HExpr::Pipe(lhs, rhs, _) => Some(Expr::Pipe(
            Box::new(h_expr_to_expr(lhs)?),
            Box::new(h_expr_to_expr(rhs)?),
        )),
        HExpr::Map(inner, _) => Some(Expr::Map(Box::new(h_expr_to_expr(inner)?))),
        HExpr::Concat(lhs, rhs) => Some(Expr::Concat(
            Box::new(h_expr_to_expr(lhs)?),
            Box::new(h_expr_to_expr(rhs)?),
        )),
        HExpr::ToList => Some(Expr::ToList),
        HExpr::Flatten => Some(Expr::Flatten),
        HExpr::Hole(_) => None,
    }
}

fn expand(ctx: &Context, expr: &HExpr) -> Vec<HExpr> {
    match expr {
        HExpr::Construct(fields) => expand_construct(ctx, fields),
        HExpr::Pipe(lhs, rhs, intermediate) => expand_pipe(ctx, lhs, rhs, intermediate),
        HExpr::Map(inner, elem_ty) => expand_map(ctx, inner, elem_ty),
        HExpr::Concat(lhs, rhs) => expand_binary(ctx, lhs, rhs, |l, r| {
            HExpr::Concat(Box::new(l), Box::new(r))
        }),
        HExpr::Equal(lhs, rhs) => {
            expand_binary(ctx, lhs, rhs, |l, r| HExpr::Equal(Box::new(l), Box::new(r)))
        }
        HExpr::Not(inner) => expand_unary(ctx, inner, |e| HExpr::Not(Box::new(e))),
        HExpr::And(lhs, rhs) => {
            expand_binary(ctx, lhs, rhs, |l, r| HExpr::And(Box::new(l), Box::new(r)))
        }
        HExpr::Or(lhs, rhs) => {
            expand_binary(ctx, lhs, rhs, |l, r| HExpr::Or(Box::new(l), Box::new(r)))
        }
        HExpr::Hole(Ty::Val(target)) => inductive_gen(ctx, target),
        HExpr::Hole(_) => Vec::new(),
        HExpr::Get(_) | HExpr::ToList | HExpr::Flatten => vec![expr.clone()],
    }
}

fn expand_construct(ctx: &Context, fields: &[(String, HExpr)]) -> Vec<HExpr> {
    let mut has_top_hole = false;
    let mut choices: Vec<Vec<HExpr>> = Vec::with_capacity(fields.len());
    for (_, value) in fields.iter() {
        match value {
            HExpr::Hole(Ty::Val(target)) => {
                has_top_hole = true;
                choices.push(inductive_gen(ctx, target));
            }
            other => choices.push(vec![other.clone()]),
        }
    }

    if has_top_hole {
        let mut results = Vec::new();
        for combo in cartesian_product(&choices) {
            let zipped = fields
                .iter()
                .map(|(k, _)| k.clone())
                .zip(combo.into_iter())
                .collect::<Vec<_>>();
            results.push(HExpr::Construct(zipped));
        }
        results
    } else {
        let mut results = Vec::new();
        for (idx, (key, value)) in fields.iter().enumerate() {
            let expansions = expand(ctx, value);
            for expanded in expansions {
                let mut new_fields = fields.to_vec();
                new_fields[idx] = (key.clone(), expanded);
                results.push(HExpr::Construct(new_fields));
            }
        }
        if results.is_empty() {
            vec![HExpr::Construct(fields.to_vec())]
        } else {
            results
        }
    }
}

fn expand_pipe(ctx: &Context, lhs: &HExpr, rhs: &HExpr, intermediate: &ValTy) -> Vec<HExpr> {
    match (lhs, rhs) {
        (HExpr::Hole(Ty::Val(left_ty)), HExpr::Hole(Ty::Arrow(a, b))) => {
            if let (Ty::Val(a_val), Ty::Val(b_val)) = (&**a, &**b) {
                let mut results = Vec::new();
                for l in inductive_gen(ctx, left_ty) {
                    for r in inductive_gen(a_val, b_val) {
                        results.push(HExpr::Pipe(
                            Box::new(l.clone()),
                            Box::new(r.clone()),
                            a_val.clone(),
                        ));
                    }
                }
                results
            } else {
                Vec::new()
            }
        }
        _ => {
            let mut results = Vec::new();
            for expanded_lhs in expand(ctx, lhs) {
                results.push(HExpr::Pipe(
                    Box::new(expanded_lhs),
                    Box::new(rhs.clone()),
                    intermediate.clone(),
                ));
            }
            for expanded_rhs in expand(intermediate, rhs) {
                results.push(HExpr::Pipe(
                    Box::new(lhs.clone()),
                    Box::new(expanded_rhs),
                    intermediate.clone(),
                ));
            }
            if results.is_empty() {
                vec![HExpr::Pipe(
                    Box::new(lhs.clone()),
                    Box::new(rhs.clone()),
                    intermediate.clone(),
                )]
            } else {
                results
            }
        }
    }
}

fn expand_map(_ctx: &Context, inner: &HExpr, elem_ty: &ValTy) -> Vec<HExpr> {
    match inner {
        HExpr::Hole(Ty::Arrow(a, b)) => {
            if let (Ty::Val(a_val), Ty::Val(b_val)) = (&**a, &**b) {
                inductive_gen(a_val, b_val)
                    .into_iter()
                    .map(|e| HExpr::Map(Box::new(e), (*elem_ty).clone()))
                    .collect()
            } else {
                Vec::new()
            }
        }
        _ => expand(elem_ty, inner)
            .into_iter()
            .map(|e| HExpr::Map(Box::new(e), (*elem_ty).clone()))
            .collect(),
    }
}

fn expand_binary<F>(ctx: &Context, lhs: &HExpr, rhs: &HExpr, builder: F) -> Vec<HExpr>
where
    F: Fn(HExpr, HExpr) -> HExpr,
{
    match (lhs, rhs) {
        (HExpr::Hole(Ty::Val(left_ty)), HExpr::Hole(Ty::Val(right_ty))) => {
            let mut results = Vec::new();
            for l in inductive_gen(ctx, left_ty) {
                for r in inductive_gen(ctx, right_ty) {
                    results.push(builder(l.clone(), r.clone()));
                }
            }
            results
        }
        _ => {
            let mut results = Vec::new();
            for expanded_lhs in expand(ctx, lhs) {
                results.push(builder(expanded_lhs, rhs.clone()));
            }
            for expanded_rhs in expand(ctx, rhs) {
                results.push(builder(lhs.clone(), expanded_rhs));
            }
            if results.is_empty() {
                vec![builder(lhs.clone(), rhs.clone())]
            } else {
                results
            }
        }
    }
}

fn expand_unary<F>(ctx: &Context, inner: &HExpr, builder: F) -> Vec<HExpr>
where
    F: Fn(HExpr) -> HExpr,
{
    match inner {
        HExpr::Hole(Ty::Val(target)) => inductive_gen(ctx, target)
            .into_iter()
            .map(builder)
            .collect(),
        _ => {
            let expanded = expand(ctx, inner);
            if expanded.is_empty() {
                vec![builder(inner.clone())]
            } else {
                expanded.into_iter().map(builder).collect()
            }
        }
    }
}

fn inductive_gen(ctx: &Context, target: &ValTy) -> Vec<HExpr> {
    let mut result = Vec::new();

    // get
    if let ValTy::Object(map) = ctx {
        for (key, ty) in map.iter() {
            if ty == target {
                result.push(HExpr::Get(key.clone()));
            }
        }
    }

    // construct
    if let ValTy::Object(fields) = target {
        let items = fields
            .iter()
            .map(|(k, ty)| (k.clone(), HExpr::Hole(Ty::Val((*ty).clone()))))
            .collect::<Vec<_>>();
        result.push(HExpr::Construct(items));
    }

    // pipe
    if let ValTy::Object(map) = ctx {
        let mut seen = Vec::new();
        for ty in map.values() {
            if !seen.iter().any(|existing: &ValTy| existing == ty) {
                seen.push((*ty).clone());
                result.push(HExpr::Pipe(
                    Box::new(HExpr::Hole(Ty::Val((*ty).clone()))),
                    Box::new(HExpr::Hole(Ty::Arrow(
                        Box::new(Ty::Val((*ty).clone())),
                        Box::new(Ty::Val((*target).clone())),
                    ))),
                    (*ty).clone(),
                ));
            }
        }
    }

    // map
    if let (ValTy::Array(elem_ty), ValTy::Array(res_ty)) = (ctx, target) {
        result.push(HExpr::Map(
            Box::new(HExpr::Hole(Ty::Arrow(
                Box::new(Ty::Val((**elem_ty).clone())),
                Box::new(Ty::Val((**res_ty).clone())),
            ))),
            (**elem_ty).clone(),
        ));
    }

    // concat
    if let ValTy::Array(inner) = target {
        result.push(HExpr::Concat(
            Box::new(HExpr::Hole(Ty::Val(ValTy::Array(inner.clone())))),
            Box::new(HExpr::Hole(Ty::Val(ValTy::Array(inner.clone())))),
        ));
    }

    // toList
    if let ValTy::Array(inner) = target {
        if ctx == &**inner {
            result.push(HExpr::ToList);
        }
        result.push(HExpr::Pipe(
            Box::new(HExpr::Hole(Ty::Val((**inner).clone()))),
            Box::new(HExpr::ToList),
            (**inner).clone(),
        ));
    }

    // flatten
    if let ValTy::Array(_inner) = target {
        if let ValTy::Array(ctx_inner) = ctx {
            if &**ctx_inner == target {
                result.push(HExpr::Flatten);
            }
        }
        let outer = ValTy::Array(Box::new((*target).clone()));
        result.push(HExpr::Pipe(
            Box::new(HExpr::Hole(Ty::Val(outer.clone()))),
            Box::new(HExpr::Flatten),
            outer,
        ));
    }

    // equal, not, and, or (only for bool)
    if matches!(target, ValTy::Bool) {
        if let ValTy::Object(map) = ctx {
            let mut seen = Vec::new();
            for ty in map.values() {
                if !seen.iter().any(|existing: &ValTy| existing == ty) {
                    seen.push((*ty).clone());
                    result.push(HExpr::Equal(
                        Box::new(HExpr::Hole(Ty::Val((*ty).clone()))),
                        Box::new(HExpr::Hole(Ty::Val((*ty).clone()))),
                    ));
                }
            }
        }
        result.push(HExpr::Not(Box::new(HExpr::Hole(Ty::Val(ValTy::Bool)))));
        result.push(HExpr::And(
            Box::new(HExpr::Hole(Ty::Val(ValTy::Bool))),
            Box::new(HExpr::Hole(Ty::Val(ValTy::Bool))),
        ));
        result.push(HExpr::Or(
            Box::new(HExpr::Hole(Ty::Val(ValTy::Bool))),
            Box::new(HExpr::Hole(Ty::Val(ValTy::Bool))),
        ));
    }

    result
}

fn cartesian_product(choices: &[Vec<HExpr>]) -> Vec<Vec<HExpr>> {
    if choices.is_empty() {
        return vec![Vec::new()];
    }
    let mut result = vec![Vec::new()];
    for group in choices.iter() {
        let mut next = Vec::new();
        for prefix in result.iter() {
            for item in group.iter() {
                let mut combined = prefix.clone();
                combined.push(item.clone());
                next.push(combined);
            }
        }
        result = next;
    }
    result
}
