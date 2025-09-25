use std::time::Duration;

use indexmap::IndexMap;
use rjsyn::dsl::{self, Expr};
use rjsyn::synth::SynthResult;
use rjsyn::types::{infer_value_type, ValTy};
use rjsyn::{read_json_examples, run_synth, JsonExample};

struct TestTask {
    name: &'static str,
    expr: fn() -> Expr,
    fixture: &'static str,
    expected_types: fn() -> Vec<(ValTy, ValTy)>,
}

fn cstring(s: &str) -> Expr {
    Expr::constant_string(s)
}

fn object_type(entries: Vec<(&str, ValTy)>) -> ValTy {
    let mut map = IndexMap::new();
    for (key, value) in entries {
        map.insert(key.to_string(), value);
    }
    ValTy::Object(map)
}

fn evaluation_tasks() -> Vec<TestTask> {
    vec![
        TestTask {
            name: "identity filter",
            expr: filter1,
            fixture: "../tests/test1.json",
            expected_types: types1,
        },
        TestTask {
            name: "swap every key for its value",
            expr: filter2,
            fixture: "../tests/test2.json",
            expected_types: types2,
        },
        TestTask {
            name: "nest an object inside a key from the field 'key'",
            expr: filter3,
            fixture: "../tests/test3.json",
            expected_types: types3,
        },
        TestTask {
            name: "denest an object (inverse operation of filter3)",
            expr: filter4,
            fixture: "../tests/test4.json",
            expected_types: types4,
        },
        TestTask {
            name: "get a single field from an object",
            expr: filter5,
            fixture: "../tests/test5.json",
            expected_types: types5,
        },
        TestTask {
            name: "get all but a single field from an object",
            expr: filter6,
            fixture: "../tests/test6.json",
            expected_types: types6,
        },
        TestTask {
            name: "get a value in a nested object",
            expr: filter7,
            fixture: "../tests/test7.json",
            expected_types: types7,
        },
    ]
}

fn assert_examples_eval(name: &str, expr: &Expr, examples: &[JsonExample]) {
    for (idx, example) in examples.iter().enumerate() {
        let result = dsl::eval(expr, &example.input)
            .unwrap_or_else(|err| panic!("{} example {} failed to eval: {}", name, idx + 1, err));
        assert_eq!(
            result,
            example.output,
            "{} example {} produced unexpected output",
            name,
            idx + 1
        );
    }
}

fn assert_examples_types(name: &str, examples: &[JsonExample], expected: &[(ValTy, ValTy)]) {
    assert_eq!(
        examples.len(),
        expected.len(),
        "{} expected {} type pairs but fixture has {} examples",
        name,
        expected.len(),
        examples.len()
    );

    for (idx, ((expected_in, expected_out), example)) in expected.iter().zip(examples).enumerate() {
        let inferred_in = infer_value_type(&example.input);
        assert_eq!(
            &inferred_in,
            expected_in,
            "{} example {} inferred unexpected input type",
            name,
            idx + 1
        );

        let inferred_out = infer_value_type(&example.output);
        assert_eq!(
            &inferred_out,
            expected_out,
            "{} example {} inferred unexpected output type",
            name,
            idx + 1
        );
    }
}

fn filter1() -> Expr {
    Expr::Id
}

fn filter2() -> Expr {
    let chicago = cstring("Chicago");
    let seattle = cstring("Seattle");
    Expr::Construct(vec![
        (Expr::Get(Box::new(chicago.clone())), chicago),
        (Expr::Get(Box::new(seattle.clone())), seattle),
    ])
}

fn filter3() -> Expr {
    let key = cstring("key");
    let host = cstring("host");
    let name = cstring("name");
    let inner = Expr::Construct(vec![
        (host.clone(), Expr::Get(Box::new(host))),
        (name.clone(), Expr::Get(Box::new(name.clone()))),
    ]);
    Expr::Construct(vec![(Expr::Get(Box::new(key)), inner)])
}

fn filter4() -> Expr {
    let key = cstring("key");
    Expr::Union(
        Box::new(Expr::Construct(vec![(key, Expr::Keys)])),
        Box::new(Expr::Elements),
    )
}

fn filter5() -> Expr {
    Expr::Get(Box::new(cstring("age")))
}

fn filter6() -> Expr {
    Expr::Construct(vec![
        (cstring("age"), Expr::Get(Box::new(cstring("age")))),
        (cstring("gpa"), Expr::Get(Box::new(cstring("gpa")))),
    ])
}

fn filter7() -> Expr {
    Expr::Pipe(
        Box::new(Expr::Get(Box::new(cstring("hostinfo")))),
        Box::new(Expr::Get(Box::new(cstring("host")))),
    )
}

fn base_person_type() -> ValTy {
    object_type(vec![("age", ValTy::Number), ("name", ValTy::String)])
}

fn host_info_object() -> ValTy {
    object_type(vec![("name", ValTy::String), ("host", ValTy::String)])
}

fn hostinfo_with_flag() -> ValTy {
    object_type(vec![
        (
            "hostinfo",
            object_type(vec![
                ("online", ValTy::Bool),
                ("name", ValTy::String),
                ("host", ValTy::String),
            ]),
        ),
        ("id", ValTy::Number),
    ])
}

fn types1() -> Vec<(ValTy, ValTy)> {
    let person = base_person_type();
    vec![(person.clone(), person)]
}

fn types2() -> Vec<(ValTy, ValTy)> {
    vec![(
        object_type(vec![("Chicago", ValTy::String), ("Seattle", ValTy::String)]),
        object_type(vec![
            ("Los Angeles", ValTy::String),
            ("New York", ValTy::String),
        ]),
    )]
}

fn types3() -> Vec<(ValTy, ValTy)> {
    let input = object_type(vec![
        ("key", ValTy::String),
        ("name", ValTy::String),
        ("host", ValTy::String),
    ]);
    let value = host_info_object();
    vec![
        (input.clone(), object_type(vec![("key1", value.clone())])),
        (input, object_type(vec![("key2", value)])),
    ]
}

fn types4() -> Vec<(ValTy, ValTy)> {
    let target = object_type(vec![
        ("key", ValTy::String),
        ("name", ValTy::String),
        ("host", ValTy::String),
    ]);
    let nested = host_info_object();
    vec![
        (object_type(vec![("key1", nested.clone())]), target.clone()),
        (object_type(vec![("key2", nested)]), target),
    ]
}

fn types5() -> Vec<(ValTy, ValTy)> {
    vec![(base_person_type(), ValTy::Number)]
}

fn types6() -> Vec<(ValTy, ValTy)> {
    let input = object_type(vec![
        ("age", ValTy::Number),
        ("name", ValTy::String),
        ("gpa", ValTy::Number),
    ]);
    let output = object_type(vec![("age", ValTy::Number), ("gpa", ValTy::Number)]);
    vec![(input, output)]
}

fn types7() -> Vec<(ValTy, ValTy)> {
    vec![(hostinfo_with_flag(), ValTy::String)]
}

#[test]
fn evaluation_and_inference_examples() {
    for task in evaluation_tasks() {
        let examples = read_json_examples(task.fixture)
            .unwrap_or_else(|err| panic!("{} failed to read fixture: {}", task.name, err));
        let expr = (task.expr)();
        assert_examples_eval(task.name, &expr, &examples);
        let expected = (task.expected_types)();
        assert_examples_types(task.name, &examples, &expected);
    }
}

fn run_synthesis_fixture(path: &str) {
    let examples = read_json_examples(path).expect("read fixture");
    let result = run_synth(Duration::from_micros(5_000_000), &examples);
    match result {
        SynthResult::Program(program) => {
            assert!(
                dsl::consistent(&examples, &program.body),
                "program not consistent with {}",
                path
            );
            insta::assert_snapshot!(program.to_js());
        }
        SynthResult::ProgramNotFound => panic!("expected program for {}", path),
        SynthResult::Timeout => panic!("timeout synthesizing {}", path),
    }
}

macro_rules! synth_snapshot_test {
    ($name:ident, $fixture:literal) => {
        #[test]
        fn $name() {
            run_synthesis_fixture($fixture);
        }
    };
}

synth_snapshot_test!(synthesizes_test1, "../tests/test1.json");
synth_snapshot_test!(synthesizes_test2, "../tests/test2.json");
synth_snapshot_test!(synthesizes_test3, "../tests/test3.json");
synth_snapshot_test!(synthesizes_test4, "../tests/test4.json");
synth_snapshot_test!(synthesizes_test5, "../tests/test5.json");
synth_snapshot_test!(synthesizes_test6, "../tests/test6.json");
synth_snapshot_test!(synthesizes_test7, "../tests/test7.json");
synth_snapshot_test!(synthesizes_test8, "../tests/test8.json");
synth_snapshot_test!(synthesizes_test9, "../tests/test9.json");
synth_snapshot_test!(synthesizes_test10, "../tests/test10.json");
synth_snapshot_test!(synthesizes_test11, "../tests/test11.json");
synth_snapshot_test!(synthesizes_test12, "../tests/test12.json");
synth_snapshot_test!(synthesizes_test13, "../tests/test13.json");
synth_snapshot_test!(synthesizes_test14, "../tests/test14.json");
synth_snapshot_test!(synthesizes_test15, "../tests/test15.json");
synth_snapshot_test!(synthesizes_test16, "../tests/test16.json");
synth_snapshot_test!(synthesizes_test17, "../tests/test17.json");
synth_snapshot_test!(synthesizes_test18, "../tests/test18.json");
synth_snapshot_test!(synthesizes_test20, "../tests/test20.json");
synth_snapshot_test!(synthesizes_test21, "../tests/test21.json");
synth_snapshot_test!(synthesizes_test22, "../tests/test22.json");
synth_snapshot_test!(synthesizes_test23, "../tests/test23.json");
synth_snapshot_test!(synthesizes_test24, "../tests/test24.json");
synth_snapshot_test!(synthesizes_test25, "../tests/test25.json");
