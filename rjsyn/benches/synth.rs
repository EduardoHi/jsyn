use std::path::Path;
use std::path::PathBuf;
use std::time::Duration;

use criterion::black_box;
use criterion::criterion_group;
use criterion::criterion_main;
use criterion::BenchmarkId;
use criterion::Criterion;
use rjsyn::read_json_examples;
use rjsyn::run_synth;
use rjsyn::JsonExample;
use rjsyn::SynthResult;

const TIME_LIMIT: Duration = Duration::from_secs(30);

struct Fixture {
    name: &'static str,
    file: &'static str,
}

const FIXTURES: &[Fixture] = &[
    Fixture {
        name: "test1",
        file: "test1.json",
    },
    Fixture {
        name: "test2",
        file: "test2.json",
    },
    Fixture {
        name: "test3",
        file: "test3.json",
    },
    Fixture {
        name: "test4",
        file: "test4.json",
    },
    Fixture {
        name: "test5",
        file: "test5.json",
    },
    Fixture {
        name: "test6",
        file: "test6.json",
    },
    Fixture {
        name: "test7",
        file: "test7.json",
    },
    Fixture {
        name: "test8",
        file: "test8.json",
    },
    Fixture {
        name: "test9",
        file: "test9.json",
    },
    Fixture {
        name: "test10",
        file: "test10.json",
    },
    Fixture {
        name: "test11",
        file: "test11.json",
    },
    Fixture {
        name: "test12",
        file: "test12.json",
    },
    Fixture {
        name: "test13",
        file: "test13.json",
    },
    // Matches the Haskell suite, where test14 and test16 currently timeout
    Fixture {
        name: "test15",
        file: "test15.json",
    },
];

fn load_examples(path: &Path) -> Vec<JsonExample> {
    read_json_examples(path)
        .unwrap_or_else(|err| panic!("failed to load {}: {}", path.display(), err))
}

fn tests_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../tests")
}

fn bench_synth(c: &mut Criterion) {
    let fixtures = FIXTURES
        .iter()
        .map(|fixture| {
            let path = tests_dir().join(fixture.file);
            let examples = load_examples(&path);
            (fixture.name.to_string(), examples)
        })
        .collect::<Vec<_>>();

    let mut group = c.benchmark_group("synth");
    group.measurement_time(Duration::from_secs(30));
    group.sample_size(10);

    for (name, examples) in fixtures.iter() {
        group.bench_with_input(BenchmarkId::from_parameter(name), examples, |b, exs| {
            b.iter(|| {
                let result = run_synth(TIME_LIMIT, exs.as_slice());
                if matches!(result, SynthResult::Timeout) {
                    panic!("benchmark {name} timed out under {:?}", TIME_LIMIT);
                }
                black_box(result);
            });
        });
    }

    group.finish();
}

criterion_group!(benches, bench_synth);
criterion_main!(benches);
