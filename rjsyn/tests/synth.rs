use std::time::Duration;

use rjsyn::{read_json_examples, run_synth, SynthResult};

#[test]
fn synthesizes_identity() {
    let examples = read_json_examples("../tests/test1.json").expect("read fixture");
    match run_synth(Duration::from_micros(2_000_000), &examples) {
        SynthResult::Program(program) => {
            insta::assert_snapshot!(program.to_js());
        }
        SynthResult::ProgramNotFound => panic!("expected program, got not found"),
        SynthResult::Timeout => panic!("expected program, got timeout"),
    }
}
