use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::fs;
use std::path::Path;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct JsonExample {
    pub input: Value,
    pub output: Value,
}

pub fn decode_json_examples(bytes: &[u8]) -> Result<Vec<JsonExample>, serde_json::Error> {
    serde_json::from_slice(bytes)
}

pub fn read_json_examples<P: AsRef<Path>>(path: P) -> anyhow::Result<Vec<JsonExample>> {
    let data = fs::read(path)?;
    let examples = decode_json_examples(&data)?;
    Ok(examples)
}
