use std::env;
use std::fs;
use std::process;
use std::time::Duration;

use anyhow::{anyhow, Context};

use rjsyn::{read_json_examples, run_synth, SynthResult};

fn main() -> anyhow::Result<()> {
    let mut args = env::args();
    let _binary = args.next();
    let command = match args.next() {
        Some(cmd) => cmd,
        None => {
            print_usage();
            process::exit(1);
        }
    };

    match command.as_str() {
        "synth" => synth_command(args.collect()),
        "infer" => infer_command(args.collect()),
        _ => {
            eprintln!("unknown command: {}", command);
            print_usage();
            process::exit(1);
        }
    }
}

fn synth_command(args: Vec<String>) -> anyhow::Result<()> {
    let mut lang: Option<String> = None;
    let mut positionals = Vec::new();
    let mut iter = args.into_iter();
    while let Some(arg) = iter.next() {
        match arg.as_str() {
            "--lang" => {
                let value = iter
                    .next()
                    .ok_or_else(|| anyhow!("--lang expects a value"))?;
                lang = Some(value);
            }
            _ if arg.starts_with("--lang=") => {
                let value = arg
                    .split_once('=')
                    .map(|(_, v)| v.to_owned())
                    .unwrap_or_default();
                if value.is_empty() {
                    return Err(anyhow!("--lang expects a value"));
                }
                lang = Some(value);
            }
            "--" => {
                positionals.extend(iter);
                break;
            }
            _ if arg.starts_with('-') => {
                return Err(anyhow!("unknown option: {}", arg));
            }
            _ => positionals.push(arg),
        }
    }

    if positionals.len() != 1 {
        return Err(anyhow!(
            "synth usage: rjsyn synth [--lang <language>] <examples file>"
        ));
    }
    let file = &positionals[0];
    let examples =
        read_json_examples(file).with_context(|| format!("reading examples from {}", file))?;
    match run_synth(Duration::from_micros(2_000_000), &examples) {
        SynthResult::Program(program) => match lang.as_deref() {
            Some("javascript") | Some("js") => println!("{}", program.to_js()),
            Some(other) => {
                return Err(anyhow!(
                    "unsupported language '{}'. try 'javascript'",
                    other
                ));
            }
            None => println!("{:#?}", program),
        },
        SynthResult::ProgramNotFound => {
            println!("Exhausted all possibilities and didn't find a valid program")
        }
        SynthResult::Timeout => println!("synthesis ran out of time"),
    }

    Ok(())
}

fn infer_command(args: Vec<String>) -> anyhow::Result<()> {
    if args.len() != 1 {
        return Err(anyhow!("infer usage: rjsyn infer <json file>"));
    }

    let file = &args[0];
    let content =
        fs::read_to_string(file).with_context(|| format!("reading json from {}", file))?;
    let value: serde_json::Value = serde_json::from_str(&content)
        .with_context(|| format!("parsing json value from {}", file))?;
    let inferred = rjsyn::types::infer_value_type(&value);
    println!("{}", inferred);
    Ok(())
}

fn print_usage() {
    eprintln!("usage: rjsyn <command> [options]\n");
    eprintln!("Commands:");
    eprintln!("  synth [--lang <language>] <examples file>   Synthesize a program from examples");
    eprintln!("  infer <json file>                           Infer the type of a JSON value");
}
