use std::fs;
use std::path::PathBuf;
use std::time::Duration;

use anyhow::Context;
use clap::{ArgAction, Parser, Subcommand, ValueEnum};

use rjsyn::{read_json_examples, run_synth, SynthResult};

#[derive(Parser, Debug)]
#[command(name = "rjsyn", about = "CLI front-end for the rjsyn synthesiser")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// Synthesise a program from JSON example pairs
    Synth {
        /// Emit program in a specific language instead of debug form
        #[arg(long, value_enum)]
        lang: Option<Language>,
        /// Path to JSON examples file
        file: PathBuf,
        /// Print progress information while searching
        #[arg(short = 'v', long, action = ArgAction::SetTrue)]
        verbose: bool,
    },
    /// Infer the abstract type of a JSON value
    Infer {
        /// Path to JSON file containing a single value
        file: PathBuf,
    },
}

#[derive(Copy, Clone, Debug, ValueEnum)]
enum Language {
    Javascript,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Synth {
            lang,
            file,
            verbose,
        } => synth_command(lang, file, verbose),
        Commands::Infer { file } => infer_command(file),
    }
}

fn synth_command(lang: Option<Language>, file: PathBuf, verbose: bool) -> anyhow::Result<()> {
    let file_display = file.display().to_string();
    let examples = read_json_examples(&file)
        .with_context(|| format!("reading examples from {}", file_display))?;

    let result = if verbose {
        rjsyn::run_synth_verbose(Duration::from_micros(2_000_000), &examples)
    } else {
        run_synth(Duration::from_micros(2_000_000), &examples)
    };

    match result {
        SynthResult::Program(program) => match lang {
            Some(Language::Javascript) => println!("{}", program.to_js()),
            None => println!("{:#?}", program),
        },
        SynthResult::ProgramNotFound => {
            println!("Exhausted all possibilities and didn't find a valid program")
        }
        SynthResult::Timeout => println!("synthesis ran out of time"),
    }

    Ok(())
}

fn infer_command(file: PathBuf) -> anyhow::Result<()> {
    let file_display = file.display().to_string();
    let content =
        fs::read_to_string(&file).with_context(|| format!("reading json from {}", file_display))?;
    let value: serde_json::Value = serde_json::from_str(&content)
        .with_context(|| format!("parsing json value from {}", file_display))?;
    let inferred = rjsyn::types::infer_value_type(&value);
    println!("{:#?}", inferred);
    Ok(())
}
