use std::fs;
use std::path::PathBuf;
use std::time::Duration;

use anyhow::Context;
use clap::ArgAction;
use clap::Parser;
use clap::Subcommand;
use clap::ValueEnum;

use rjsyn::dsl::sexpr::parse_program_sexpr;
use rjsyn::read_json_examples;
use rjsyn::run_synth;
use rjsyn::types::sexpr::format_valty_pretty;
use rjsyn::SynthResult;

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
        /// if set, then infer will not assume it's `input/output` format.
        #[arg(short = 'f', long, action = ArgAction::SetTrue)]
        freeform: bool,
    },
    /// Evaluate a program written in the DSL S-expression form against a JSON value
    Eval {
        /// Path to the S-expression program
        program: PathBuf,
        /// Path to JSON file containing a single value
        json: PathBuf,
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
        Commands::Infer { file, freeform } => infer_command(file, freeform),
        Commands::Eval { program, json } => eval_command(program, json),
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

fn infer_command(file: PathBuf, freeform: bool) -> anyhow::Result<()> {
    let file_display = file.display().to_string();
    let content =
        fs::read_to_string(&file).with_context(|| format!("reading json from {}", file_display))?;
    if freeform {
        let value: serde_json::Value = serde_json::from_str(&content)
            .with_context(|| format!("parsing json value from {}", file_display))?;
        let inferred = rjsyn::types::infer_value_type(&value);
        let sexpr = format_valty_pretty(&inferred);
        println!("{}", sexpr);
    } else {
        let examples = read_json_examples(&file)
            .with_context(|| format!("reading examples from {}", file_display))?;
        let (in_inferred, out_inferred) = rjsyn::types::infer_vt_examples(&examples);

        println!(
            "INPUT\n{}\nOUTPUT\n{}\n",
            format_valty_pretty(&in_inferred),
            format_valty_pretty(&out_inferred)
        );
    }
    Ok(())
}

fn eval_command(program_path: PathBuf, json_path: PathBuf) -> anyhow::Result<()> {
    let program_display = program_path.display().to_string();
    let program_source = fs::read_to_string(&program_path)
        .with_context(|| format!("reading program from {}", program_display))?;
    let program = parse_program_sexpr(&program_source)
        .with_context(|| format!("parsing program from {}", program_display))?;

    let json_display = json_path.display().to_string();
    let json_source = fs::read_to_string(&json_path)
        .with_context(|| format!("reading json from {}", json_display))?;
    let json_value: serde_json::Value = serde_json::from_str(&json_source)
        .with_context(|| format!("parsing json value from {}", json_display))?;

    let result = rjsyn::dsl::eval(&program.body, &json_value)?;

    let rendered =
        serde_json::to_string_pretty(&result).context("serialising evaluation result")?;
    println!("{}", rendered);

    Ok(())
}
