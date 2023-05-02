use crate::cli::Cli;
use clap::Parser;
use std::process::ExitCode;

mod cli;
mod matcher;
mod source;

fn main() -> ExitCode {
    Cli::parse()
        .run()
        .map(|_| ExitCode::SUCCESS)
        .unwrap_or(ExitCode::FAILURE)
}
