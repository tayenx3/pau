mod cli;
mod span;
mod diag;
mod operator;

mod lexer;
mod parser;
mod semantics;
mod pil;

use cli::{Cli, EmitStage};

fn main() {
    let cli = Cli::command();

    if let Err(err) = run_cli(cli) {
        eprintln!("{err}");
    }
}

fn run_cli(cli: Cli) -> Result<(), String> {
    use std::fs;
    use std::time::Instant;
    use colored::Colorize;

    let start_time = Instant::now();

    let path = cli.input;
    let source = fs::read_to_string(&path)
        .map_err(|err| format!("{}: {err}", "error".bright_red().bold()))?;

    let line_starts = line_starts(&source);
    let lines = source.lines().collect::<Vec<_>>();

    let tokens = lexer::tokenize(&path, &source)
        .map_err(|err| err.display(&line_starts, &lines))?;
    if let Some(EmitStage::Lex) = cli.emit {
        eprintln!("{}: TOKENS: {tokens:#?}", "debug".bright_cyan().bold());
    }

    let mut parse_tree = parser::parse(&path, &tokens)
        .map_err(|err| err.display(&line_starts, &lines))?;

    let comp_duration = start_time.elapsed().as_secs_f32();
    
    if let Some(EmitStage::Ast) = cli.emit {
        eprintln!("\n{}: AST: {parse_tree:#?}", "debug".bright_cyan().bold());
    }

    semantics::analyze(&path, &mut parse_tree)
        .map_err(
            |err|
            err.iter()
                .map(|d| d.display(&line_starts, &lines))
                .collect::<Vec<_>>()
                .join("\n")
        )?;

    println!("{} in {comp_duration:.2}s", "Compilation finished".bright_green().bold());

    Ok(())
}

fn line_starts(s: &str) -> Vec<usize> {
    let mut indices = vec![0];
    
    for (i, ch) in s.char_indices() {
        if ch == '\n' {
            indices.push(i + 1);
        }
    }

    indices
}