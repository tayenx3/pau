mod cli;
mod span;
mod diag;
mod operator;

mod lexer;
mod parser;
mod semantics;
mod middleend;

use cli::Cli;
use colored::Colorize;

fn main() {
    let cli = Cli::command();

    if let Err(err) = run_cli(cli) {
        eprintln!("{err}");
    }
}

fn run_cli(cli: Cli) -> Result<(), String> {
    use std::fs;
    use std::time::Instant;

    let start_time = Instant::now();

    let path = cli.input;
    let source = fs::read_to_string(&path)
        .map_err(|err| format!("{}: {err}", "error".bright_red().bold()))?;

    let line_starts = line_starts(&source);
    let lines = source.lines().collect::<Vec<_>>();

    let tokens = lexer::tokenize(&path, &source)
        .map_err(|err| err.display(&line_starts, &lines))?;
    if cli.emit_tokens {
        eprintln!("{}: TOKENS: {tokens:#?}", "debug".bright_cyan().bold());
    }

    let mut parse_tree = parser::parse(&path, &tokens)
        .map_err(|err| err.display(&line_starts, &lines))?;
    
    if cli.emit_parse_tree {
        eprintln!("{}: AST: {parse_tree:#?}", "debug".bright_cyan().bold());
    }

    semantics::analyze(&path, &mut parse_tree)
        .map_err(
            |err|
            err.iter()
                .map(|d| d.display(&line_starts, &lines))
                .collect::<Vec<_>>()
                .join("\n")
        )?;

    let obj = middleend::generate_obj(&path, &parse_tree, cli.emit_ir)?;

    use std::fs::File;
    use std::io::Write;
    use std::path::Path;

    let obj_path = cli.output.clone()
        .map(|path| format!("{}.o", Path::new(&path).file_stem().unwrap().display()))
        .unwrap_or(format!("{}.o", Path::new(&path).file_stem().unwrap().display()));
    let mut file = File::create(&obj_path)
        .map_err(|err| format!("{}: {err}", "error".bright_red().bold()))?;
    file.write_all(&obj)
        .map_err(|err| format!("{}: {err}", "error".bright_red().bold()))?;

    let output_exe = cli.output
        .unwrap_or(format!("{}.exe", Path::new(&path).file_stem().unwrap().display()));

    link(&obj_path, &output_exe, cli.verbose)?;

    if !cli.keep_object_file {
        fs::remove_file(obj_path)
            .map_err(|err| format!("{}: {err}", "error".bright_red().bold()))?;
    }

    let comp_duration = start_time.elapsed().as_secs_f32();

    if cli.verbose {
        println!("{} in {comp_duration}s", "Compilation finished".bright_green().bold());
    } else {
        println!("{} in {comp_duration:.2}s", "Compilation finished".bright_green().bold());
    }

    Ok(())
}

fn link(obj_path: &str, output_exe: &str, verbose: bool) -> Result<(), String> {
    use std::process::Command;

    let linker_options = &[
        ("lld-link", &[
            &format!("/out:{output_exe}"),
            "/subsystem:console",
            "/entry:main",
        ] as &[&str]),
        ("ld.lld", &[
            &format!("-o {output_exe}"),
            "--entry=main",
        ]),
        ("ld64.lld", &[
            &format!("-o {output_exe}"),
            "-e main",
        ]),
        ("link", &[
            &format!("/out:{output_exe}"),
            "/subsystem:console",
            "/entry:main",
        ]),
    ];

    let mut link_success = false;
    for (linker_name, linker_args) in linker_options {
        if let Ok(bin_path) = which::which(linker_name) {
            let output = Command::new(bin_path.display().to_string())
                .arg(obj_path)
                .args(*linker_args)
                .output()
                .map_err(|err| format!(
                    "{}: linker failed (used `{linker_name}`): {err}",
                    "error".bright_red().bold()
                ))?;
            
            // if linker succeeded, stop
            if output.status.success() {
                link_success = true;
                if verbose {
                    eprintln!("successfully linked with {}",
                        linker_name.cyan().bold());
                }
                break
            }
        }
    }

    if !link_success {
        return Err(format!("{}: no suitable linker found
{}: please try manually linking `{obj_path}`",
            "error".bright_red().bold(), "error".bright_red().bold()
        ));
    }

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