use clap::Parser;

#[derive(Debug, Parser, Clone)]
#[command(
    name = "pauc",
    bin_name = "pauc",
    about = "The Pau Language Compiler",
    version,
)]
pub struct Cli {
    pub input: String,

    #[arg(short, long)]
    pub output: Option<String>,

    #[arg(short, long)]
    pub long_time_output: bool,

    #[arg(short, long)]
    pub keep_object_file: bool,

    #[arg(short = 't', long)]
    pub emit_tokens: bool,
    #[arg(short = 'p', long)]
    pub emit_parse_tree: bool,
    #[arg(short = 'i', long)]
    pub emit_ir: bool,
}

impl Cli {
    pub fn command() -> Self {
        Self::parse()
    }
}