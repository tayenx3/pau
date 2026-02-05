use std::str::FromStr;

use clap::Parser;

#[derive(Debug, Parser, Clone, PartialEq)]
pub enum EmitStage {
    Lex, Ast,
}

impl FromStr for EmitStage {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "lex" | "tokens" => Ok(Self::Lex),
            "ast" | "parse-tree" => Ok(Self::Ast),
            _ => Err(format!("Unknown emit stage: `{s}`"))
        }
    }
}

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
    pub emit: Option<EmitStage>,
}

impl Cli {
    pub fn command() -> Self {
        Self::parse()
    }
}