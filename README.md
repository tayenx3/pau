<div align="center">
    <img src="icon.png" width="400">
    <h1>Pau</h1>
    <p>A deliberately small, experimental language focused on clarity and simplicity.</p>
    <img src="https://img.shields.io/badge/Status-v0.1.0%20Pre--Alpha-616A6B" alt="Status: Pre-Alpha">
    <img src="https://img.shields.io/badge/License-MIT-blue" alt="License: MIT">
    <img src="https://img.shields.io/badge/Production%20Ready%3F-No-red" alt="Production Ready?: No">
</div>

```pau
def main()
    println("Hello, world!");
end
```

Pau is a statically-typed, experimental compiled programming language written in Rust that targets the Cranelift backend. Pau is **NOT** a systems language and is not intended for production use. It is deliberately small and built for experimentation.

## Installing
```bash
cargo install --git https://github.com/tayenx3/pau.git

# confirm installation
pauc --version

# get CLI guide
pauc --help
```
(visit https://rustup.rs if you don't have Rust/`cargo` yet)

## Where to learn Pau?
Check out [the Guide](docs/guide.md)

## Supported Targets
Pau currently supports the following targets via Cranelift:
| Arch            | OS      | Target         |
| --------------- | ------- | -------------- |
| x86-64          | Linux   | x86-64-linux   |
| x86-64          | Windows | x86-64-windows |
| x86-64          | macOS   | x86-64-darwin  |
| AArch64/ARM64   | Linux   | aarch64-linux  |
| AArch64/ARM64   | macOS   | aarch64-darwin |
| RISC-V (64-bit) | Linux   | riscv64-linux  |
| IBM Z           | Linux   | s390x-linux    |
| WebAssembly     | WASI    | wasm32-wasi    |

## Why the name?
The name "Pau" comes from the Latin word "paulus" which means "small" or "not large"

## Goals and Roadmap
Pau is experimental, but still aims to have:
- Simple syntax
- Clear error messages
- Lean compilation speeds

[More on the roadmap](ROADMAP.md)