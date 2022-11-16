# typescript-to-gdscript

Convert TypeScript type definitions into concrete models in GDScript for godot

## Set up Rust

- Install rustup
  `curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh` or `rustup`
- Install `rust-analyzer` and `CodeLLDB` extensions in vscode.
- Restart VsCode

## Building and coding with Rust

`cargo run -- [arguments]` to run the program.

Add `#![allow(warnings)]` to the top of the file to ignore warnings while writing code
