#!/usr/bin/env rust-script
//! Dependencies can be specified in the script file itself as follows:
//!
//! ```cargo
//! [dependencies]
//! serde = { version = "1.0", features = ["derive"] }
//! serde_json = "1.0"
//! ```

use serde::Deserialize;
use serde_json::{Result, Value};
use std::fs::read_to_string;
use std::fs::File;
use std::io::BufReader;

fn main() {
    let x: u64 = random();
    println!("A random number: {}", x);
}

fn read_json_file(file_path: &str) -> Result {
    // Open the file in read-only mode with buffer.
    let file = File::open(file_path)?;
    let reader = BufReader::new(file);

    // Parse the JSON file into a serde_json::Value.
    let json_value: Value = serde_json::from_reader(reader)?;

    Ok(json_value)
}
