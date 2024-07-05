use serde::{Deserialize, Serialize};
use serde_json::{Result, Value};
use std::collections::HashMap;
use std::fs::File;
use std::io::BufReader;

fn main() {
    match read_json_file("./M-yomitan-settings-2024-07-05-02-30-40.bak.json") {
        Ok(json) => process_json(&json),
        Err(e) => eprintln!("Failed to read JSON: {:?}", e),
    }
}

fn process_json(config: &YomitanConfig) {
    println!("{:?}", config.date);
}

fn read_json_file(file_path: &str) -> Result<YomitanConfig> {
    let file = File::open(file_path).map_err(serde_json::Error::io)?;
    let reader = BufReader::new(file);
    let config: YomitanConfig = serde_json::from_reader(reader)?;
    Ok(config)
}

struct YomitanConfig {
    options: Options,
}

struct Options {
    profiles: Vec<Profile>,
}

struct Profile {
    name: String,

}
