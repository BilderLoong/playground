use minigrep::Config;
use std::env;
use std::process;

fn main() {
    // println!("{}", Solution::third_max(vec![1, 2, 2, 5, 3, 5]));
    let binding = env::args().collect::<Vec<String>>();
    let slice: &[String] = binding.as_slice();
    // let test: &[String] = slice;
    let slice: &Vec<String> = &binding;
    // let test: &[String] = slice;

    let config = Config::new(slice).unwrap_or_else(|err| {
        eprintln!("Problem parsing arguments: {}", err);
        process::exit(1);
    });

    if let Err(e) = minigrep::run(config) {
        eprintln!("Application error: {}", e);
        process::exit(1);
    };
}
