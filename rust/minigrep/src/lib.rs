use std::env;
use std::error::Error;
use std::fs::File;
use std::io::prelude::*;

pub fn run(config: Config) -> Result<(), Box<dyn Error>> {
    let mut file = File::open(config.file_name)?;

    let contents = &mut String::new();
    file.read_to_string(contents)?;
    let query = &config.query;

    let res = if config.case_insensitive {
        search_case_insensitive(query, contents)
    } else {
        search(query, contents)
    };

    println!("{:?}", res);

    // println!("Content: {}", contents);

    Ok(())
}

pub fn search_case_insensitive<'a>(query: &str, contents: &'a str) -> Vec<&'a str> {
    contents
        .lines()
        .filter(|&x| x.to_lowercase().contains(&query.to_lowercase()))
        .collect()
}
pub fn search<'a>(query: &str, contents: &'a str) -> Vec<&'a str> {
    contents.lines().filter(|&x| x.contains(query)).collect()
}

pub struct Config {
    query: String,
    file_name: String,
    case_insensitive: bool,
}

impl Config {
    pub fn new(args: &[String]) -> Result<Config, &str> {
        match args {
            [_, file_name, query, ..] => Ok(Config {
                query: query.to_string(),
                file_name: file_name.to_string(),
                case_insensitive: env::var("CASE_INSENSITIVE").is_err(),
            }),

            _ => Err("Not enough arguments"),
        }
    }
}

struct Solution;
use std::collections::HashSet;
// [1,2,2,5,3,5]
// [1,1,2]
impl Solution {
    pub fn third_max(nums: Vec<i32>) -> i32 {
        if nums.len() < 3 {
            return *nums.iter().max().unwrap();
        };

        let mut nums = nums.clone();

        nums.sort();
        *nums
            .iter()
            .rev()
            .fold(HashSet::new(), |mut acc, e| {
                if acc.len() >= 3 {
                    return acc;
                }

                if acc.contains(e) {
                    return acc;
                }

                acc.insert(*e);
                acc
            })
            .iter()
            .min()
            .unwrap()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn one_result() {
        let query = "duct";

        let contents = "\
Rust:
safe, fast, productive.
Pick three.
Dutc tape.";

        assert_eq!(vec!["safe, fast, productive."], search(query, contents));
    }

    #[test]
    fn case_insensitive() {
        let query = "rUsT";
        let contents = "\
Rust:
safe, fast, productive.
Pick three.
Trust me.";

        assert_eq!(
            vec!["Rust:", "Trust me."],
            search_case_insensitive(query, contents)
        );
    }
}
