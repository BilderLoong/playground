use std::collections::HashMap;
use std::fs::File;

use std::io;
use std::io::Read;

fn read_username_from_file() -> Result<String, io::Error> {
    let mut f = File::open("hello.txt")?;
    let mut s = String::new();
    f.read_to_string(&mut s)?;
    Ok(s)
}
fn main() {
    let res = read_username_from_file().unwrap_or("123".to_string());
    print!("{}", res);
    let x = 1;
    ref_fn(&x);
    normal_fn(x);
}
fn normal_fn(x: i32) {
    println!("{}", x);
}

fn ref_fn_1(x: &i32) {
    println!("{}", x);
}

fn ref_fn(&x: &i32) {
    println!("{}", x);
}

#[derive(Debug)]
struct Solution {}

impl Solution {
    pub fn has_trailing_zeros(nums: Vec<i32>) -> bool {
        nums.iter().filter(|&x| x % 2 == 0).count() > 1
    }
}
