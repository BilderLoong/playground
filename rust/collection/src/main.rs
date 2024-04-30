use core::panic;
use std::collections::HashMap;
use std::fs::File;
use std::io::ErrorKind;

fn main() {
    let s = String::from("Hello, world!");
    let s1 = &s[1..2];
    // let s2 = s[..];
    let s = "Hello, world!";
    let s = s.to_string();

    let mut s = s + " bar";
    s.push_str(" foo");

    let a = "daada";
    s.push_str(a);

    println!("{}", s);

    let v = vec![1, 1, 2, 3];

    let third = &v[2];
    let third_1 = v[2];

    let mut v = vec![1, 1, 2, 3];
    let first = &v[0];

    let s1 = String::from("Hello, ");
    let s2 = String::from("world!");
    let s3 = s1 + &s2;
    // s2 = "bar".to_string();
    // println!("s3: {}\ns2: {}", s3, s2);

    // v.push(4);

    println!("The first element is: {}", first);

    // for i in &v {
    //     println!("{}", i);
    // }
    for i in &v {
        println!("{}", i);
    }

    let mut v0 = vec![1, 2, 3, 4, 5];
    let v = &mut v0;

    let mut iter = v.into_iter();
    while let Some(i) = iter.next() {
        println!("{}", i);
    }

    let v0 = vec![1, 2, 3, 4, 5];
    let v2 = &v0;
    // let v3 = v0;

    println!("{:?}", v2);

    // for i in v0 {
    // }
    //
    // for i in v2 {
    // }

    // println!("{:?}", v);
    let nums = vec![0, 1, 0];
    nums.split(|&x| x == 1);

    let hello = "Здравствуйте";

    let s = &hello[0..4];
    println!("{}", s);

    let s = &hello[0..2];
    println!("{}", s);

    let s = "नमस्ते";
    s.chars().for_each(|x| println!("{}", x));
    s.bytes().for_each(|x| println!("{}", x));

    let mut scores = HashMap::new();
    scores.insert("Blue".to_string(), 10);
    scores.insert("Yellow".to_string(), 50);

    let teams = vec![String::from("Blue"), String::from("Yellow")];
    let initial_scores = vec![10, 50];

    let mut scores: HashMap<_, _> = teams.iter().zip(initial_scores.iter()).collect();

    let field_name = String::from("Favorite color");
    let field_value = String::from("Blue");
    let mut color = HashMap::new();
    color.insert(field_name, field_value);

    let mut scores = HashMap::new();
    scores.insert(String::from("Blue"), 10);
    scores.insert(String::from("Yellow"), 50);
    let team_name = String::from("Blue");
    let score = scores.get(&team_name);

    let mut scores = HashMap::new();
    scores.insert(String::from("Blue"), 10);

    scores.entry(String::from("Yellow")).or_insert(50);
    scores.entry(String::from("Blue")).or_insert(50);
    println!("{:?}", scores);

    let f = match File::open("hello.txt") {
        Err(ref error) if error.kind() == ErrorKind::NotFound => match File::create("hello.txt") {
            Err(error) => {
                panic!("There was a problem creating the file: {:?}", error);
            }
            Ok(file) => file,
        },

        Err(error) => {
            panic!("There was a problem opening the file: {:?}", error);
        }
        Ok(file) => file,
    };
}

struct Solution;

impl Solution {}
