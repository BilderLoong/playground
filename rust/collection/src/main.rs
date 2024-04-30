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
}

struct Solution;

impl Solution {
    pub fn find_max_consecutive_ones(nums: Vec<i32>) -> i32 {
        nums.split(|&x| x == 1).map(|x| x.len()).max().unwrap() as i32
    }
}
