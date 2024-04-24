use std::fmt;

fn main() {
    let some_u8_value = Some(0u8);

    match some_u8_value {
        Some(3) => println!("three"),
        _ => (),
    }

    if let Some(3) = Some(3) {
        println!("three!");
    }

    let m = Message::Write(String::from("sdf"));
    let m = Message::Move { x: 123, y: 234 };
    m.call();

    let res = area(&Rectangle {
        width: 30,
        height: 50,
    });

    println!("{}", res);

    let rect1 = Rectangle {
        width: 30,
        height: 50,
    };

    // rect1は{}です
    println!("rect1 is {}", rect1);
    println!("rect1 is {:#?}", rect1);
    println!("rect1 is {:?}", rect1);

    let v = vec![1, 2, 3];

    let a = v[2];
    println!("a: {}", a);

    mut_loop();

    let mut b = &v[2];
    b = &4;
    println!("v: {:?}", v);

    let mut v1 = vec![1, 2, 3];
    let b = &mut v1[2]; // Equal to  `let b = &mut (v1[2]);`
    *b = 4;
    println!("v1: {:?}", v1);

    let mut v1 = vec![1, 2, 3];
    let b = &mut (v1[2]);
    *b = 1;
    println!("v1: {:?}", v1);

    let mut v1 = vec![1, 2, 3];
    let b = (&mut v1)[2];
    println!("b: {:?}", b);

    let a = Rectangle {
        width: 30,
        height: 50,
    };

    let rect_area = a.area1();
    // let a = a; // `a` is moved here.

    let home = IpAddr {
        kind: IpAddrKind::V4,
        address: String::from("127.0.0.1"),
    };

    let loopback = IpAddr {
        kind: IpAddrKind::V6,
        address: String::from("::1"),
    };

    let home = IpAddr1::V4(String::from("127.0.0.1"));
    let loopback = IpAddr1::V6(String::from("::1"));

    let home = IpAddr2::V4(123, 3, 3, 3);
    let loopback = IpAddr2::V6(String::from("::1"));
}

fn area(rect: &Rectangle) -> u32 {
    rect.width * rect.height
}

fn mut_loop() {
    let mut v = vec![100, 32, 57];
    for i in &mut v {
        *i += 50;
    }

    println!("{:?}", v);
}

#[derive(Debug)]
struct Rectangle {
    width: u32,
    height: u32,
}

impl Rectangle {
    fn square(size: u32) -> Rectangle {
        Rectangle {
            width: size,
            height: size,
        }
    }

    fn area(&self) -> u32 {
        self.width * self.height
    }

    fn area1(self) -> u32 {
        self.width * self.height
    }

    fn can_hold(&self, other: &Rectangle) -> bool {
        self.width > other.width && self.height > other.height
    }
}

impl std::fmt::Display for Rectangle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}, {})", self.width, self.height)
    }
}

enum IpAddr1 {
    V4(String),
    V6(String),
}

enum IpAddr2 {
    V4(u8, u8, u8, u8),
    V6(String),
}
enum IpAddrKind {
    V4,
    V6,
}

struct IpAddr {
    kind: IpAddrKind,
    address: String,
}

enum Message {
    Quit,
    Move { x: i32, y: i32 },
    Write(String),
    ChangeColor(i32, i32, i32),
}

impl Message {
    fn call(&self) {}
}
