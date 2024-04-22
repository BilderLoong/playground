use std::fmt;

fn main() {
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
}

fn area(rect: &Rectangle) -> u32 {
    rect.width * rect.height
}

#[derive(Debug)]
struct Rectangle {
    width: u32,
    height: u32,
}

impl std::fmt::Display for Rectangle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}, {})", self.width, self.height)
    }
}
